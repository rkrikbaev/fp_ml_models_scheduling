% Скрипт предназначен для следующих задач:
%   1. Поиск и запуск моделей по условиям ( паттерн поиска: [.pattern == model_control], модель не заблокирована [поле "disable" == folse], ) с наступление контрактного часа (поле "start_hour")
%   2. Сброс поля "triggered" после наступления полуночи 00:00 с установкой флага WAITING
%   3. Сброс поля "triggered" после активации поля "reset"==true с установкой флага QUEUED
%   4. Оценка окружения модели: 
%               а. наличие конфигурации в поле "model_config" с минимальным набором полей
%               {"input_archive":<archive name>, "output_archive":<archive name>, "granularity":<step in seconds>},
%               б. наличие типа модели [tf_model, prophet_model]
%      Если окружение модели будет оцененно как некачественное то модель не будет вызвана
%   5. 

-module(ml_start_models_on_schedule).

-include("fp_struct.hrl").
-export([on_event/2]).

on_event(_Event, _State0)->

    GlobalControlTag = <<"/root/PROJECT/TAGS/global_model_control">>,
    {_Date, {Hour, _, _}} = calendar:local_time(),
    
    { ok, StartHour } =
        case fp:get_value(GlobalControlTag,"start_hour") of
            {ok, Start_hour0 } when is_integer(Start_hour0)->
                { ok, Start_hour0 };
            _-> 
                { ok, Hour }
        end,
    
    % select tags if ... disabled=false, without - isTriggered=false , triggered=false, because we need tags
    TagControlList = find_tags(),
    
    if 
        Hour >= StartHour->
            [begin
                ModelControlTag = fp_db:to_path(fp_db:open(TagOID,none)),
                try
                    trigger_tag(GlobalControlTag, ModelControlTag)
                catch 
                    _M1:_Err1:_Stack1 -> ?LOGWARNING("DEBUG ERRORS: ModelControlTag ~p, _M1 ~p, _Err1 ~p, _Stack1 ~p ",[ModelControlTag, _M1, _Err1, _Stack1])
                end,
                timer:sleep(2000)
            end
            || TagOID <- TagControlList];
        Hour < StartHour ->
            ok = reset_trigger_state();
        true-> ok
    end,
    ok.
    
trigger_tag( GlobalModelControlTag, ModelControlTag )->
    ?LOGINFO("INFO: Start Trigger Tag: ~p ",[ModelControlTag]),
    #{
        <<"model_config">> := ModelConfig,
        <<"model_type">> := ModelType,
        <<"model_run_id">>:=ModelRunId,
        <<"task_status">> := TaskState,
        <<"reset">> := Reset,
        <<"input_archive">>:=InputArchive,
        <<"output_archive">>:=OutputArchive,
        <<"isDebug">> := IsDebug %%debug mode is true
    } = fp_db:read_fields( fp_db:open(ModelControlTag ), [ 
                                                            <<"model_config">>,
                                                            <<"model_type">>,
                                                            <<"model_run_id">>,
                                                            <<"task_status">>,
                                                            <<"reset">>,
                                                            <<"input_archive">>,
                                                            <<"output_archive">>,
                                                            <<"isDebug">>]),
    
    #{
        <<"trigger">> := TriggerValue0
    } = fp_db:read_fields( fp_db:open( GlobalModelControlTag ),[
        <<"trigger">>
    ]),
    
    [<<>>,<<"root">>,<<"PROJECT">>,<<"TAGS">>|ModelPoint0] = binary:split(ModelControlTag,<<"/">>,[global]),
    
    ModelPoint = string:lowercase(fp_lib:join_binary(ModelPoint0, <<"_">>)),
    
    % if ModelType == <<"tf_model">>->
    %     ModelUri = fp_lib:to_json(#{<<"experiment_id">>=>ModelPoint});
    % true->
    %     ok
    % end,  
    
    % update model point and tag values
    fp_db:edit_object(fp_db:open(ModelControlTag), #{<<"model_tag">>=>ModelControlTag, <<"model_point">>=>ModelPoint}),    
    fp_db:edit_object(fp_db:open(GlobalModelControlTag), #{<<"model_tag">>=>ModelControlTag, <<"isDebug">>=>IsDebug}),    

    % send request if only all nesseccary fields are filled
    try
        if ModelType == <<"tf_model">>->
            if not is_binary(ModelRunId)->
                throw('Model Run Id not set');
            true->
                ok
            end;
        true->
            ok
        end,
            
        RequiredFields = [ InputArchive, OutputArchive, ModelPoint, ModelConfig, ModelType ],
        
        case [ I || I<-RequiredFields, not is_binary(I)] of
            []-> 
                ok;
            [_]-> 
                ?LOGINFO("WARN: Invalid value in tag: ~p ",[ModelControlTag]),
                throw('Invalid value in required fields')
        end,
        
        % check if model_config has minimul set of data
        C = fp_lib:from_json(ModelConfig),
        #{<<"input_window">>:=InputWindow, <<"output_window">>:=OutputWindow, <<"granularity">>:=Granularity} = C,
        
        % check if value set
        if 
            not is_number(InputWindow)->
                throw('Invalid value in model configuration [InputWindow]');
            not is_number(OutputWindow)->
                throw('Invalid value in model configuration [OutputWindow]');
            not is_number(Granularity)->
                throw('Invalid value in model configuration [Granularity]');
            true-> ok
        end,
        
        case TaskState of
            <<"SUCCESS">> ->
                fp_db:edit_object(fp_db:open(ModelControlTag), #{<<"triggered">>=>true});
            _->
                %% Put information about folder in tag. Then we trigger source, and script extracts values
                TriggerValue = 
                    case TriggerValue0 of _Value 
                        when is_integer(_Value)->_Value + 1;
                        _-> 
                            1
                    end,
                
                fp_db:edit_object(fp_db:open(GlobalModelControlTag),#{<<"trigger">>=>TriggerValue})
        end
            
    catch
        _m1: ErrorConfig:_stack -> 
            ?LOGWARNING("DEBUG ERROR From JSON: m1 ~p, Errors ~p, Stack ~p ",[ _m1, ErrorConfig, _stack ])
    end,
    
    if Reset ->
        fp_db:edit_object(fp_db:open(ModelControlTag), #{
                                            <<"triggered">>=>false,
                                            <<"task_id">>=>none,
                                            <<"task_status">>=><<"QUEUED">>,
                                            <<"reset">>=>false});
        true-> ok
    end,
    
    ok.
    
    
% , disabled=false, without - isTriggered=false , triggered=false, because we need tags
find_tags()->
    
    AvailQuery=fp_db:query(<<"get .oid from root where and(and(
                                                            .pattern=$oid('/root/.patterns/model_control'), 
                                                            disabled=false, 
                                                            or(.name like 'config_' )
                                                            ),
                                                        or(and(triggered=true, reset=true), triggered=false)
                                                        )
                                                    ">>),
                                                 % .name='model_control.q_load',
                                                                % .name='model_control.p_section',
                                                                % .name='p_load',
                                                                % .name='q_load',
                                                                % .name='p_section'   
    ?LOGINFO("DEBUG: Find all available tags ~p ",[AvailQuery ]),
    
    AvailQuery.
    
    
reset_trigger_state() ->
    ResetQuery=fp_db:query(<<"get .oid from root where and( .pattern=$oid('/root/.patterns/model_control'), disabled=false, triggered=true)">>),

    % ?LOGINFO("DEBUG: Reset trigger of:  ~p ",[ResetQuery ]),
    
    [ begin 
        ModelControlTag = fp_db:open(ModelControlTagId,none),
        fp_db:edit_object( ModelControlTag, #{ <<"task_id">> => none } ),
        fp_db:edit_object( ModelControlTag, #{ <<"task_status">> => <<"STDBY">> } ),
        fp_db:edit_object( ModelControlTag, #{ <<"triggered">> => false } ),
        fp_db:edit_object( ModelControlTag, #{ <<"reset">> => false } )
      end || ModelControlTagId <- ResetQuery
    ],
    
    % ?LOGINFO("DEBUG: Reset END ",[]),
    
    ok.