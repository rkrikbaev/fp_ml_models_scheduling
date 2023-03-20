% Скрипт исполняется в контексте обработки REST HTTP POST запроса запуска модели
% Скрипт предназначен для формирование сообщения и только!
% Ключевые параметры определяются и проверяются отдельно!

-module(ml_models_processing).

-include("fp_struct.hrl").

-export([on_event/2]).
-export([
    request/2,
    response/1
]).

-define(HOUR, 3600). %In Second
-define(START_HOUR, 1).
-define(MAX_COUNT, 3).

on_event(_Event, State)->

    {_, {Hour, _, _}} = calendar:local_time(),
    
    if
        is_number(State)->
            if
                State < ?START_HOUR, Hour >= ?START_HOUR->
                    run(<<"STDBY">>);
                State < (?START_HOUR - 1), Hour >= (?START_HOUR - 1)->
                    restart_by_status(<<"SUCCESS">>);
                true-> 
                    ignore
                end;
        true->
            ignore
        end,
    
    restart_by_trigger(<<"QUEUED">>),
    
    ok.

run()->

    TagControlList = find_models(<<"STDBY">>),
    % ?LOGINFO("RUN Context ~p: ", [TagControlList]),
    
    [ begin ModelTag = fp_db:to_path(fp_db:open(TagOID,none)),
            ?LOGINFO("RUN Model: ~p: ", [ModelTag]),
            try
                check_out(ModelTag),
                update_state(ModelTag)
            catch 
                _M1:_Err1:_Stack1 -> 
                    ?LOGWARNING("DEBUG ERRORS: Tag ~p, _M1 ~p, _Err1 ~p, _Stack1 ~p ",[ModelTag, _M1, _Err1, _Stack1])
            end
        end || [ TagOID | _ ] <- TagControlList ],
        
    ok.

restart_by_trigger(TaskStatus)->

    TagList = fp_db:get([?PROJECT_DB],[<<".oid">>],{'AND',[
        {<<".pattern">>,'=',?OID(<<"/root/.patterns/model_control">>)},
        {<<"disabled">>,'=',false},
        {<<".name">>,'LIKE',<<"config_">>},
        {<<"reset">>,'=',true}]}),            

    [ begin
        
        fp_db:edit_object(fp_db:open(Tag,none), #{ <<"task_status">>=>TaskStatus, <<"reset">>=>false, <<"triggered">>=>false, <<"task_id">>=>none, <<"counter">>=>0} )
    
    end || Tag<-TagList ];
        
    ok.

restart_by_status(TaskStatus) ->

    % Query=fp_db:query(<<"get .oid from root where and( .pattern=$oid('/root/.patterns/model_control'), disabled=false, triggered=true)">>),
    TagList = fp_db:get([?PROJECT_DB],[<<".oid">>],{'AND',[
        {<<".pattern">>,'=',?OID(<<"/root/.patterns/model_control">>)},
        {<<"disabled">>,'=',false},
        {<<".name">>,'LIKE',<<"config_">>},
        {<<"task_status">>,'=',TaskStatus}]}),
        
    [ begin 
        fp_db:edit_object(fp_db:open(Tag,none), #{<<"task_id">>=> none,<<"task_status">>=> <<"STDBY">>,<<"triggered">>=> false,<<"reset">>=> false, <<"counter">>=>0})
    end || Tag <- TagList ],
    
    ok.

update_state(ModelTag)->
    
    [ <<>>,<<"root">>,<<"PROJECT">>,<<"TAGS">> | ModelPoint0 ] = binary:split(ModelTag,<<"/">>,[global]),
    
    ModelPoint = string:lowercase(fp_lib:join_binary(ModelPoint0, <<"_">>)),
        
    fp_db:edit_object(fp_db:open(ModelTag), #{<<"model_tag">>=>ModelTag, 
                                            <<"model_point">>=>ModelPoint,
                                            <<"task_status">>=><<"QUEUED">>
    }),
    ?LOGINFO("update_state ~p: ", [ModelTag]),                                        
    ok.

check_out(ModelTag)->
        
    #{<<"model_config">>:=ModelConfig, <<"model_type">>:=ModelType, <<"model_run_id">>:=ModelRunId, <<"input_archive">>:=InputArchive,<<"output_archive">>:=OutputArchive, <<"model_point">>:=ModelPoint} = 
    fp_db:read_fields( fp_db:open( ModelTag ), [ <<"model_config">>,
                                                 <<"model_type">>,
                                                 <<"model_run_id">>,
                                                 <<"input_archive">>,
                                                 <<"output_archive">>,
                                                 <<"model_point">>]),
                                                           
    RequiredFields = [ InputArchive, OutputArchive, ModelPoint, ModelConfig, ModelType ],
    
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
        
        case [ R || R<-RequiredFields, not is_binary(R)] of
            []-> 
                ok;
            [_]-> 
                throw('Field is empty')
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
        end
    
    catch
        _m1: ErrorConfig: _stack -> 
            ?LOGWARNING("DEBUG ERROR From JSON: m1 ~p, Errors ~p, Stack ~p ",[ _m1, ErrorConfig, _stack ])
    end,
    
    ok.
    
% , disabled=false, without - isTriggered=false , triggered=false, because we need tags
find_models(TaskStatus)->
    % ?LOGINFO("RUN: <<get .oid from root where and(and(.pattern=$oid('/root/.patterns/model_control'), disabled=false, .name like 'config_', task_status=~p), or(and(triggered=true, reset=~p), triggered=true))", [_TaskStatus, _Reset]),
    { [<<".oid">>,<<"task_updated">>], Query } = fp_db:get([?PROJECT_DB],[<<".oid">>,<<"task_updated">>], {'AND',[
            {<<".pattern">>,'=',?OID(<<"/root/.patterns/model_control">>)},
            {<<"disabled">>,'=',false},
            {<<".name">>,'LIKE',<<"config_">>},
            {<<"task_status">>,'=',TaskStatus}]}, 
            #{ order => [ {<<"task_updated">>, 'ASC'} ] }),

    % Query = fp_db:query(<<"get .oid from root where and(and(.pattern=$oid('/root/.patterns/model_control'), disabled=false, .name like 'config_', task_status=",_TaskStatus/binary,"), or(and(triggered=true, reset='",_Reset,"'), triggered=true))">>),
    Query.

next_ts(TS, Cycle) ->
    (TS div Cycle) * Cycle + Cycle.


request(Ts, TaskStatus)->
    
    case find_models(TaskStatus) of 
        [ [ModelTagOID,_] | _ ] ->
        
            ?LOGINFO("process model with task status: ~p ", [TaskStatus]),

            #{
                <<"task_id">> := TaskId,
                <<"input_archive">>:=InputArchive0,
                <<"model_config">>:=ModelConfig0,
                <<"model_run_id">>:= ModelRunId,
                <<"model_type">>:=ModelType,
                <<"model_point">>:=ModelPoint,
                <<".folder">>:=ModelPathOID} = fp_db:read_fields( fp_db:open(ModelTagOID), [
                                                                                        <<"task_id">>,
                                                                                        <<"input_archive">>,
                                                                                        <<"model_config">>,
                                                                                        <<"model_run_id">>,
                                                                                        <<"model_type">>,
                                                                                        <<"model_point">>,
                                                                                        <<".folder">>]),
            ModelPath = fp_db:to_path(fp_db:open(ModelPathOID, none)),
            ModelTag = fp_db:to_path(fp_db:open(ModelTagOID, none)),
            
            ModelConfig = fp_lib:from_json(ModelConfig0),
            #{ <<"input_window">>:=InputWindow0, <<"granularity">>:=Granularity0 } = ModelConfig,
            
            case TaskStatus of
                <<"QUEUED">>->
                    InputArchivesPath = <<ModelPath/binary,"/archivesS/">>,
                    InputArchive1 = 
                        case InputArchive0 of
                            InputArchive0 when is_list(InputArchive0)->
                                InputArchive0;
                            _-> 
                               [ <<InputArchive0/binary>> ]
                        end,
    
                    InputWindow = if is_number(InputWindow0)->(InputWindow0 + 1) * ?HOUR; true-> 96 * ?HOUR end,
                    Granularity = if is_number(Granularity0)->Granularity0; true-> 1 * ?HOUR end,
                    
                    % The time point to which read the history 
                    To = next_ts(Ts, Granularity * 1000) - Granularity * 1000,  %in ms
                    
                    InputDataset = 
                        if is_list(InputArchive1)->
                                InputArchive = [ [<<InputArchivesPath/binary,(unicode:characters_to_binary(R))/binary>>,<<"avg">>] || R <- InputArchive1 ],
                                InputData = fp:archives_get_periods(#{
                                    <<"step_unit">> => <<"second">>,
                                    <<"step_size">> => Granularity,
                                    <<"step_count">> => InputWindow div Granularity,
                                    <<"end">> => To
                                }, InputArchive),
                                fp_db:to_json(term, InputData);
                        true->
                            none
                        end,
                    #{
                        "model_type"=> ModelType,
                        "model_point"=> ModelPoint,
                        "model_tag"=> ModelTag, 
                        "task_id" => null,
                        "task_status" => TaskStatus, 
                        "model_config"=> ModelConfig,
                        "model_run_id"=> ModelRunId,  
                        "metadata"=> null,             
                        "dataset"=> InputDataset,
                        "period"=>null
                    };
    
                <<"PENDING">>->
                    #{
                        "model_type"=> ModelType,
                        "model_point"=> ModelPoint,
                        "model_tag"=> ModelTag, 
                        "task_id" => TaskId,
                        "task_status" => TaskStatus,
                        "model_config"=> ModelConfig,
                        "model_run_id"=> ModelRunId,  
                        "metadata"=> null,             
                        "dataset"=> null,
                        "period"=>null
                    };
                _->
                    throw({stop, no_tasks})
            end;
        _->
            throw({stop, no_tasks})
    end.
    

response(#{
    "result":= Result,
    "task_updated":= TaskUpdated,
    "task_status":= TaskStatus,
    "task_id":= TaskId,
    "model_tag":= ModelTag,
    "model_uri":=_ModelUri
    })->
    
    ?LOGINFO("RES: Model ModelTag: ~p TaskStatus ~p ", [ModelTag, TaskStatus]),
    
    #{
        <<"output_archive">>:=OutputArchive0,
        <<"counter">>:=Counter,
        <<".folder">>:=ModelPathOID
    } = fp_db:read_fields( fp_db:open(ModelTag),[
        <<"output_archive">>,
        <<"counter">>,
        <<".folder">>
    ] ),
    
    ModelPath = fp_db:to_path(fp_db:open(ModelPathOID, none)),

    OutputArchive = 
        <<ModelPath/binary, "/futureP/", (unicode:characters_to_binary(hd([OutputArchive0])))/binary>>,
    
    case TaskStatus of 
        
        <<"SUCCESS">> ->
                
            if is_list(Result)->
                
                Predict = [ {floor(TS) * 1000, V } || [TS, V] <-Result ],
                    
                try
                
                    _ResIns = fp_archive:insert_values( OutputArchive, Predict )
                    
                catch
                    _mesArch0: ErrorArch0:_stackArch0 -> ?LOGWARNING("RESP ERROR insert values to archives: _mesArch0 ~p, ErrorArch0 ~p, _stackArch0 ~p ",[ _mesArch0, ErrorArch0,_stackArch0 ])
                end,
                
                fp_db:edit_object(fp_db:open(ModelTag), #{
                                                        <<"triggered">>=>true,
                                                        <<"task_status">>=>TaskStatus,
                                                        <<"task_updated">>=>TaskUpdated});
            true-> 
                ?LOGINFO("Unexpected data format in result")
            end;
            
        <<"PENDING">> ->
                
                { TaskStatus0, Counter0 } = if 
                    not is_number(Counter)-> { TaskStatus, 0 };
                    
                    Counter > ?MAX_COUNT-> { <<"FAILED">>, Counter };
                    Counter =< ?MAX_COUNT -> { TaskStatus, Counter+1 };
                    true-> { TaskStatus, 0 }
                end,
                
                ?LOGINFO("Counter: ~p TaskStatus0: ~p ", [TaskStatus0, Counter]),
                
                fp_db:edit_object(fp_db:open(ModelTag), #{
                                            <<"task_id">>=>TaskId,
                                            <<"task_status">>=>TaskStatus0,
                                            <<"task_updated">>=>TaskUpdated,
                                            <<"counter">>=>Counter0});
        _->
            ok
    end.