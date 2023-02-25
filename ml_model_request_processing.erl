% Скрипт исполняется в контексте обработки REST HTTP POST запроса запуска модели
% Скрипт предназначен для формирование сообщения и только!
% Ключевые параметры определяются и проверяются отдельно!

-module(ml_model_request_processing).

-include("fp_struct.hrl").

-export([on_event/2]).
-export([
    request/2,
    response/1
]).

-define(HOUR, 3600). %In Second

on_event(_vent, State)->
   % DUMMY
   State.

next_ts(TS, Cycle) ->
    (TS div Cycle) * Cycle + Cycle.


request(Ts, #{ "model_tag":=ModelControlTag, "isDebug":=IsDebug })->

    case IsDebug of 
        true-> ?LOGINFO("REQ: Model ModelControlTag: ~p", [ModelControlTag]) ;
        _ -> ok
    end,  
    
    #{
        <<"task_id">> := TaskId,
        <<"input_archive">>:=InputArchive0,
        <<"model_config">>:=ModelConfig0,
        <<"model_run_id">>:= ModelRunId,
        <<"model_type">>:=ModelType,
        <<"model_point">>:=ModelPoint,
        <<".folder">>:=ModelPathOID
    } = fp_db:read_fields( fp_db:open(ModelControlTag), [
        <<"task_id">>,
        <<"input_archive">>,
        <<"model_config">>,
        <<"model_run_id">>,
        <<"model_type">>,
        <<"model_point">>,
        <<".folder">>
        ]),
        
    ModelConfig = fp_lib:from_json(ModelConfig0),
        
    #{ <<"input_window">>:=InputWindow0, <<"granularity">>:=Granularity0 } = ModelConfig,

    ModelPath = fp_db:to_path(fp_db:open(ModelPathOID, none)),
    Req =
        case TaskId of
            none->

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
                    "model_path"=> ModelControlTag, 
                    "task_id" => null,      
                    "model_run_id"=> ModelRunId,  
                    "model_config"=> ModelConfig,
                    "metadata"=> null,             
                    "dataset"=> InputDataset,
                    "period"=>null
                };
                
            TaskId when is_binary(TaskId) ->
                
                #{
                    "model_type"=> ModelType,
                    "model_point"=> ModelPoint,
                    "model_path"=> ModelControlTag, 
                    "task_id" => TaskId,      
                    "model_run_id"=>  ModelRunId,  
                    "model_config" => ModelConfig,
                    "metadata"=> null,              
                    "dataset"=> null,
                    "period"=>null
                }
        end,
    
    case IsDebug of 
        true -> 
            ?LOGINFO("REQ: ModelControlTagPath ~p Req: ~p", [ModelControlTag, Req]),
            ?LOGINFO("REQ: model_path ~p ", [maps:get("model_tag",Req,none)]),
            ?LOGINFO("REQ: task_id ~p ", [maps:get("task_id",Req,none)]),
            ?LOGINFO("REQ: model_type ~p ", [maps:get("model_type",Req,none)]),
            ?LOGINFO("REQ: model_point ~p ", [maps:get("model_point",Req,none)]),
            ?LOGINFO("REQ: model_uri ~p ", [maps:get("model_run_id",Req,none)]),
            ?LOGINFO("REQ: model_config ~p ", [maps:get("model_config",Req,none)]) ;
        _ -> ok
    end,

    Req.
    

response(#{
    "result":= Result,
    "task_updated":= TaskUpdated,
    "task_status":= TaskStatus,
    "task_id":= TaskId,
    "model_path":= ModelControlTag,
    "model_uri":=_ModelUri
})->
    
    ?LOGINFO("RES: Model ModelControlTag: ~p TaskStatus ~p ", [ModelControlTag, TaskStatus]),
    
    #{
        <<"output_archive">>:=OutputArchive0,
        <<"isDebug">> := IsDebug
    } = fp_db:read_fields( fp_db:open(ModelControlTag),[
        <<"output_archive">>,
        <<"isDebug">>
    ] ),
    
    case IsDebug of 
        true-> 
            ?LOGINFO("RES: Result: ~p", [Result]),
            ?LOGINFO("RES: Result: TaskId ~p, TaskUpdated ~p, _ModelUri ~p ", [TaskId, TaskUpdated, _ModelUri])
            ;
        _ -> ok
    end,  
    
    
    [_|Tokens] = binary:split(ModelControlTag,<<"/">>,[global]),
    
    { ModelParentPath0, _ } = lists:split(length(Tokens) - 1, Tokens),
    
    ModelParentPath1 = fp_lib:join_binary(ModelParentPath0, <<"/">>),
    ModelParentPath = <<"/", ModelParentPath1/binary>>,

    OutputArchive = 
        <<ModelParentPath/binary, "/futureP/", (unicode:characters_to_binary(hd([OutputArchive0])))/binary>>,
    
    case IsDebug of 
        true-> ?LOGINFO("RES: OutputArchive: ~p", [OutputArchive]) ;
        _ -> ok
    end, 
    
    {ok, Triggered} = 
        case TaskStatus of 
            <<"SUCCESS">> ->
                case Result of 
                    Result when is_list(Result)->
                        Predict = [ {floor(TS) *1000, V}||[TS, V]<-Result ],
                        try
                            _ResIns = fp_archive:insert_values( OutputArchive, Predict )
                        catch
                            _mesArch0: ErrorArch0:_stackArch0 -> ?LOGWARNING("RESP ERROR insert values to archives: _mesArch0 ~p, ErrorArch0 ~p, _stackArch0 ~p ",[ _mesArch0, ErrorArch0,_stackArch0 ])
                        end;
                    _-> 
                        ok
                end,
                {ok, true};
            _->
                {ok, false}
        end,
    
    case IsDebug of 
        true-> ?LOGINFO("RES: Triggered: ~p", [Triggered]) ;
        _ -> ok
    end, 
        
    Res = fp_db:edit_object(fp_db:open(ModelControlTag), #{
                                                    <<"triggered">>=>Triggered,
                                                    <<"task_id">>=>TaskId,
                                                    <<"task_status">>=>TaskStatus,
                                                    <<"task_updated">>=>TaskUpdated}),   
    
    case IsDebug of 
        true-> ?LOGINFO("RES: Res response script: ~p", [Res]) ;
        _ -> ok
    end,
    
    ok.