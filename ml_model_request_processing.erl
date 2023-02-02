
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

request(Ts, #{ "model_path":=ModelPath })->
    
     ModelControlTag = <<ModelPath/binary,"/model_control">>,
    
    #{
        <<"task_id">> := TaskId,
        <<"model_uri">> := ModelUri0,
        <<"input_archive">>:=InputArchive0,
        <<"model_config">>:= Config0,
        <<"model_type">>:=ModelType,
        <<"model_point">>:=ModelPoint
    } = fp_db:read_fields( fp_db:open(ModelControlTag), [
        <<"task_id">>,
        <<"model_uri">>,
        <<"input_archive">>,
        <<"model_config">>,
        <<"model_type">>,
        <<"model_point">>]),  
    
    Config = 
        try
            fp_lib:from_json(Config0)
            
        catch
            ErrorConfig -> ?LOGINFO("DEBUG ERROR write to archives: ~p ",[ ErrorConfig ])
        end,
        
    fp:log(info,"DEBUG ML: Config: ~p ", [ Config ]),
    
    #{ <<"input_window">>:=InputWindow0, <<"granularity">>:=Granularity0 } = Config,
    
    Req =
        case TaskId of
            none->
                ?LOGINFO("REQ: TaskId: ~p", [TaskId]),
                % Path settings
                InputArchivesPath = <<ModelPath/binary,"/archivesS/">>,
                OutputArchivePath = <<ModelPath/binary,"/futureP/">>,
                ?LOGINFO("REQ: ArchivesPath ~p, FuturePath ~p",[InputArchivesPath, OutputArchivePath]),
                
                InputArchive1 = 
                    case InputArchive0 of
                        InputArchive0 when is_list(InputArchive0)->
                            InputArchive0;
                        _-> 
                           [ <<InputArchive0/binary>> ]
                    end,
                
                ModelUri = fp_lib:from_json(ModelUri0),
                
                ?LOGINFO("REQ: ModelUri value ~p",[ModelUri]),
                ?LOGINFO("REQ: Config value ~p",[Config]),
                ?LOGINFO("REQ: Name of input archive ~p",[InputArchive1]),
                % Check settings

                InputWindow = if is_number(InputWindow0)->( InputWindow0 + 1 ) * ?HOUR; true-> 96 * ?HOUR end,
                Granularity = if is_number(Granularity0)->Granularity0; true-> 1 * ?HOUR end,
                
                % The time point to which read the history 
                To = next_ts( Ts, Granularity * 1000) - Granularity * 1000,  %in ms
                
                InputDataset = 
                    if is_list(InputArchive1)->
                            InputArchive = [ [<<InputArchivesPath/binary,(unicode:characters_to_binary(R))/binary>>,<<"avg">>] || R <- InputArchive1 ],
                            ?LOGINFO("REQ: InputArchive list ~p",[InputArchive]),
                            ?LOGINFO("REQ: Granularity ~p",[Granularity]),
                            ?LOGINFO("REQ: InputWindow ~p",[InputWindow]),
                            ?LOGINFO("REQ: From (Ts) ~p, round to (To) ~p",[ To, Ts ]),
                            InputData = fp:archives_get_periods(#{
                                <<"step_unit">> => <<"second">>,
                                <<"step_size">> => Granularity,
                                <<"step_count">> => InputWindow div Granularity,
                                <<"end">> => To
                            }, InputArchive),
                            ?LOGINFO("REQ: InputData ~p",[InputData]),
                            fp_db:to_json(term, InputData);
                    true->
                        none
                    end,
                ?LOGINFO("REQ: Size of dataset ~p",[length(InputDataset)]),
                ?LOGINFO("REQ: Config value ~p, ModelUri value ~p",[Config, ModelUri]),
                #{
                    "model_type"=> ModelType,
                    "model_point"=> ModelPoint,
                    "model_path"=> ModelPath, 
                    "task_id" => null,      
                    "model_uri"=> ModelUri,  
                    "model_config"=> Config,
                    "metadata"=> null,             
                    "dataset"=> InputDataset,
                    "period"=>null
                };
            TaskId when is_binary(TaskId) ->
                ?LOGINFO("REQ: TaskId: ~p", [TaskId]),
                #{
                    "model_type"=> null,
                    "model_point"=> null,
                    "model_path"=> ModelPath, 
                    "task_id" => TaskId,      
                    "model_uri"=>  null,  
                    "model_config" => null,
                    "metadata"=> null,              
                    "dataset"=> null,
                    "period"=>null
                }
        end,
    Req.

response(#{
    "result":= Result,
    "task_created":= TaskCreated,
    "task_status":= TaskStatus,
    "task_id":= TaskId,
    "model_path":= ModelPath
})->

    ModelControlTagPath = <<ModelPath/binary,"/model_control">>,
    ?LOGINFO("REQ: Model Control Tag Path: ~p", [ModelControlTagPath]),
    
    #{
        <<"output_archive">>:=OutputArchive0
    } = fp_db:read_fields( fp_db:open(ModelControlTagPath),[
        <<"output_archive">>
    ] ),

    OutputArchive = 
        <<ModelPath/binary, "/futureP/", (unicode:characters_to_binary(hd([OutputArchive0])))/binary>>,

    {ok, Triggered} = 
        case TaskStatus of <<"SUCCESS">> ->
                ?LOGINFO("DEBUG: Task complited: ~p ",[ TaskStatus ]),
                case Result of 
                    Result when is_list(Result)->
                        Predict = [ {floor(TS) *1000, V}||[TS, V]<-Result ],
                        try
                            ResIns = fp_archive:insert_values( OutputArchive, Predict ),
                            fp:log(info,"DEBUG ML: fp_archive:insert_values: ~p ~p", [ Predict, ResIns ])
                        catch
                            Error1 -> ?LOGINFO("DEBUG ERROR write to archives: ~p ",[ Error1 ])
                        end;
                    _-> 
                        ok
                end,                
                {ok, true};
            _->
                {ok, false}
        end,
    
    try
        fp:set_value(ModelControlTagPath, "task_id", TaskId),
        fp:set_value(ModelControlTagPath, "task_status", TaskStatus),
        fp:set_value(ModelControlTagPath, "task_created",TaskCreated),
        fp:set_value(ModelControlTagPath, "triggered",Triggered)
    catch
        Error0 -> ?LOGINFO("DEBUG wacs_ml Result :  ERROR ~p ",[Error0])
    end,

    ok.