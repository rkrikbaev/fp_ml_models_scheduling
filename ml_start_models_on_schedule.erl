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
    
    ?LOGINFO("DEBUG: curr_hour ~p, Start_hour ~p",[Hour, StartHour]),

    if 
        Hour >= StartHour->
            [begin
                ModelControlTag = fp_db:to_path(fp_db:open(TagID,none)),
                [ModelPath,_,_] = string:replace(ModelControlTag, "/model_control", "", all),
                ?LOGINFO("DEBUG: ModelPath ~p",[ModelPath]),
                trigger_tag(GlobalControlTag, ModelControlTag),
                timer:sleep(5000)
            end
            ||TagID <- TagControlList];
        Hour < StartHour ->
            ok = reset_trigger_state();
        true-> ok
    end,
    ok.
    
trigger_tag( GlobalModelControlTag, LocalModelControlTag )->

    #{
        <<"task_id">> := TaskId,
        <<"model_uri">> := ModelUri,
        <<"model_path">> := ModelPath,
        <<"task_status">> := TaskState,
        <<"reset">> := Reset,
        <<"input_archive">>:=InputArchive,
        <<"output_archive">>:=OutputArchive
    } = fp_db:read_fields( fp_db:open(LocalModelControlTag), [
                                                            <<"task_id">>,
                                                            <<"model_uri">>,
                                                            <<"model_path">>,
                                                            <<"task_status">>,
                                                            <<"reset">>,
                                                            <<"input_archive">>,
                                                            <<"output_archive">>]),
    
    ?LOGINFO("DEBUG: Trigger tag Id: ~p",[LocalModelControlTag]),
    
    #{
        <<"trigger">> := TriggerValue0
    } = fp_db:read_fields( fp_db:open( GlobalModelControlTag ),[
        <<"trigger">>
    ]),
        
    {Triggered, TriggerValue} = 
        case TaskState of
            <<"SUCCESS">> ->
                ?LOGINFO("DEBUG: Task state: ~p Location ~p",[ TaskState, LocalModelControlTag]),
                {true, TriggerValue0};
            _->
                %% Put information about folder in tag. Then we trigger source, and script extracts values
                TriggerValue1 = 
                    case TriggerValue0 of
                        _Value when is_integer(_Value)->_Value + 1;
                        _-> 
                            1
                    end,
                {false, TriggerValue1}
        end,
        
    ?LOGINFO("DEBUG: Task: ~p trigger: ~p",[ LocalModelControlTag, TriggerValue0 ]),
    ?LOGINFO("DEBUG: Task: ~p state ~p",[ LocalModelControlTag, TaskState]),

    fp_db:edit_object(fp_db:open(GlobalModelControlTag),#{
                                                            <<"task_id">>=>TaskId,
                                                            <<"model_uri">>=>ModelUri,
                                                            <<"model_path">>=>ModelPath,
                                                            <<"trigger">>=>TriggerValue,
                                                            <<"task_status">>=>TaskState,
                                                            <<"input_archive">>=>InputArchive,
                                                            <<"output_archive">>=>OutputArchive                                                            
                                                        }),
    
    fp_db:edit_object(fp_db:open(LocalModelControlTag), #{
                                            <<"triggered">>=>Triggered
                                    }),
    if Reset ->
        fp_db:edit_object(fp_db:open(LocalModelControlTag), #{
                                            <<"triggered">>=>false,
                                            <<"task_id">>=>none,
                                            <<"task_status">>=><<"QUEUED">>,
                                            <<"reset">>=>false
                                    });
    true-> ok
    
    end,
    ok.
    
% , disabled=false, without - isTriggered=false , triggered=false, because we need tags
find_tags()->
%    ResQuery=fp_db:query(<<"get .oid from root where or(and( .folder=$oid('/root/PROJECT/TAGS/Nodes'), .pattern=$oid('/root/.patterns/model_control'), disabled=false, triggered=false ), and(disabled=false, reset=true))">>),

    ResQuery=fp_db:query( <<"get .oid  from root where  and( .pattern=$oid('/root/.patterns/model_control'), or( and(disabled=false, triggered=false), and(reset=true, disabled=false)))">>),
    ?LOGINFO("DEBUG: Find available tags ~p ",[ResQuery ]),
    %for only tag - where disable is false and not triggered yet.
    ResQuery.
    
reset_trigger_state() ->
    ResResetQuery=fp_db:query(<<"get .oid from root where and( .pattern=$oid('/root/.patterns/model_control'), disabled=false )">>),
    ?LOGINFO("DEBUG: Reset state trigger ~p ",[ResResetQuery ]),
    
    [ begin 
        ModelControlTag = fp_db:open(ModelControlTagId,none),
        ?LOGINFO("DEBUG: wacs_trigger_MODELLING: Reset state trigger  ~ts ",[ ?PATH(ModelControlTagId) ]),
        
        fp_db:edit_object( ModelControlTag, #{ <<"task_id">> => none } ),
        fp_db:edit_object( ModelControlTag, #{ <<"task_status">> => <<"STDBY">> } ),
        fp_db:edit_object( ModelControlTag, #{ <<"triggered">> => false } ),
        fp_db:edit_object( ModelControlTag, #{ <<"reset">> => false } )
      end || ModelControlTagId <- ResResetQuery
    ],
    ok.