%%-----------------------------------------------------------------
%% This script is executed at server side. The programming language
%% is Erlang.
%% The module must export method "on_event/1" or "on_event/2" . 
%% The on_event/2 should be used if your code needs to keep some state between
%% calls.
%%
%% The callback "on_event" is called by the faceplate only in run-time. 
%% As the trigger might be used timer and/or change of the value of some field 
%% of the tag.
%% If the event is triggered by the timer then the first argument of the on_event 
%% callback will be:
%%    {on_cycle,Cycle} - Cycle is period (ms).
%% If the event is triggered by the change of the value of the linked tag field 
%% then the first argument of the on_event will be:
%%    {tag,TagID,Field,Value} - TagID - OID of the tag
%%                              Field - name of the field
%%                              Value - current value of the field. 
%% The returned value is considered being state and passed as the second argument
%% at the next call.
%%-----------------------------------------------------------------

-module(ml_model_config).

-include("fp_struct.hrl").

-export([on_event/2]).

on_event({on_cycle,_Cycle},_State)->

    {_, TagList} = find_tags(<<"model_control">>),

    % setup control tags
    StartHour = 3,
    InputArchive = <<"p_load_sc">>,
    OutputArchive = <<"predict_p_load_sc">>,
    
    ModelType = <<"tf_model">>,
    Granularity = 3600,
    InputWindow = 96,
    OutputWindow = 48,
    ModelConfig = <<"">>,
    % ExperimentID = <<"571625146127493926">>,
    % RunID = <<"6776c0c6dda044bd8f120d2875463883">>,
    % ModelURI = <<"{ \"experiment_id\":", ExperimentID, "\"run_id\":", RunID }">>,
    ModelURI = <<"{ \"experiment_id\":\"571625146127493926\", \"run_id\": \"6776c0c6dda044bd8f120d2875463883\" }">>,
    [begin
        
        [Tag0 | _] = Tag,
        ModelConfigTag = fp_db:to_path(fp_db:open(Tag0,none)),
        [ModelPath,_,_] = string:replace(ModelConfigTag, "/model_control", "", all),
        ModelSettingTag = <<ModelPath/binary,"/model_settings">>,
        
        fp_db:edit_object(fp_db:open(ModelConfigTag),#{
                                                        <<"model_path">>=>ModelPath,
                                                        <<"output_archive">>=>OutputArchive,
                                                        <<"input_archive">>=>InputArchive,
                                                        <<"start_hour">>=>StartHour,
                                                        <<"model_uri">>=>ModelURI
                                                    }),
        
        fp_db:edit_object(fp_db:open(ModelSettingTag),#{
                                                        <<"model_config">>=>ModelConfig,
                                                        <<"model_type">>=>ModelType,
                                                        <<"input_window">>=>InputWindow,
                                                        <<"output_window">>=>OutputWindow,
                                                        <<"granularity">>=>Granularity
                                                    })
    end || Tag <- TagList],

    ok.

% find all tags   
find_tags(Pattern)->
    ResQuery=fp_db:query(<<"get .oid, .name from root where and( .pattern=$oid('/root/.patterns/", Pattern/binary, "'), disabled=false)">>),
    %for only tag - where disable is false and not triggered yet.
    ResQuery.