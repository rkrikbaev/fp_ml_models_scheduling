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

-module(model_config).

-include("fp_struct.hrl").

-export([on_event/2]).

on_event({on_cycle,_Cycle},_State)->

    {_, TagList} = find_tags(<<"model_control">>),

    % setup control tags
    StartHour = 3,
    PeriodArchiveNames = <<"">>,
    DataArchiveName = <<"p_load_sc">>,
    ResultArchiveName = <<"predict_p_load_sc">>,
    
    % setup setting tags
    ModelConfig = <<"{ 
    \"growth\":\"linear\", 
    \"seasonality_mode\": \"multiplicative\", 
    \"changepoint_prior_scale\":31, 
    \"seasonality_prior_scale\":35,
    \"interval_width\":0.98,
    \"daily_seasonality\":false,
    \"weekly_seasonality\":false,
    \"yearly_seasonality\":false,
    \"seasonality\":[{ \"name\":\"hour\", \"period\":0.1, \"fourier_order\":5 }]}">>,
    
    ModelType = <<"prophet_model">>,
    ModelFeatures = <<"yhat_upper, yhat_lower">>,
    Granularity = 60,
    DataLen = 60*60*24,
    PeriodLen = 3600,
    
    [begin
        
        [Tag0 | _] = Tag,
        ModelConfigTag = fp_db:to_path(fp_db:open(Tag0,none)),
        [ModelPath,_,_] = string:replace(ModelConfigTag, "/model_control", "", all),
        ModelSettingTag = <<ModelPath/binary,"/model_settings">>,
        
        fp_db:edit_object(fp_db:open(ModelConfigTag),#{
                                                        <<"model_path">>=>ModelPath,
                                                        <<"periods_archive_name">>=>PeriodArchiveNames,
                                                        <<"data_archive_name">>=>DataArchiveName,
                                                        <<"result_archive_name">>=>ResultArchiveName,
                                                        <<"start_hour">>=>StartHour
                                                    }),
        
        fp_db:edit_object(fp_db:open(ModelSettingTag),#{
                                                        <<"model_config">> => ModelConfig,
                                                        <<"model_type">>=>ModelType,
                                                        <<"model_features">>=>ModelFeatures,
                                                        <<"data_length">>=>DataLen,
                                                        <<"period_length">>=>PeriodLen,
                                                        <<"granularity">>=>Granularity
                                                    })
    end || Tag <- TagList],

    ok.

% find all tags   
find_tags(Pattern)->
    ResQuery=fp_db:query(<<"get .oid, .name from root where and( .pattern=$oid('/root/.patterns/", Pattern/binary, "'), disabled=false)">>),
    %for only tag - where disable is false and not triggered yet.
    ResQuery.