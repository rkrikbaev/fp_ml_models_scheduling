
-module(ml_model_config).

-include("fp_struct.hrl").

-define(PROTOTYPE_TAG, <<"/root/PROJECT/TAGS/Nodes/Agadyr/model_control">>). %In Second

-export([on_event/2]).

on_event({on_cycle,_Cycle},_State)->

    % read prototype tag
    %  ModelControlTag = <<ModelPath/binary,"/model_control">>,
    
    #{
        <<"input_archive">>:=InputArchive,
        <<"output_archive">>:=OutputArchive,
        <<"model_config">>:= ModelConfig,
        <<"model_type">>:=ModelType,
        <<"start_hour">>:=StartHour
    } = fp_db:read_fields( fp_db:open(?PROTOTYPE_TAG), [
        <<"model_uri">>,
        <<"input_archive">>,
        <<"output_archive">>,
        <<"model_config">>,
        <<"model_type">>,
        <<"start_hour">>]),  

    {_, TagList} = find_tags(),
    ?LOGINFO("REQ: Find Model tags: ~p", [TagList]),
    
    % setup control tags
    % StartHour = 3,
    % InputArchive = <<"p_load_sc">>,
    % OutputArchive = <<"predict_p_load_baseline">>,
    
    % ModelType = <<"tf_model">>,
    % Granularity = 3600,
    % InputWindow = 96,
    % OutputWindow = 48,
    % ModelConfig = <<"">>,
    % ExperimentID = <<"571625146127493926">>,
    % RunID = <<"6776c0c6dda044bd8f120d2875463883">>,
    % ModelURI = <<"{ \"experiment_id\":", ExperimentID, "\"run_id\":", RunID }">>,
    % ModelURI = <<"{ \"experiment_id\":\"571625146127493926\", \"run_id\": \"6776c0c6dda044bd8f120d2875463883\" }">>,
    
    [begin
        
        [Tag0 | _] = Tag,
        ModelConfigTag = fp_db:to_path(fp_db:open(Tag0,none)),
        [ModelPath,_,_] = string:replace(ModelConfigTag, "/model_control", "", all),
        ?LOGINFO("REQ: ModelPath: ~p", [ModelPath]),
        
        ModelPoint0 = string:replace(ModelPath, "/", <<"_">>, all), 
        [ <<>>, <<"_">>, <<"root">>, <<"_">>, <<"PROJECT">>, <<"_">>, <<"TAGS">>, <<"_">> | ModelPoint1 ] = ModelPoint0,
        ModelPoint = fp_lib:join_binary(ModelPoint1,<<>>),
        ?LOGINFO("REQ: ModelPoint: ~p", [ModelPoint]),

        fp_db:edit_object(fp_db:open(ModelConfigTag),#{
                                                        <<"model_path">>=>ModelPath,
                                                        <<"model_point">>=>ModelPoint,
                                                        <<"output_archive">>=>OutputArchive,
                                                        <<"input_archive">>=>InputArchive,
                                                        <<"start_hour">>=>StartHour,
                                                        % <<"model_uri">>=>ModelURI,
                                                        <<"model_config">>=>ModelConfig,
                                                        <<"model_type">>=>ModelType,
                                                        <<"preset">>=>false
                                                    })

    end || Tag <- TagList],

    ok.

% find all tags   
find_tags()->
    ResQuery=fp_db:query(<<"get .oid, .name from root where and( .pattern=$oid('/root/.patterns/model_control'), preset=true)">>),
    %for only tag - where disable is false and not triggered yet.
    ?LOGINFO("REQ: Find Model tags: ~p", [ResQuery]),
    ResQuery.