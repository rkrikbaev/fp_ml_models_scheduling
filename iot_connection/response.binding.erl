% sorce script
% 
fun( Vars, _Input, _From, To)->

    TaskStatus = <<"PENDING">>,
    
    Request = ml_models_processing:request(Ts=To, TaskStatus),
    
    fp:log(info,"source script in MODEL RESPONSE binding: ~p: ", [Request]),
    
    Request
end.

% action script
% 
fun( #{ "task_id":=TaskId, "result":=Result, "task_status":=TaskStatus, "task_updated":=TaskUpdated, "model_tag":=ModelTag, "model_uri":=ModelUri } = Input, Output, Context)->
    
    fp:log(info,"Action script in MODEL RESPONSE biding. TaskId: ~p, Result: ~p, ModelPath: ~p, TaskStatus: ~p", [TaskId, Result, ModelTag, TaskStatus]),
    
    ml_models_processing:response(#{
                            "result"=>Result,
                            "task_status"=>TaskStatus,
                            "task_updated"=>TaskUpdated,
                            "task_id"=>TaskId,
                            "model_tag"=>ModelTag,
                            "model_uri"=>ModelUri
                        }),
    ok
    
end.