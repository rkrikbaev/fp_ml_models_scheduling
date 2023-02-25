fun( #{ "task_id":=TaskId, "result":=Result, "task_status":=TaskStatus, "task_updated":=TaskUpdated, "model_path":=ModelPath, "model_uri":=ModelUri } = Input, Output, Context)->
    % fp:log(info,"DEBUG ML: Request in Action IoT biding. TaskId: ~p, Result: ~p, ModelPath: ~p, TaskStatus: ~p", [TaskId, Result, ModelPath, TaskStatus]),
    ml_model_request_processing:response(#{
                            "result"=>Result,
                            "task_status"=>TaskStatus,
                            "task_updated"=>TaskUpdated,
                            "task_id"=>TaskId,
                            "model_path"=>ModelPath,
                            "model_uri"=>ModelUri
                        }),
    ok
end.