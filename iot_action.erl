fun( #{ "task_id":=TaskId, "prediction":=Result, "task_status":=TaskStatus, "task_created":=TaskCreated, "model_path":=ModelPath } = Input, Output, Context)->
    fp:log(info,"DEBUG ML: Request in Action IoT biding. TaskId: ~p, Result: ~p, ModelPath: ~p, TaskStatus: ~p", [TaskId, Result, ModelPath, TaskStatus]),
    ml_model_request_processing:response(#{
                            "result"=>Result,
                            "task_status"=>TaskStatus,
                            "task_created"=>TaskCreated,
                            "task_id"=>TaskId,
                            "model_path"=>ModelPath
                        }),
    ok
end.