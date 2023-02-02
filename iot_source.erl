fun( #{ "model_path":=ModelPath }=Vars, _Input, _From, To)->
    
    fp:log(info,"DEBUG ML: Request in IoT biding for Model: ~p: ", [ModelPath]),
    
    Request = ml_model_request_processing:request(Ts=To, Vars#{ "model_path"=>ModelPath }),
    
    fp:log(info,"DEBUG ML: Request out IoT biding for Model: ~p: ", [Request]),
    
    Request
end.