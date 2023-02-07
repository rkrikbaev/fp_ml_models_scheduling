fun( #{ "model_path":=ModelPath }=Vars, _Input, _From, To)->
    
    Request = ml_model_request_processing:request(Ts=To, Vars#{ "model_path"=>ModelPath }),
    
    fp:log(info,"DEBUG ML: IoT Source request : ~p: ", [Request]),
    
    Request
end.