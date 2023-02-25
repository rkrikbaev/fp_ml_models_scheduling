fun( #{ "model_tag":=ModelTag, "isDebug":=IsDebug }=Vars, _Input, _From, To)->

    Request = ml_model_request_processing:request(Ts=To, Vars#{ "model_tag"=>ModelTag, "isDebug"=> IsDebug}),
    
    case IsDebug of 
        true-> fp:log(info,"DEBUG ML: Request out IoT biding for Model: ~p: ", [Request]);
        _ -> ok
    end, 
    
    Request
    
end.