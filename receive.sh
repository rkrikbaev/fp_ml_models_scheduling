#!/bin/bash

task_id=$1

curl --location \
--request POST 'http://138.68.70.41:8015/predict' \
--header 'Content-Type: application/json' \
--data-raw '{ 
    "task_id": "'$task_id'", 
    "model_point": "almaty4", 
    "model_type": "tf_model", 
    "model_config": { "window":96 }, 
    "model_uri": { 
        "experiment_id": "571625146127493926", 
        "run_id": "6776c0c6dda044bd8f120d2875463883" 
        }, 
    "metadata":null, 
    "period": null, 
    "dataset":null 
    }'