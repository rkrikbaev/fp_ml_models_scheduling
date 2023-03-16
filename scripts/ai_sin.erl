%%-----------------------------------------------------------------
%% Copyright (c) 2020, Faceplate. All Rights Reserved.
%% Author: Vozzhenikov Roman, vzroman@gmail.com
%%-----------------------------------------------------------------
-module(ai_sin).

%% ====================================================================
%% API functions
%% ====================================================================
-export([on_event/2]).


on_event(_Event, State)->
  TS = fp_lib:get_datetime() div 1000,
  Tags = fp_db:query(<<"get .oid from root where .pattern=$oid('/root/.patterns/AI')">>),
  [begin
    Tag = fp_db:open(T, none),
    #{ <<"valueH">> := H, <<"valueL">> := L } = fp_db:read_fields( Tag, #{ <<"valueH">> => 100.0, <<"valueL">> => 0.0 } ),
    Value = new_value(TS,Tag,H,L),
    fp_db:edit_object( Tag, #{<<"value">> => Value} )
  end || T <- Tags ],
  State.

new_value(Time,Object,H,L)->
    Amp1=(erlang:phash2(Object,1000)/1000)*(H-L),
    Amp2=(erlang:phash2(Object,100)/(100*50))*(H-L),
    T1=(Time/3600)*erlang:phash2(Object,10)*math:pi(),
    T2=(Time/60)*erlang:phash2(Object,10)*math:pi(),
    Sin1=math:sin(T1)+1,
    Sin2=math:sin(T2)+1,
    Sin1V=((L+(H-L)*(Sin1*0.55*Amp1))/(H-L)),
    Sin2V=((L+(H-L)*(Sin2*0.55*Amp2))/(H-L)),
    Sin1V+Sin2V.