% 
% Sinusoid signal generator
% 
% 
%%-----------------------------------------------------------------
%% Copyright (c) 2020, Faceplate. All Rights Reserved.
%% Author: Vozzhenikov Roman, vzroman@gmail.com
%%-----------------------------------------------------------------
-module(ai_sin_flat).

%% ====================================================================
%% API functions
%% ====================================================================
-export([on_event/2]).
-include("fp_struct.hrl").


on_event(_Event, State)->
  TS = fp_lib:get_datetime() div 1000,
  Tags = fp_db:query(<<"get .oid from root where and(.folder=$oid('/root/PROJECT/TAGS/Nodes/Agadyr'), .name='ntag_p_load_sc_sim')">>),
  [begin
    Tag = fp_db:open(T, none),
    #{ <<"valueH">> := H, <<"valueL">> := L } = fp_db:read_fields( Tag, #{ <<"valueH">> => 100.0, <<"valueL">> => 0.0 } ),
    Value = new_value(TS,H,L),
    fp_db:edit_object( Tag, #{<<"value">> => Value} )
  end || T <- Tags ],
  State.

new_value(Time,H,L)->
    Amp1 = ( H - L ),
    T1=(Time/3600)*math:pi(),
    Sin1=math:sin(T1)+1,
    Sin1V=((L+(H-L)*(Sin1*0.5*Amp1))/(H-L)),
    % ?LOGINFO("DEBUG: Sin1V ~p",[Sin1V]),
    Sin1V.