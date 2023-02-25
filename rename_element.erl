%%-----------------------------------------------------------------
%% This script is executed at server side. The programming language
%% is Erlang.
%% The module must export method "on_event/1" or "on_event/2" . 
%% The on_event/2 should be used if your code needs to keep some state between
%% calls.
%%
%% The callback "on_event" is called by the faceplate only in run-time. 
%% As the trigger might be used timer and/or change of the value of some field 
%% of the tag.
%% If the event is triggered by the timer then the first argument of the on_event 
%% callback will be:
%%    {on_cycle,Cycle} - Cycle is period (ms).
%% If the event is triggered by the change of the value of the linked tag field 
%% then the first argument of the on_event will be:
%%    {tag,TagID,Field,Value} - TagID - OID of the tag
%%                              Field - name of the field
%%                              Value - current value of the field. 
%% The returned value is considered being state and passed as the second argument
%% at the next call.
%%-----------------------------------------------------------------

-module(rename_elements).

-include("fp_struct.hrl").

-export([on_event/2]).

on_event({on_cycle,Cycle},State)->
   % here should be your code that is executed each Cycle milliseconds
   State;
on_event({tag,TagID,Field,Value},State)->
   % here should be your code that is executed on change of the Field of the Tag
   
   ?LOGINFO("DEBUG RENAME_ELEMENTS: START"),

    {_, OidAndPath} = fp_db:query(" get .oid, .path from root where and( .pattern=$oid('/root/.patterns/model_control'), .name='q_load') ") , 
    [ rename_el(Oid, Path) || [Oid, Path] <-  OidAndPath ],
     
   State.
   
   
   rename_el(Oid, Path) ->
     try 
        %timer:sleep(100),
        %test in {53,1521}
        %Oid = {53,1521},
        ?LOGINFO("DEBUG RENAME_ELEMENTS: Start Edit ~p , ~p ", [Oid, Path]),
        FpDbOid=fp_db:open( Oid ),

        %get current script
        {ResF1,Name}=fp_db:read_field(FpDbOid, <<".name">>),
        % ?LOGINFO("DEBUG RENAME_ELEMENTS: ResF1 .name ~p", [Name]),
        NewName = binary:replace(Name, <<"q_load">>, <<"config_q_load">>),
        % ?LOGINFO("DEBUG RENAME_ELEMENTS: NewName ~p", [NewName])
        ResEdit = fp_db:edit_object(FpDbOid, #{<<".name">> => NewName}),
        ?LOGINFO("DEBUG RENAME_ELEMENTS: END ResEdit (model_control) from ~p to ~p is ~p ", [ Name, NewName, ResEdit])  
        
     catch
        _:Error:Stack->            
            % fp:log(info,"DEBUG RENAME_ELEMENTS: ERROR: Oid ~p  ",[Oid]),
            ?LOGINFO("DEBUG RENAME_ELEMENTS: ERROR: Oid ~p Path ~p ",[Oid, Path]),
            ?LOGINFO("DEBUG RENAME_ELEMENTS: ERROR: Error ~p ",[Error]),
            ?LOGINFO("DEBUG RENAME_ELEMENTS: ERROR: Stack ~p ",[Stack])
     end.
     
   
  
  