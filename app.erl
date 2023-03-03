%%%-------------------------------------------------------------------
%% @doc myproj public API
%% @end
%%%-------------------------------------------------------------------

-module(app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("Hello, world!~n"),
    ok.

stop(_State) ->
    io:format("Hello, world!~n"),
    ok.