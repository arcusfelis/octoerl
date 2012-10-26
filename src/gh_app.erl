% vim: set filetype=erlang shiftwidth=4 tabstop=4 expandtab tw=80:

-module(gh_app).

-behaviour(application).
-export([start/0,start/2,stop/1]).


start() ->
	application:start(crypto),
	application:start(public_key),
	application:start(ssl),
	application:start(lhttpc),
    application:start(octoerl).


start(_Type, _StartArgs) ->
    gh_sup:start_link().

stop(_State) ->
    ok.
