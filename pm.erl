-module(example).
-export([start/0, hello/0]).

start() ->
    io:format("Starting Erlang application~n"),
    hello().

hello() ->
    io:format("Hello from Erlang!~n").
