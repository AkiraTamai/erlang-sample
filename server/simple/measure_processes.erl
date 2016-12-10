-module(measure_processes).
-export([limit/1]).

limit(N) ->
  Limit = erlang:system_info(process_limit),
  io:format("Limit allowed processes:~p~n", [Limit]),
  statistics(runtime),
  statistics(wall_clock),
  L = for(1, N, fun() -> spawn(fun() -> wait() end) end),
  {_, Time1} = statistics(runtime),
  {_, Time2} = statistics(wall_clock),
  lists:foreach(fun(Pid) -> Pid ! die end, L),
  U1 = Time1 * 1000 /N,
  U2 = Time2 * 1000 /N,
  io:format("Process spawn time=~p (~p) μs~n", [U1,U2]).

wait() ->
  receive
    die -> void
  end.

for(N, N, F) -> [F()];
for(I, N, F) -> [F()|for(I+1, N, F)].


%1> measure_processes:limit(1000).
%Limit allowed processes:262144
%Process spawn time=0.0 (9.0) μs
%ok
%2> measure_processes:limit(10000).
%Limit allowed processes:262144
%Process spawn time=4.0 (8.6) μs
%ok
%3> measure_processes:limit(100000).
%Limit allowed processes:262144
%Process spawn time=3.9 (8.84) μs
%ok
%4> measure_processes:limit(200000).
%Limit allowed processes:262144
%Process spawn time=3.6 (8.34) μs
%ok
%5> measure_processes:limit(210000).
%Limit allowed processes:262144
%Process spawn time=3.9047619047619047 (8.790476190476191) μs
%ok
