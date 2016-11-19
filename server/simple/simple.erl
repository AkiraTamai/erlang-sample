-module(simple).
-export([loop/0, rpc/2]).

rpc(Pid, Request) ->
  Pid ! {self(), Request},
  receive
    Response ->
      Response
  end.
loop() ->
  receive
    {From, {order, Operation}} ->
      From ! {200, Operation},
      loop();
    {From, Other} ->
      From ! {error, Other},
      loop()
  end.

% run sample
%1> c(simple).
%{ok,simple}
%2> Pid = spawn(fun simple:loop/0).
%<0.77.0>
%3> simple:rpc(Pid, {order,{100,200}}).
%{200,{100,200}}
%4> simple:rpc(Pid, {noorder,{100,200}}).
%{error,{noorder,{100,200}}}

