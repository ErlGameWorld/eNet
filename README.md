# erlNetLib
    erlang网络库

    {ok, Pid} = erlNetLib:addTcpLr(test11, {{127,0,0, 1}, 9010}, echo_server, [{tcpOpts, [binary, {packet, 2}, {active, false}]}]).

    {ok, S} = gen_tcp:connect({127,0,0, 1}, 9007, [{binary, {packet, 2}}]).
    {ok, S} = gen_tcp:connect({127,0,0, 1}, 9007, [binary]).
