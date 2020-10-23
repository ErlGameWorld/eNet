-module(echo_server).
-behaviour(gen_server).

%% start
-export([start/1, newAcceptor/1]).

-export([start_link/2]).

%% gen_server Function Exports
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(state, {transport, socket}).

start(Port) ->
    ok = esockd:start(),
    TcpOpts = [binary,
               {reuseaddr, true},
               {backlog, 512},
               {nodelay, false}
              ],
    Options = [{acceptors, 8},
               {max_connections, 100000},
               {tcp_options, TcpOpts}
              ],
    MFArgs = {?MODULE, start_link, []},
    esockd:open(echo, Port, Options, MFArgs).

start_link(Transport, Sock) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [[Transport, Sock]])}.

newAcceptor(Sock) ->
   start_link(prim_inet, Sock).
   %case whereis(tttttMgr) of
   %   undefined ->
   %      start_link(prim_inet, Sock);
   %   Pid ->
   %      {ok, Pid}
   %end.

safeRegister(Name) ->
   try register(Name, self()) of
      true -> true
   catch
      _:_ -> {false, whereis(Name)}
   end.

init([Transport, Sock]) ->
   safeRegister(tttttMgr),
   gen_server:enter_loop(?MODULE, [], #state{}).

handle_call(_Request, _From, State) ->
   io:format("handle_call for______ ~p~n", [_Request]),
    {reply, ignore, State}.

handle_cast(_Msg, State) ->
   io:format("handle_cast for______ ~p~n", [_Msg]),
    {noreply, State}.

handle_info({inet_async, Sock, _Ref, {ok, Data}}, State = #state{transport = Transport, socket = _Sock}) ->
    {ok, Peername} = inet:peername(Sock),
    io:format("packet:~p  Data from ~p: ~s~n", [inet:getopts(Sock, [packet]), Peername, Data]),
    prim_inet:send(Sock, Data),
    prim_inet:async_recv(Sock, 0, -1),
    {noreply, State};

handle_info({inet_async, _Sock, _Ref, {error, Reason}}, State) ->
    io:format("Shutdown for ~p~n", [Reason]),
    shutdown(Reason, State);

handle_info({inet_reply, _Sock ,ok}, State) ->
   io:format("inet_reply for______ ~p~n", [ok]),
    {noreply, State};

handle_info({inet_reply, _Sock, {error, Reason}}, State) ->
    io:format("Shutdown for ~p~n", [Reason]),
    shutdown(Reason, State);

handle_info({miSockReady, Sock}, State) ->
   prim_inet:async_recv(Sock, 0, -1),
   io:format("get miSockReady for______ ~p~n", [Sock]),
   {noreply, State};

handle_info(_Info, State) ->
   io:format("handle_info for______ ~p~n", [_Info]),
    {noreply, State}.

terminate(_Reason, #state{transport = Transport, socket = Sock}) ->
   catch port_close(Sock).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

shutdown(Reason, State) ->
    {stop, {shutdown, Reason}, State}.

