-module(utTcpAFSrv).             %% tcp active false server
-behaviour(gen_server).

-include("eNet.hrl").

%% start
-export([newConn/2]).

-export([start/2, start_link/1]).

%% gen_server Function Exports
-export([init/1
   , handle_call/3
   , handle_cast/2
   , handle_info/2
   , terminate/2
   , code_change/3
]).

-record(state, {transport, socket}).

start(Name, Port) ->
   TcpOpts = [binary, {reuseaddr, true}],
   Opts = [{tcpOpts, TcpOpts}, {conMod, ?MODULE}],
   eNet:openTcp(Name, Port, Opts).

start_link(Sock) ->
   {ok, proc_lib:spawn_link(?MODULE, init, [Sock])}.

newConn(Sock, _) ->
   start_link(Sock).

init(_Sock) ->
   gen_server:enter_loop(?MODULE, [], #state{}).

handle_call(_Request, _From, State) ->
   io:format("handle_call for______ ~p~n", [_Request]),
   {reply, ignore, State}.

handle_cast(_Msg, State) ->
   io:format("handle_cast for______ ~p~n", [_Msg]),
   {noreply, State}.

handle_info({inet_async, Sock, _Ref, {ok, Data}}, State = #state{socket = _Sock}) ->
   {ok, Peername} = inet:peername(Sock),
   io:format("packet:~p  Data from ~p: ~s~n", [inet:getopts(Sock, [packet]), Peername, Data]),
   prim_inet:send(Sock, Data),
   prim_inet:async_recv(Sock, 0, -1),
   {noreply, State};

handle_info({inet_async, _Sock, _Ref, {error, Reason}}, State) ->
   io:format("Shutdown for ~p~n", [Reason]),
   shutdown(Reason, State);

handle_info({inet_reply, _Sock, ok}, State) ->
   io:format("inet_reply for______ ~p~n", [ok]),
   {noreply, State};

handle_info({inet_reply, _Sock, {error, Reason}}, State) ->
   io:format("Shutdown for ~p~n", [Reason]),
   shutdown(Reason, State);

handle_info({?mSockReady, Sock}, State) ->
   prim_inet:async_recv(Sock, 0, -1),
   io:format("get miSockReady for______ ~p~n", [Sock]),
   {noreply, State};

handle_info(_Info, State) ->
   io:format("handle_info for______ ~p~n", [_Info]),
   {noreply, State}.

terminate(_Reason, #state{socket = Sock}) ->
   catch gen_tcp:close(Sock).

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

shutdown(Reason, State) ->
   {stop, {shutdown, Reason}, State}.

