-module(utPPtANSrv).             %% ssl active N server

-behaviour(gen_server).

-include("eNet.hrl").
-include("ntCom.hrl").

-export([newConn/1]).

-export([start/2, start_link/1]).

%% gen_server Function Exports
-export([init/1
   , handle_call/3
   , handle_cast/2
   , handle_info/2
   , terminate/2
   , code_change/3
]).

-record(state, {socket}).

start(Name, Port) ->
   PrivDir = code:priv_dir(eNet),
   TcpOpts = [binary, {reuseaddr, true}],
   SslOpts = [
      {certfile, filename:join(PrivDir, "demo.crt")},
      {keyfile, filename:join(PrivDir, "demo.key")}
   ],
   Opts = [{tcpOpts, TcpOpts}, {sslOpts, SslOpts}, {conMod, ?MODULE}, {proxyPt, true}, {proxyPtTet, 1111111}],
   eNet:openPpt(Name, Port, Opts).

start_link(Sock) ->
   {ok, proc_lib:spawn_link(?MODULE, init, [Sock])}.

newConn(Sock) ->
   start_link(Sock).

init(_Sock) ->
   gen_server:enter_loop(?MODULE, [], #state{}).

handle_call(_Request, _From, State) ->
   io:format("handle_call for______ ~p~n", [_Request]),
   {reply, ignore, State}.

handle_cast(_Msg, State) ->
   io:format("handle_cast for______ ~p~n", [_Msg]),
   {noreply, State}.

handle_info({ssl, Socket, Data}, State = #state{socket = _Sock}) ->
   io:format("packet:~p  Data from ~s~n", [ssl:getopts(Socket, [packet]), Data]),
   ssl:send(Socket, Data),
   {noreply, State};

handle_info({ssl_error, _Socket, Reason}, State) ->
   io:format("ssl_error for ~p~n", [Reason]),
   {stop, {shutdown, Reason}, State};
handle_info({ssl_closed, _Socket}, State) ->
   io:format("ssl_closed"),
   {noreply, State};

handle_info({ssl_passive, SslSock}, State) ->
   ssl:setopts(SslSock, [{active, 100}]),
   {noreply, State};
handle_info({?mSockReady, Sock, SslOpts, SslHSTet, ProxyPt, ProxyPtTet}, State) ->
   case ntPptAcceptor:pptAndHS(Sock, SslOpts, SslHSTet, ProxyPt, ProxyPtTet) of
      {ok, SslSock} ->
         ssl:setopts(SslSock, [{active, 100}]),
         {noreply, State#state{socket = SslSock}};
      _Err ->
         io:format("handshake error ~p~n", [_Err]),
         {stop, State}
   end;
handle_info(_Info, State) ->
   io:format("handle_info for______ ~p~n", [_Info]),
   {noreply, State}.

terminate(_Reason, #state{socket = Sock}) ->
   catch ssl:close(Sock).

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

