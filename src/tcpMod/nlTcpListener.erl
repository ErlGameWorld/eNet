-module(nlTcpListener).
-include("erlNetLib.hrl").

-behaviour(gen_server).

-export([start_link/3]).

-export([
   options/1
   , get_port/1
]).

%% gen_server callbacks
-export([init/1
   , handle_call/3
   , handle_cast/2
   , handle_info/2
   , terminate/2
   , code_change/3
]).

-record(state, {
   serverName :: atom()
   , listenAddr :: inet:ip_address()
   , listenPort :: inet:port_number()
   , lSock :: inet:socket()
   , opts :: [listenOpt()]
}).

-define(ACCEPTOR_POOL, 16).
-define(DEFAULT_TCP_OPTIONS,
   [{nodelay, true},
      {reuseaddr, true},
      {send_timeout, 30000},
      {send_timeout_close, true}
   ]).

-spec(start_link(atom(), listenOn(), [listenOpt()]) -> {ok, pid()} | ignore | {error, term()}).
start_link(ListenName, ListenOn, Opts) ->
   gen_server:start_link({local, ListenName}, ?MODULE, {ListenName, ListenOn, Opts}, []).

-spec(options(pid()) -> [esockd:option()]).
options(Listener) ->
   gen_server:call(Listener, options).

-spec(get_port(pid()) -> inet:port_number()).
get_port(Listener) ->
   gen_server:call(Listener, get_port).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init({Proto, ListenOn, Opts}) ->
   Port = port(ListenOn),
   process_flag(trap_exit, true),
   SockOpts = merge_addr(ListenOn, sockopts(Opts)),
   %% Don't active the socket...
   case gen_tcp:listen(Port, [{active, false} | lists:keydelete(active, 1, SockOpts)]) of
      {ok, LSock} ->
         AcceptorNum = ?getListValue(acceptors, Opts, ?ACCEPTOR_POOL),
         startAcceptor(AcceptorNum, LSock),
         {ok, {LAddr, LPort}} = inet:sockname(LSock),
         {ok, #state{proto = Proto, listen_on = ListenOn, options = Opts,
            lsock = LSock, laddr = LAddr, lport = LPort}};
      {error, Reason} ->
         error_logger:error_msg("~s failed to listen on ~p - ~p (~s)",
            [Proto, Port, Reason, inet:format_error(Reason)]),
         {stop, Reason}
   end.

sockopts(Opts) ->
   TcpOpts = proplists:get_value(tcp_options, Opts, []),
   esockd_util:merge_opts(?DEFAULT_TCP_OPTIONS, TcpOpts).

port(Port) when is_integer(Port) -> Port;
port({_Addr, Port}) -> Port.

merge_addr(Port, SockOpts) when is_integer(Port) ->
   SockOpts;
merge_addr({Addr, _Port}, SockOpts) ->
   lists:keystore(ip, 1, SockOpts, {ip, Addr}).

handle_call(options, _From, State = #state{options = Opts}) ->
   {reply, Opts, State};

handle_call(get_port, _From, State = #state{lport = LPort}) ->
   {reply, LPort, State};

handle_call(Req, _From, State) ->
   error_logger:error_msg("[~s] unexpected call: ~p", [?MODULE, Req]),
   {noreply, State}.

handle_cast(Msg, State) ->
   error_logger:error_msg("[~s] unexpected cast: ~p", [?MODULE, Msg]),
   {noreply, State}.

handle_info(Info, State) ->
   error_logger:error_msg("[~s] unexpected info: ~p", [?MODULE, Info]),
   {noreply, State}.

terminate(_Reason, #state{proto = Proto, listen_on = ListenOn,
   lsock = LSock, laddr = Addr, lport = Port}) ->
   error_logger:info_msg("~s stopped on ~s~n", [Proto, esockd_net:format({Addr, Port})]),
   esockd_rate_limiter:delete({listener, Proto, ListenOn}),
   esockd_server:del_stats({Proto, ListenOn}),
   esockd_transport:fast_close(LSock).

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

startAcceptor(0, _LSock) ->
   ok;
startAcceptor(N, LSock) ->
   nlTcpAcceptorSup:start_acceptor(nlTcpAcceptorSup, LSock),
   startAcceptor(N - 1, LSock).