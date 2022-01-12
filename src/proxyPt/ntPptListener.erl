-module(ntPptListener).

-include("eNet.hrl").
-include("ntCom.hrl").

-compile(inline).
-compile({inline_size, 128}).

-export([
   start_link/4
   , getOpts/1
   , getListenPort/1

   , init_it/3
   , system_code_change/4
   , system_continue/3
   , system_get_state/1
   , system_terminate/4
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% genActor  start %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(start_link(atom(), atom(), inet:port_number(), [listenOpt()]) -> {ok, pid()} | ignore | {error, term()}).
start_link(ListenName, AptSupName, Port, ListenOpts) ->
   proc_lib:start_link(?MODULE, init_it, [ListenName, self(), {AptSupName, Port, ListenOpts}], infinity, []).

init_it(Name, Parent, Args) ->
   case safeRegister(Name) of
      true ->
         process_flag(trap_exit, true),
         modInit(Parent, Args);
      {false, Pid} ->
         proc_lib:init_ack(Parent, {error, {already_started, Pid}})
   end.

-spec system_code_change(term(), module(), undefined | term(), term()) -> {ok, term()}.
system_code_change(State, _Module, _OldVsn, _Extra) ->
   {ok, State}.

-spec system_continue(pid(), [], {module(), atom(), pid(), term()}) -> ok.
system_continue(_Parent, _Debug, {Parent, State}) ->
   loop(Parent, State).

-spec system_get_state(term()) -> {ok, term()}.
system_get_state(State) ->
   {ok, State}.

-spec system_terminate(term(), pid(), [], term()) -> none().
system_terminate(Reason, _Parent, _Debug, _State) ->
   exit(Reason).

safeRegister(Name) ->
   try register(Name, self()) of
      true -> true
   catch
      _:_ -> {false, whereis(Name)}
   end.

modInit(Parent, Args) ->
   case init(Args) of
      {ok, State} ->
         proc_lib:init_ack(Parent, {ok, self()}),
         loop(Parent, State);
      {stop, Reason} ->
         proc_lib:init_ack(Parent, {error, Reason}),
         exit(Reason)
   end.

loop(Parent, State) ->
   receive
      {system, From, Request} ->
         sys:handle_system_msg(Request, From, Parent, ?MODULE, [], {Parent, State});
      {'EXIT', Parent, Reason} ->
         terminate(Reason, State);
      Msg ->
         case handleMsg(Msg, State) of
            {ok, NewState} ->
               loop(Parent, NewState);
            {stop, Reason} ->
               terminate(Reason, State),
               exit(Reason)
         end
   end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% genActor  end %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {
   listenAddr :: inet:ip_address()
   , listenPort :: inet:port_number()
   , lSock :: inet:socket()
   , opts :: [listenOpt()]
}).

-define(DefTcpOpts, [{nodelay, true}, {reuseaddr, true}, {send_timeout, 30000}, {send_timeout_close, true}]).

init({AptSupName, Port, ListenOpts}) ->
   process_flag(trap_exit, true),
   TcpOpts = ?getLValue(tcpOpts, ListenOpts, []),
   LastTcpOpts = ntCom:mergeOpts(?DefTcpOpts, TcpOpts),
   %% Don't active the socket...
   case gen_tcp:listen(Port, lists:keystore(active, 1, LastTcpOpts, {active, false})) of
      {ok, LSock} ->
         AptCnt = ?getLValue(aptCnt, ListenOpts, ?AptCnt),
         ConMod = ?getLValue(conMod, ListenOpts, undefined),
         startAcceptor(AptCnt, LSock, AptSupName, ConMod),
         {ok, {LAddr, LPort}} = inet:sockname(LSock),
         % ?ntInfo("success to listen on ~p ~n", [Port]),
         {ok, #state{listenAddr = LAddr, listenPort = LPort, lSock = LSock, opts = [{acceptors, AptCnt}, {tcpOpts, LastTcpOpts}]}};
      {error, Reason} ->
         ?ntErr("failed to listen on ~p - ~p (~s) ~n", [Port, Reason, inet:format_error(Reason)]),
         {stop, Reason}
   end.

handleMsg({'$gen_call', From, miOpts}, #state{opts = Opts} = State) ->
   gen_server:reply(From, Opts),
   {ok, State};

handleMsg({'$gen_call', From, miListenPort}, #state{listenPort = LPort} = State) ->
   gen_server:reply(From, LPort),
   {ok, State};

handleMsg(_Msg, State) ->
   ?ntErr("~p unexpected info: ~p ~n", [?MODULE, _Msg]),
   {noreply, State}.

terminate(_Reason, #state{lSock = LSock, listenAddr = Addr, listenPort = Port}) ->
   ?ntInfo("stopped on ~s:~p ~n", [inet:ntoa(Addr), Port]),
   %% 关闭这个监听LSock  监听进程收到tcp_close 然后终止acctptor进程
   catch port_close(LSock),
   ok.

startAcceptor(0, _LSock, _AptSupName, _ConMod) ->
   ok;
startAcceptor(N, LSock, AptSupName, ConMod) ->
   supervisor:start_child(AptSupName, [LSock, ConMod, []]),
   startAcceptor(N - 1, LSock, AptSupName, ConMod).

-spec getOpts(pid()) -> [listenOpt()].
getOpts(Listener) ->
   gen_server:call(Listener, miOpts).

-spec getListenPort(pid()) -> inet:port_number().
getListenPort(Listener) ->
   gen_server:call(Listener, miListenPort).

