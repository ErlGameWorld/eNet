-module(ntUdpSrv).

-include("eNet.hrl").
-include("ntCom.hrl").

-compile(inline).
-compile({inline_size, 128}).

-export([
   start_link/3
   , getOpts/1
   , getOpenPort/1

   , init_it/3
   , system_code_change/4
   , system_continue/3
   , system_get_state/1
   , system_terminate/4
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% genActor  start %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(start_link(atom(), inet:port_number(), [listenOpt()]) -> {ok, pid()} | ignore | {error, term()}).
start_link(UoName, Port, UoOpts) ->
   proc_lib:start_link(?MODULE, init_it, [UoName, self(), {Port, UoOpts}], infinity, []).

init_it(UoName, Parent, Args) ->
   case safeRegister(UoName) of
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
system_terminate(Reason, _Parent, _Debug, State) ->
   terminate(Reason, State).

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
            kpS ->
               loop(Parent, State);
            {ok, NewState} ->
               loop(Parent, NewState);
            {stop, Reason} ->
               terminate(Reason, State)
         end
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% genActor  end %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {
   listenAddr :: inet:ip_address()
   , listenPort :: inet:port_number()
   , oSock :: inet:socket()
   , opts :: [listenOpt()]
   , conMod :: atom()
   , peers = #{} :: map()
}).

-define(DefUdpOpts, [binary, {reuseaddr, true}]).

init({Port, UoOpts}) ->
   UdpOpts = ?ntGLV(udpOpts, UoOpts, []),
   LastUdpOpts = ntCom:mergeOpts(?DefUdpOpts, UdpOpts),
   %% Don't active the socket...
   case gen_udp:open(Port, lists:keystore(active, 1, LastUdpOpts, {active, false})) of
      {ok, OSock} ->
         AptCnt = ?ntGLV(aptCnt, UoOpts, ?AptCnt),
         ConMod = ?ntGLV(conMod, UoOpts, undefined),
         {ok, {LAddr, LPort}} = inet:sockname(OSock),
         % ?ntInfo("success to open on ~p ~p ~n", [LAddr, LPort]),
         ok = inet:setopts(OSock, [{active, 100}]),
         {ok, #state{listenAddr = LAddr, listenPort = LPort, oSock = OSock, conMod = ConMod, opts = [{acceptors, AptCnt}, {tcpOpts, LastUdpOpts}]}};
      {error, Reason} ->
         ?ntErr("failed to open on ~p - ~p (~s) ~n", [Port, Reason, inet:format_error(Reason)]),
         {stop, Reason}
   end.

handleMsg({udp, Sock, IP, InPortNo, AncData, Packet}, #state{oSock = Sock, conMod = ConMod, peers = Peers} = State) ->
   case maps:find({IP, InPortNo}, Peers) of
      {ok, Pid} ->
         Pid ! {datagram, self(), Packet},
         kpS;
      error ->
         try ConMod:datagram(Sock, IP, InPortNo, AncData, Packet) of
            {ok, Pid} ->
               _Ref = erlang:monitor(process, Pid),
               Pid ! {datagram, self(), Packet},
               {noreply, addPeer({IP, InPortNo}, Pid, State)};
            {error, Reason} ->
               ?ntErr("Failed to start udp channel for peer ~p ~p reason: ~p", [IP, InPortNo, Reason]),
               kpS
         catch
            C:R:S ->
               ?ntErr("Exception occurred when starting udp channel for peer ~p ~p, reason: ~p", [IP, InPortNo, {C, R, S}]),
               kpS
         end
   end;
handleMsg({udp, Sock, IP, InPortNo, Packet}, #state{oSock = Sock, conMod = ConMod, peers = Peers} = State) ->
   case maps:find({IP, InPortNo}, Peers) of
      {ok, Pid} ->
         Pid ! {datagram, self(), Packet},
         kpS;
      error ->
         try ConMod:datagram(Sock, IP, InPortNo, undefined, Packet) of
            {ok, Pid} ->
               _Ref = erlang:monitor(process, Pid),
               Pid ! {datagram, self(), Packet},
               {ok, addPeer({IP, InPortNo}, Pid, State)};
            {error, Reason} ->
               ?ntErr("Failed to start udp channel for peer ~p ~p reason: ~p", [IP, InPortNo, Reason]),
               kpS
         catch
            C:R:S ->
               ?ntErr("Exception occurred when starting udp channel for peer ~p ~p, reason: ~p", [IP, InPortNo, {C, R, S}]),
               kpS
         end
   end;
handleMsg({udp_passive, Sock}, #state{oSock = Sock} = _State) ->
   inet:setopts(Sock, [{active, 100}]),
   kpS;

handleMsg({'DOWN', _MRef, process, DownPid, _Reason}, State = #state{peers = Peers}) ->
   peerDown(DownPid, Peers, State);

handleMsg({'EXIT', DownPid, _Reason}, State = #state{peers = Peers}) ->
   peerDown(DownPid, Peers, State);

handleMsg({'$gen_call', From, miOpts}, #state{opts = Opts} = _State) ->
   gen_server:reply(From, Opts),
   kpS;

handleMsg({'$gen_call', From, miOpenPort}, #state{listenPort = LPort} = _State) ->
   gen_server:reply(From, LPort),
   kpS;

handleMsg(_Msg, _State) ->
   ?ntErr("~p unexpected info: ~p ~n", [?MODULE, _Msg]),
   kpS.

terminate(Reason, #state{oSock = LSock}) ->
   catch gen_udp:close(LSock),
   exit(Reason).

-spec getOpts(pid()) -> [listenOpt()].
getOpts(Listener) ->
   gen_server:call(Listener, miOpts).

-spec getOpenPort(pid()) -> inet:port_number().
getOpenPort(Listener) ->
   gen_server:call(Listener, miOpenPort).

addPeer(Peer, Pid, State = #state{peers = Peers}) ->
   State#state{peers = maps:put(Pid, Peer, maps:put(Peer, Pid, Peers))}.

delPeer(Peer, Pid, State = #state{peers = Peers}) ->
   State#state{peers = maps:remove(Peer, maps:remove(Pid, Peers))}.

peerDown(DownPid, Peers, State) ->
   case maps:find(DownPid, Peers) of
      {ok, Peer} ->
         {ok, delPeer(Peer, DownPid, State)};
      error ->
         kpS
   end.