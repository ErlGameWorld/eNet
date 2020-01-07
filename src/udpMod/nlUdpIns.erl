-module(nlUdpIns).

-export([
   server/4
   , count_peers/1
   , stop/1
]).

-export([
   init/1
   , handleMsg/2
   , terminate/2
]).

-record(state, {proto, sock, port, peers, mfa}).

-define(ERROR_MSG(Format, Args),
   error_logger:error_msg("[~s]: " ++ Format, [?MODULE | Args])).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec(server(atom(), esockd:listen_on(), [gen_udp:option()], mfa())
      -> {ok, pid()} | {error, term()}).
server(Proto, Port, Opts, MFA) when is_integer(Port) ->
   gen_server:start_link(?MODULE, [Proto, Port, Opts, MFA], []);
server(Proto, {Host, Port}, Opts, MFA) when is_integer(Port) ->
   IfAddr = case proplists:get_value(ip, Opts) of
               undefined -> proplists:get_value(ifaddr, Opts);
               Addr -> Addr
            end,
   (IfAddr == undefined) orelse (IfAddr = Host),
   gen_server:start_link(?MODULE, [Proto, Port, merge_addr(Host, Opts), MFA], []).

merge_addr(Addr, Opts) ->
   lists:keystore(ip, 1, Opts, {ip, Addr}).

-spec(count_peers(pid()) -> integer()).
count_peers(Pid) ->
   gen_server:call(Pid, count_peers).

-spec(stop(pid()) -> ok).
stop(Pid) ->
   gen_server:stop(Pid, normal, infinity).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([Proto, Port, Opts, MFA]) ->
   process_flag(trap_exit, true),
   case gen_udp:open(Port, esockd_util:merge_opts([binary, {reuseaddr, true}], Opts)) of
      {ok, Sock} ->
         %% Trigger the udp_passive event
         inet:setopts(Sock, [{active, 1}]),
         %% error_logger:info_msg("~s opened on udp ~p~n", [Proto, Port]),
         {ok, #state{proto = Proto, sock = Sock, port = Port, peers = #{}, mfa = MFA}};
      {error, Reason} ->
         {stop, Reason}
   end.

handleMsg(count_peers, _From, State = #state{peers = Peers}) ->
   {reply, maps:size(Peers) div 2, State, hibernate};

handleMsg(Req, _From, State) ->
   ?ERROR_MSG("unexpected call: ~p", [Req]),
   {reply, ignored, State}.


handleMsg({udp, Sock, IP, InPortNo, Packet},
   State = #state{sock = Sock, peers = Peers, mfa = {M, F, Args}}) ->
   Peer = {IP, InPortNo},
   case maps:find(Peer, Peers) of
      {ok, Pid} ->
         Pid ! {datagram, self(), Packet},
         {noreply, State};
      error ->
         try erlang:apply(M, F, [{udp, self(), Sock}, Peer | Args]) of
            {ok, Pid} ->
               _Ref = erlang:monitor(process, Pid),
               Pid ! {datagram, self(), Packet},
               {noreply, store_peer(Peer, Pid, State)};
            {error, Reason} ->
               ?ERROR_MSG("Error returned. udp channel: ~s, reason: ~p",
                  [esockd_net:format(Peer), Reason]),
               {noreply, State}
         catch
            _Error:Reason ->
               ?ERROR_MSG("Failed to start udp channel: ~s, reason: ~p",
                  [esockd_net:format(Peer), Reason]),
               {noreply, State}
         end
   end;

handleMsg({udp_passive, Sock}, State) ->
   %% TODO: rate limit here?
   inet:setopts(Sock, [{active, 100}]),
   {noreply, State, hibernate};

handleMsg({'DOWN', _MRef, process, DownPid, _Reason}, State = #state{peers = Peers}) ->
   case maps:find(DownPid, Peers) of
      {ok, Peer} ->
         {noreply, erase_peer(Peer, DownPid, State)};
      error -> {noreply, State}
   end;

handleMsg({datagram, Peer = {IP, Port}, Packet}, State = #state{sock = Sock}) ->
   case gen_udp:send(Sock, IP, Port, Packet) of
      ok -> ok;
      {error, Reason} ->
         ?ERROR_MSG("Dropped packet to: ~s, reason: ~s", [esockd_net:format(Peer), Reason])
   end,
   {noreply, State};

handleMsg(Info, State) ->
   ?ERROR_MSG("unexpected info: ~p", [Info]),
   {noreply, State}.

terminate(_Reason, #state{sock = Sock}) ->
   gen_udp:close(Sock).


store_peer(Peer, Pid, State = #state{peers = Peers}) ->
   State#state{peers = maps:put(Pid, Peer, maps:put(Peer, Pid, Peers))}.

erase_peer(Peer, Pid, State = #state{peers = Peers}) ->
   State#state{peers = maps:remove(Peer, maps:remove(Pid, Peers))}.

