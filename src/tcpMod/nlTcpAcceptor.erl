-module(nlTcpAcceptor).
-include("eNet.hrl").
-compile(inline).
-compile({inline_size, 128}).

%% 该模块不可热更新

-export([
   start_link/3

   , init/1
   , handleMsg/2

   , init_it/2
   , system_code_change/4
   , system_continue/3
   , system_get_state/1
   , system_terminate/4
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% genActor  start %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_link(socket(), module(), [proc_lib:spawn_option()]) -> {ok, pid()}.
start_link(LSock, ConMod, SpawnOpts) ->
   proc_lib:start_link(?MODULE, init_it, [self(), {LSock, ConMod}], infinity, SpawnOpts).

init_it(Parent, Args) ->
   process_flag(trap_exit, true),
   moduleInit(Parent, Args).

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

moduleInit(Parent, Args) ->
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
         exit(Reason);
      Msg ->
         case handleMsg(Msg, State) of
            {ok, NewState} ->
               loop(Parent, NewState);
            {stop, Reason} ->
               exit(Reason)
         end
   end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% genActor  end %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {
   lSock
   , ref
   , conMod
   , sockMod
}).

-spec init(Args :: term()) -> ok.
init({LSock, ConMod}) ->
   case prim_inet:async_accept(LSock, -1) of
      {ok, Ref} ->
         {ok, SockMod} = inet_db:lookup_socket(LSock),
         {ok, #state{lSock = LSock, ref = Ref,  conMod = ConMod, sockMod = SockMod}};
      {error, Reason} ->
         ?WARN(nlTcpAcceptor , " init prim_inet:async_accept error ~p~n",[Reason]),
         {stop, Reason}
   end.

handleMsg({inet_async, LSock, Ref, Msg}, #state{lSock = LSock, ref = Ref, conMod = ConMod, sockMod = SockMod} = State) ->
   case Msg of
      {ok, Sock} ->
         %% make it look like gen_tcp:accept
         inet_db:register_socket(Sock, SockMod),
         try ConMod:newAcceptor(Sock) of
            {ok, Pid} ->
               io:format("IMY******************controlling_process   ~p ~p ~n",[Sock, Pid]),
               gen_tcp:controlling_process(Sock, Pid),
               Pid ! {?miSockReady, Sock},
               newAsyncAccept(LSock, State);
            {close, Reason} ->
               ?WARN(nlTcpAcceptor , " handleMsg ConMod:newAcceptor return close ~p~n",[Reason]),
               catch port_close(Sock),
               newAsyncAccept(LSock, State);
            _Ret ->
               ?WARN(nlTcpAcceptor , " ConMod:newAcceptor return error ~p~n",[_Ret]),
               {stop, error_ret}
         catch
            E:R:S ->
               ?WARN(nlTcpAcceptor, "CliMod:newConnect crash: ~p:~p~n~p~n ~n ", [E, R, S]),
               newAsyncAccept(LSock, State)
         end;
      {error, closed} ->
         ?WARN(nlTcpAcceptor , "error, closed listen sock error ~p~n",[closed]),
         {stop, normal};
      {error, Reason} ->
         ?WARN(nlTcpAcceptor , "listen sock error ~p~n",[Reason]),
         {stop, {lsock, Reason}}
   end;
handleMsg(_Msg, State) ->
   ?WARN(nlTcpAcceptor, "receive unexpected ~p  msg: ~p", [self(), _Msg]),
   {ok, State}.

newAsyncAccept(LSock, State) ->
   case prim_inet:async_accept(LSock, -1) of
      {ok, Ref} ->
         {ok, State#state{ref = Ref}};
      {error, Reason} ->
         ?WARN(nlTcpAcceptorIns , " prim_inet:async_accept error ~p~n",[Reason]),
         {stop, Reason}
   end.

