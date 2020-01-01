-module(nlTcpAcceptorIns).
-include("erlNetLib.hrl").

-compile(inline).
-compile({inline_size, 128}).


-export([

   %% genExm API
   init/1
   , handleMsg/2
   , terminate/2
]).

-record(state, {
   lSock
   , ref
   , cliMod
   , sockMod
}).

-spec init(Args :: term()) -> ok.
init({LSock, CliMod, SockMod}) ->
   case prim_inet:async_accept(LSock, -1) of
      {ok, Ref} ->
         {ok, #state{lSock = LSock, ref = Ref,  cliMod = CliMod, sockMod = SockMod}};
      {error, Reason} ->
         ?WARN(nlTcpAcceptorIns_init , " prim_inet:async_accept error ~p~n",[Reason]),
         {stop, Reason}
   end.

handleMsg({inet_async, LSock, Ref, Msg}, #state{lSock = LSock, ref = Ref, cliMod = CliMod, sockMod = SockMod} = State) ->
   case Msg of
      {ok, Sock} ->
         %% make it look like gen_tcp:accept
         inet_db:register_socket(Sock, SockMod),
         try CliMod:newConnect(Sock) of
            {ok, Pid} ->
               gen_tcp:controlling_process(Sock, Pid),
               Pid ! {?miSockReady, Sock},
               case prim_inet:async_accept(LSock, -1) of
                  {ok, NewRef} ->
                     {ok, State#state{ref = NewRef}};
                  {error, Reason} ->
                     ?WARN(nlTcpAcceptorIns_handleMsg , " prim_inet:async_accept error ~p~n",[Reason]),
                     {stop, Reason}
               end
         catch
            E:R:S ->
               ?WARN(nlTcpAcceptorIns_handleMsg, "CliMod:newConnect crash: ~p:~p~n~p~n ~n ", [E, R, S]),
               case prim_inet:async_accept(LSock, -1) of
                  {ok, NewRef} ->
                     {ok, State#state{ref = NewRef}};
                  {error, Reason} ->
                     ?WARN(nlTcpAcceptorIns_handleMsg , " prim_inet:async_accept error ~p~n",[Reason]),
                     {stop, Reason}
               end
         end;
      {error, closed} ->
         ?WARN(nlTcpAcceptorIns_handleMsg , "listen sock error ~p~n",[closed]),
         {stop, lsock_closed};
      {error, Reason} ->
         ?WARN(nlTcpAcceptorIns_handleMsg , "listen sock error ~p~n",[Reason]),
         {stop, {lsock, Reason}}
   end;
handleMsg(_Msg, State) ->
   ?WARN(?MODULE, "receive unexpected  msg: ~p", [_Msg]),
   {ok, State}.

terminate(_Reason, #state{lSock = LSock}) ->
   catch port_close(LSock),
   ok.

