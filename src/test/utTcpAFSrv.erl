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
   %% 只把发送命令交给 port,不阻塞等结果;真正的发送结果由下面的 inet_reply / DOWN 分支消费
   case ntCom:asyncSend(Sock, Data) of
      ok ->
         ok;
      {error, Reason} ->
         io:format("asyncSend error: ~p~n", [Reason])
   end,
   prim_inet:async_recv(Sock, 0, -1),
   {noreply, State};

handle_info({inet_async, _Sock, _Ref, {error, Reason}}, State) ->
   io:format("inet_async error, Shutdown for ~p~n", [Reason]),
   shutdown(Reason, State);

%% asyncSend/3 把 MRef 编进了 port command,当前 OTP 的回复是带引用的 4 元组,
%% 收到后必须 demonitor,否则每次发送都会漏一个 monitor
handle_info({inet_reply, _Sock, ok, MRef}, State) ->
   erlang:demonitor(MRef, [flush]),
   io:format("inet_reply(have MRef) for______ ~p~n", [ok]),
   {noreply, State};

handle_info({inet_reply, _Sock, {error, Reason}, MRef}, State) ->
   erlang:demonitor(MRef, [flush]),
   sendError(Reason, State);

%% 被 monitor 的 port 关闭:port DOWN 时 monitor 已自动结束,不用再 demonitor
handle_info({'DOWN', _MRef, port, _Sock, Reason}, State) ->
   io:format("port DOWN for ~p~n", [Reason]),
   shutdown(Reason, State);

%% 老 OTP(不带引用)的回复格式。这一形态在当前 OTP 的 TCP 路径上拿不到
%% (socket port 不带 caller tag 的 port_command 会直接把 port 打成 einval),
%% 而且它没有 MRef,命中了也清不掉上面的 monitor;保留只为兜底不崩
handle_info({inet_reply, _Sock, ok}, State) ->
   io:format("inet_reply(no MRef) for______ ~p~n", [ok]),
   {noreply, State};

handle_info({inet_reply, _Sock, {error, Reason}}, State) ->
   sendError(Reason, State);

handle_info({?mSockReady, Sock}, State) ->
   prim_inet:async_recv(Sock, 0, -1),
   io:format("get miSockReady for______ ~p~n", [Sock]),
   {noreply, State};

handle_info(_Info, State) ->
   io:format("handle_info for______ ~p~n", [_Info]),
   {noreply, State}.

terminate(_Reason, #state{socket = Sock}) ->
   try gen_tcp:close(Sock)
   catch _:_ -> ok
   end.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

%% 发送失败回复的处理。
%% 默认选项下 TCP 发送只会回 closed / enotconn —— 都是"连接已经没了",停掉进程是对的;
%% 而且这两个错误同时也会从 recv 那边报一遍(driver 设了 TCP_ADDF_DELAYED_CLOSE_RECV,
%% 保证下一次 recv 拿到 {error,closed}),所以这里属于"早一步的重复信号"。
%% 只有 timeout 例外:那是 send_timeout 到点,不是连接故障,而且 OTP 是先
%% driver_enqv 入队、再回这条错误的 —— 数据还在队列里没发出去,这里一 shutdown
%% 就把它一起丢了。OTP 自己把"超时即关连接"做成 send_timeout_close 显式选项(默认 false),
%% 也说明超时不等于该死。
sendError(timeout, State) ->
   io:format("send timeout, connection kept for______ ~p~n", [timeout]),
   {noreply, State};
sendError(Reason, State) ->
   io:format("send error, Shutdown for ~p~n", [Reason]),
   shutdown(Reason, State).

shutdown(Reason, State) ->
   {stop, {shutdown, Reason}, State}.

