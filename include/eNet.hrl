-ifndef(eNet_H).
-define(eNet_H, true).

%% gen_tcp ready maybe to set sock options
%% ssl ready and then need do ntSslAcceptor:handshake/3 and maybe to set other options
%% ppt ready and then need do ntPptAcceptor:pptAndHS/5 and maybe to set other options
-define(mSockReady, mSockReady).

-define(DefTpOpts, [
   binary
   , {packet, 4}
   , {active, false}
   , {reuseaddr, true}
   , {nodelay, false}
   , {delay_send, true}
   , {send_timeout, 15000}
   , {keepalive, true}
   , {exit_on_close, true}
   , {back_log, 1024}
]).


-define(AptCnt, 16).
-define(DefSslHSTet, 15000).
-define(DefProxyPtTet, 5000).

-export_type([listenOpt/0]).
-type listenOpt() ::
   {aptCnt, non_neg_integer()} |
   {conMod, atom()} |
   {conArgs, atom()} |
   {tcpOpts, [gen_tcp:listen_option()]} |
   {sslOpts, [ssl:ssl_option()]} |
   {sslHSTet, timeout()} |
   {udpOpts, [gen_udp:option()]} |
   {proxyPt, boolean()} |
   {proxyPtTet, timeout()}.

%% 令牌桶相关定义
-record(tBucket, {
   rate :: pos_integer()               %% 速率
   , tokens :: non_neg_integer()       %% 剩余tokens数量
   , lastTime :: pos_integer()         %% 最后一次更新访问时间单位毫秒
   , bucketSize :: pos_integer()       %% 桶大小 可以容纳的令牌数量
}).

-endif.