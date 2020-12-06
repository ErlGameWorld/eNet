-define(nlTcpMgrSup, nlTcpMgrSup).
-define(nlSslMgrSup, nlSslMgrSup).
-define(nlUdpMgrSup, nlUdpMgrSup).

-define(miSockReady, miSockReady).

-define(WARN(Tag, Format, Data), nlNetCom:warnMsg(Tag, Format, Data)).
-define(getListValue(Key, List, Default), nlNetCom:getListValue(Key, List, Default)).

-define(SSL_CLOSE_TIMEOUT, 5000).
-define(SSL_HANDSHAKE_TIMEOUT, 15000).
-define(PROXY_RECV_TIMEOUT, 5000).

-type(listenOpt() ::
   {acceptors, non_neg_integer()} |
   {tcpOpts, [gen_tcp:listen_option()]} |
   {sslOpts, [ssl:ssl_option()]} |
   {udpOpts, [gen_udp:option()]}).

-type(listenOn() :: inet:port_number() | {host(), inet:port_number()}).
-type(listenName() :: atom()).


%% 令牌桶相关定义
-record(tokenBucket, {
   rate :: pos_integer()               %% 速率
   , tokens :: non_neg_integer()       %% 剩余tokens数量
   , lastTime :: pos_integer()         %% 最后一次更新访问时间单位毫秒
   , bucketSize :: pos_integer()       %% 桶大小 可以容纳的令牌数量
}).

-type(conMod() :: module()).
-type(socket() :: esockd_transport:socket()).
-type(mfargs() :: atom() | {atom(), atom()} | {module(), atom(), [term()]}).
-type(sock_fun() :: fun((esockd_transport:socket()) -> {ok, esockd_transport:socket()} | {error, term()})).
-type(host() :: inet:ip_address() | string()).






