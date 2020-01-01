-define(nlTcpMgrSup, nlTcpMgrSup).
-define(nlSslMgrSup, nlSslMgrSup).
-define(nlUdpMgrSup, nlUdpMgrSup).
-define(nlDtlsMgrSup, nlDtlsMgrSup).

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
   {udpOpts, [gen_udp:option()]} |
   {dtlsOpts, [gen_udp:option() | ssl:ssl_option()]}).

-type(listenOn() :: inet:port_number() | {host(), inet:port_number()}).

%%--------------------------------------------------------------------
%% SSL socket wrapper
%%--------------------------------------------------------------------

-record(ssl_socket, {tcp :: inet:socket(), ssl :: ssl:sslsocket()}).

-define(IS_SSL(Sock), is_record(Sock, ssl_socket)).

%% 令牌桶相关定义
-record(tokenBucket, {
   rate :: pos_integer()               %% 速率
   , tokens :: non_neg_integer()       %% 剩余tokens数量
   , lastTime :: pos_integer()         %% 最后一次更新访问时间单位毫秒
   , bucketSize :: pos_integer()       %% 桶大小 可以容纳的令牌数量
}).

%%--------------------------------------------------------------------
%% Proxy-Protocol Socket Wrapper
%%--------------------------------------------------------------------

-export_type([listenOn/0]).

-type(proto() :: atom()).
-type(transport() :: module()).
-type(udp_transport() :: {udp | dtls, pid(), inet:socket()}).
-type(socket() :: esockd_transport:socket()).
-type(mfargs() :: atom() | {atom(), atom()} | {module(), atom(), [term()]}).
-type(sock_fun() :: fun((esockd_transport:socket()) -> {ok, esockd_transport:socket()} | {error, term()})).

-type(host() :: inet:ip_address() | string()).
-type(listen_on() :: inet:port_number() | {host(), inet:port_number()}).

-type(pp2_additional_ssl_field() :: {pp2_ssl_client,           boolean()}
                                  | {pp2_ssl_client_cert_conn, boolean()}
                                  | {pp2_ssl_client_cert_sess, boolean()}
                                  | {pp2_ssl_verify,  success | failed}
                                  | {pp2_ssl_version, binary()}  % US-ASCII string
                                  | {pp2_ssl_cn,      binary()}  % UTF8-encoded string
                                  | {pp2_ssl_cipher,  binary()}  % US-ASCII string
                                  | {pp2_ssl_sig_alg, binary()}  % US-ASCII string
                                  | {pp2_ssl_key_alg, binary()}).% US-ASCII string

-type(pp2_additional_field() :: {pp2_alpn,      binary()}  % byte sequence
                              | {pp2_authority, binary()}  % UTF8-encoded string
                              | {pp2_crc32c,    integer()} % 32-bit number
                              | {pp2_netns,     binary()}  % US-ASCII string
                              | {pp2_ssl,       list(pp2_additional_ssl_field())}).

-record(proxy_socket, {inet     :: inet4 | inet6 | 'unix' | 'unspec',
                       socket   :: inet:socket() | #ssl_socket{},
                       src_addr :: inet:ip_address() | undefined,
                       dst_addr :: inet:ip_address() | undefined,
                       src_port :: inet:port_number() | undefined,
                       dst_port :: inet:port_number() | undefined,
                       %% Proxy protocol v2 addtional fields
                       pp2_additional_info = [] :: list(pp2_additional_field())}).

-define(IS_PROXY(Sock), is_record(Sock, proxy_socket)).

