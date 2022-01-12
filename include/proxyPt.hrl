-ifndef(UT_PROXY_PT_H).
-define(UT_PROXY_PT_H, true).

%%--------------------------------------------------------------------
%% Proxy-Protocol Socket Wrapper
%%--------------------------------------------------------------------

-type(pp2_additional_ssl_field() :: {pp2_ssl_client, boolean()}
   | {pp2_ssl_client_cert_conn, boolean()}
   | {pp2_ssl_client_cert_sess, boolean()}
   | {pp2_ssl_verify, success | failed}
   | {pp2_ssl_version, binary()}    % US-ASCII string
   | {pp2_ssl_cn, binary()}         % UTF8-encoded string
   | {pp2_ssl_cipher, binary()}     % US-ASCII string
   | {pp2_ssl_sig_alg, binary()}    % US-ASCII string
   | {pp2_ssl_key_alg, binary()}    % US-ASCII string
).

-type(pp2_additional_field() ::
   {pp2_alpn, binary()}          % byte sequence
   | {pp2_authority, binary()}   % UTF8-encoded string
   | {pp2_crc32c, integer()}     % 32-bit number
   | {pp2_netns, binary()}       % US-ASCII string
   | {pp2_ssl, list(pp2_additional_ssl_field())}
).

-record(proxy_socket, {
   inet = inet4 :: inet4 | inet6 | 'unix' | 'unspec',
   src_addr = {0, 0, 0, 0} :: inet:ip_address() | undefined,
   dst_addr = {0, 0, 0, 0} :: inet:ip_address() | undefined,
   src_port = 0 :: inet:port_number() | undefined,
   dst_port = 0 :: inet:port_number() | undefined,
   %% Proxy protocol v2 addtional fields
   pp2_additional_info = [] :: list(pp2_additional_field())
}).

-endif.
