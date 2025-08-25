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
	, {nodelay, true}               % 禁用Nagle 可以减少延迟(对于需要低延迟的游戏)
	, {delay_send, true}            % 提升吞吐量 尤其在高并发场景下，减少调度器争用 增加约10-50μs的延迟（取决于队列深度和调度频率）
	, {send_timeout, 15000}         % 发送超时时间
	, {send_timeout_close, true}    % 发送超时自动关闭连接（防止半开连接）
	, {keepalive, true}             % 检测死连接（默认间隔2小时，需系统级调整）
	, {exit_on_close, true}         % 当socket被关闭时，与其关联的控制进程（controlling process） 会收到{'EXIT', Port, Reason}信号 强制清理异常连接
	, {backlog, 1024}               % 等待连接队列长度
	, {buffer, 128 * 1024}         % 接收缓冲区1MB
	, {recbuf, 128 * 1024}         % 内核接收缓冲区 通常接受的数据比较小
	, {sndbuf, 512 * 1024}         % 内核发送缓冲区
	, {high_watermark, 32 * 1024}    % 32KB   Erlang内部Socket实现的数据队列  当队列数据量达到此阈值时，Socket标记为**繁忙（busy）**，发送进程会被挂起。
	, {low_watermark, 16 * 1024}     % 16KB  当队列数据量降低到此阈值时，Socket恢复**非繁忙状态**，允许继续发送。
	, {high_msgq_watermark, 64 * 1024} % 64KB Erlang进程消息队列**的繁忙状态 消息队列数据量达到此值时，队列标记为繁忙，阻止新消息进入
	, {low_msgq_watermark, 32 * 1024}  % 32KB 消息队列数据量低于此值时，恢复非繁忙状态
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