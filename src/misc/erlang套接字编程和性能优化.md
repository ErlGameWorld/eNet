#### erlang 各种 优化设置

     一、 erl启动时参数:
    +K true  开启epoll调度，在linux中开启epoll，会大大增加调度的效率
    +A 100   异步线程池，为某些port调用服
    +P 1024000  最大进程数
    +Q 65535 最大port数
    +sbt db 绑定调度器，绑定后调度器的任务队列不会在各个CPU线程之间跃迁，结合sub使用，可以让CPU负载均衡的同时也避免了大量的跃迁发生。
         将scheduler绑定到具体的cpu核心上，再配合erlang进程和port绑定，可以显著提升性能，但是如果绑定错误，反而会有反效果
         （ 进程调度器绑定：erlang:process_flag(scheduler, 1)，当进程使用了port时，还需要port绑定支持，防止进程在不同调度器间迁移引起性能损失，如cache、跨numa node拷贝等，当进程使用了port时，主要是套接字，若进程与port不在一个scheduler上，可能会引发严重的epoll fd锁竞争及跨numa node拷贝，导致性能严重下降）
    注意：一个linux系统中，最好只有一个evm开启此选项，若同时有多个erlang虚拟机在系统中运行，还是关闭为好
   
    +sub true  开启CPU负载均衡，false的时候是采用的CPU密集调度策略，优先在某个CPU线程上运行任务，直到该CPU负载较高为止。
    +swct eager  此选项设置为eager后，CPU将更频繁的被唤醒，可以增加CPU利用率
    +spp true  开启并行port并行调度队列，当开启后会大大增加系统吞吐量，如果关闭，则会牺牲吞吐量换取更低的延迟。
    +zdbbl 65536  分布式erlang的端口buffer大小，当buffer满的时候，向分布式的远程端口发送消息会阻塞
    
    二、erlang内部进程启动参数
    示例：创建一个新进程并进行注册,该进程是全局唯一的自增ID生成进程，因此无法做多进程处理，这个时候单进程的性能就是至关重要的
    首先，出于性能和功能考虑，这个进程不是gen_server；其次进行了部分参数调优能
    register(num_generator, spawn_opt(?MODULE, init, [],[{priority,high},{scheduler,0},{min_heap_size, 65536 * 2},{min_bin_vheap_size,65536 * 2}])).
    参数讲解:
    1.priority 
        erlang是公平调度策略，因此默认情况下每个进程得到的运行时间片是相同的：2000reductions,但是对于我们的应用场景来说，这个进程应该是优先级较高的，需要得到更多的调度，因此设置为high,还可以设置为max,但是max是系统进程的预留优先级，用high即可
    2. scheduler 
        将该进程绑定到指定的scheduler上，防止进程的任务被scheduler分配来分配去，可以减少CPU调用,注意这个和+sbt db是不同的，+sbt db是防治调度器的任务队列在CPU线程间跃迁，scheduler是为了防止进程在时间片切换过程中被分配给其它的调度器
    3.min_heap_size
        进程初始堆大小，用内存换CPU的典型做法，增大初始大小，可以显著降低GC次数和内存再分配次数， 减少处理过程中产生大量term，尤其是list时的gc次数
    4.min_bin_vheap_size
    进程初始二进制堆大小，当该进程对于binary数据的处理交换很多时，可以获得和增大min_heap_size一样的效果， 减少大量消息到达或处理过程中产生大量binary时的gc次数

     三、port(socket)调优
    示例：服务器监听端口，接受客户端请求。典型应用场景web服务器，需要实现高吞吐，低延迟的目标
    Res = gen_tcp:listen(Port, [binary,
    {reuseaddr, true},
    {nodelay, true},
    {delay_send,true},
    {high_watermark,64 * 1024},
    {send_timeout, 30000},
    {send_timeout_close, true},
    {keepalive, true}])
    
    参数详解:
    binary:
    接收到客户端的消息后，作为binary来处理，binary在erlang中是很高效的数据结构，超过64字节，就是全局保存的，因此在很多操作下是不需要复制的，仅仅复制binary的指针即可，详细请搜索refc binary，注意：binary大量使用需要有丰富的经验，不然可能会内存泄漏
    reuseaddr:
    允许系统复用port,对于高吞吐的系统，这个参数很重要,请搜索:linux port 复用
    nodelay:
    开启linux中的TCP_NODELAY参数，请搜索:TCP_NODELAY 40毫秒延迟
    delay_send:
    默认的erlang port消息发送，是直接发送，若失败则排队处理，然后由调度器进行队列poll操作，如果设置为true,那么就不尝试直接发送，而且扔进队列，等待poll，开启选项会增加一点点消息延迟，换来吞吐量的大量提升
    high_watermark:
    port的发送缓存，缓存满了后，下次发送会直接阻塞，直到缓存低于某个阈值low_watermark。如果是密集网络IO系统，请增大该buffer,避免发送阻塞
    send_timeout:
    在high_watermark中提到了发送阻塞，如果阻塞超过这个时间，那么就会超时，发送直接返回,停止发送
    send_timeout_close:
    如果发生了send_timeout同时设置了send_timeout_close选项，那么超时后，会直接关闭socket.如果发送进程不是很重要，例如web用户进程，强烈建议开启这个选项，当发送30秒超时的时候，就说明该用户出现了很大的麻烦，断开连接是最理想的做法，否则可能出现很多奇怪的bug.
    keepalive:
    遵循HTTP/1.1协议的keepalive规定,这个根据业务需求选择是否开启,如果同一个客户端会连续发起http请求，那么建议设置为true,避免多次TCP握手
    示例：服务器发起大量的http请求,在优化了参数后，同样的吞吐量所耗费的时间是未优化前的1/3 - 1/2(经过严苛的测试得出的数据)
    
    inets:start(),
       httpc:set_options([{max_keep_alive_length,500},{max_sessions,100},{nodelay,true},{reuseaddr,true}]),
    
    参数详解:
    max_keep_alive_length:  
    在同一条http连接上允许发送的最大包数，默认为5，超过5个包，就会重连
    max_sessions:
    跟目标服务器之间最大的并行http连接数目,大大的增加了数据上行吞吐量
    nodelay_true:
    见上文
    reuseaddr:
    
      6. 数据结构：
         减少遍历，尽量使用API提供的操作
         由于各种类型的变量实际可以当做c的指针，因此erlang语言级的操作并不会有太大代价
         lists：reverse为c代码实现，性能较高，依赖于该接口实现的lists API性能都不差，避免list遍历，[||]和foreach性能是foldl的2倍，不在非必要的时候遍历list
         dict：find为微秒级操作，内部通过动态hash实现，数据结构先有若干槽位，后根据数据规模变大而逐步增加槽位，fold遍历性能低下
         gb_trees：lookup为微秒级操作，内部通过一个大的元组实现，iterator+next遍历性能低下，比list的foldl还要低2个数量级
    9. 文件预读，批量写，缓存：
        这些方式都是局部性的体现：
        预读：读空间局部性，文件提供了read_ahead选项
        批量写：写空间局部性
          对于文件写或套接字发送，存在若干级别的批量写：
            1. erlang进程级：进程内部通过list缓存数据
            2. erlang虚拟机：不管是efile还是inet的driver，都提供了批量写的选项delayed_write|delay_send，
               它们对大量的异步写性能提升很有效
            3. 操作系统级：操作系统内部有文件写缓冲及套接字写缓冲
            4. 硬件级：cache等
        缓存：读写时间局部性，读写空间局部性，主要通过操作系统系统，erlang虚拟机没有内部的缓存
    10.套接字标志设置：
        延迟发送：{delay_send, true}，聚合若干小消息为一个大消息，性能提升显著
        发送高低水位：{high_watermark, 128 * 1024} | {low_watermark, 64 * 1024}，辅助delay_send使用，delay_send的聚合缓冲区大小为high_watermark，数据缓存到high_watermark后，将阻塞port_command，使用send发送数据，直到缓冲区大小降低到low_watermark后，解除阻塞，通常这些值越大越好，但erlang虚拟机允许设置的最大值不超过128K
        发送缓冲大小：{sndbuf, 16 * 1024}，操作系统对套接字的发送缓冲大小，在延迟发送时有效，越大越好，但有极值
        接收缓冲大小：{recbuf, 16 * 1024}，操作系统对套接字的接收缓冲大小

#### Erlang 虚拟机调优

    目录
    SMP
    Schedulers
    Port Settings
    Asynchronous Thread Pool
    Kernel Polling
    Warning Messages
    Process Limit
    Distribution Buffer
    Erlang Built-in Storage
    Crash Dumps
    Net Kernel Tick Time
    Shutdown Time
    Riak 是用Erlang语言写的,运行在Erlang虚拟机之上.所以Erlang虚拟机的调优对Riak的性能优化就显得尤为重要. Erlang虚拟机本身提供了非常多的配置参数对性能调优, Riak支持其中的一部分参数,你可以在每个node的Riak配置文件中进行设置.

下表列出了其中的一部分,左边一列是Erlang中的参数名称, 右边一列是在Riak中的参数名称. Erlang parameter Riak parameter +A erlang.async_threads +K erlang.K +P
erlang.process_limit +Q erlang.max_ports +S erlang.schedulers.total, erlang.schedulers.online +W erlang.W +a
erlang.async_threads.stack_size +e erlang.max_ets_tables +scl erlang.schedulers.compaction_of_load +sfwi
erlang.schedulers.force_wakeup_interval -smp erlang.smp +sub erlang.schedulers.utilization_balancing +zdbbl
erlang.distribution_buffer_size -kernel net_ticktime erlang.distribution.net_ticktime -env FULLSWEEP_AFTER
erlang.fullsweep_after -env ERL_CRASH_DUMP erlang.crash_dump -env ERL_MAX_ETS_TABLES erlang.max_ets_tables -name
nodename Note on upgrading to 2.0 在Riak2.0版本之前, Erlang虚拟机相关的参数放在配置文件 vm.args 里面. 在2.0及之后的版本中, 所有Erlang虚拟机相关的配置参数放在配置文件
riak.conf 里面. 如果你从Riak2.0之前的版本升级到Riak 2.0, 你仍然可以继续使用旧的配置文件 vm.args. 但是, 如果你同时设置了配置文件 vm.args 和riak.conf, 在
vm.args里面的配置将会覆盖riak.conf里面的配置.

##### SMP

    有些操作系统提供Erlang虚拟机对称多处理器能力(SMP)以利用多处理器硬件架构的优势. SMP的支持可以通过设置erlang.smp参数来打开和关闭, 默认是打开的. 下面的例子是关闭SMP的支持.
    riak.conf
    erlang.smp = disable
    由于Riak也可以运行在一些不支持SMP的操作系统上, 所以在使用之前需要确认操作系统是否支持SMP,如果操作系统本身不支持,那么需要在启动Riak集群之前在配置文件riak.conf中关闭SMP的选项.
    
    比较安全的一个选择是把erlang.smp设置成auto, 这个选项会指示Erlang虚拟机启动SMP支持之前检查操作系统是否支持以及是否有一个以上的逻辑处理器,只有这两个条件都满足的时候,Erlang虚拟机才启动SMP支持.

##### Schedulers

    Note on missing scheduler flags
    We recommend that all users set the +sfwi to 500 (milliseconds) and the +sclflag to false if using the older, vm.args-based configuration system. If you are using the new, riak.conf-based configuration system, the corresponding parameters are erlang.schedulers.force_wakeup_interval anderlang.schedulers.compaction_of_load.
    Please note that you will need to uncomment the appropriate lines in your riak.conf for this configuration to take effect.
    如果在Erlang虚拟机里已经打开了支持SMP的选项, 比如erlang.smp已经被设置成enabled 或者auto,而且机器本身超过一个逻辑处理器同时也支持SMP, 那么当你启动Riak的时候, 你可以配置逻辑处理器的数量或者调度线程的数量,同时也可以设置online线程的数量. 
    全部调度线程的数量可以通过参数erlang.schedulers.total来设置, online线程的数量则是通过参数erlang.schedulers.online来配置. 这两个参数可以分别对应到Erlang虚拟机的参数Schedulers 和SchedulersOnline.
    两个参数的最大值都是1024,  参数并没有统一的默认值. 但是, Erlang 虚拟机自己会尝试去判定有多少配置的CPU(core)和可用的CPU(core). 如果Erlang虚拟机能够做出这个判定,那么参数schedulers.total会默认设置成配置的CPU(core)数量,
    参数schedulers.online会默认设置成可用的CPU(core)数量. 但是, 如果Erlang虚拟机不能做出判定, 两个参数的默认值将会设置成1.
    如果两个参数中的任意一个被设置成负数, 那么意味着这个参数值将会被设成默认配置的处理器数量(如果scheduler.total是负数)或者可用的处理器数量(如果schedulers.online是负数) 减去配置的负值. 比如, 如果机器配置有100个cpu(cores)然后参数schedulers.total配置为-50, 计算以后的值就是50. 
    如果两个参数中的任意一个被设置为0,两个值都会被重新设为默认值.
    如果SMP支持被关闭, 比如erlang.smp被设成disabled或者设成auto 但是机器本身不支持SMP或者机器只有一个逻辑处理器,那么两个参数schedulers.total 和 schedulers.online都将会被忽略.

Scheduler Wakeup Interval 调度器唤醒是一个可选处理, 通过这个Erlang 虚拟机调度器被周期性的扫描来判定是否已经陷入睡眠, 比如是否调度器有一个空的运行列表.
这个扫描时间间隔可以通过参数erlang.schedulers.force_wakeup_interval设置, 单位为毫秒.这个参数对应于Erlang虚拟机的+sfwi选项.该参数默认设为0, 不激活调度器唤醒功能.
Erlang在R15Bx版本里有把调度器睡眠过于频繁的倾向,如果你使用的是更新的版本,比如Riak2.0 及以后, 那多数情况下不需要启动唤醒功能. 注:
OTP的工程师曾经解释过这个功能,如果需要调度的任务不是很多,没有很多task在运行列表上的话, R15B的Erlang虚拟机会倾向于把这些task尽量集中到尽可能少的调度器上来调度, 睡眠没有调度任务的调度器,
这样可以减少调度器之间的通信花费overhead, 提高CPU的利用率. 但这个也是一个trade off, 具体还是需要用户来根据自己的实际环境来调优.
因为一旦task的数量增加比较多,或者task数量没有增加但是task本身比较耗时,那么很可能就会触发调度器的唤醒, 而唤醒调度器是比较expensive的操作, 如果频繁睡眠唤醒的话,可能会得不偿失.

##### Scheduler Compaction and Balancing

    Erlang调度器提供了两种方式来分发负载到不同的调度器上, 集中负载和utilization balancing.
    集中负载是默认打开的, 打开的时候Erlang虚拟机会尝试去尽可能多的使调度器繁忙,比如通过把任务集中到有限的几个调度器上(假设这几个有限的调度器充分运行的情况下可以调度完目前的tasks)使这几个调度器一直有工作做(not run out of work). 为了达到这个目的, 当虚拟机分配任务的时候会考虑哪些调度器应该被分配任务. 用户可以设置参数erlang.schedulers.compaction_of_load为false来关闭这个功能.
    另外一个选项, utilization balancing, 为了支持负载平衡, 默认是关闭的. 如果打开了这个选项, Erlang虚拟机则努力在不同调度器之间平衡调度器的利用. 如果不考虑每个调度器没有任务可调度的频度的话, 可以打开这个设置, erlang.schedulers.utilization_balancing 设为true(老版本里面通过设置+scl false)
    在任何时候, 只可以是使用两个功能中的一个. 如果同时设置这两个选项为false的话, Riak 会默认使用集中负载选项.如果同时设置为true, Riak会使用那个在配置文件riak.conf中最先出现的那个.(如果是旧版本的话,配置文件会是vm.args)

##### Port Settings

Riak 使用epmd, Erlang 端口映射Daemon来进行大多数的节点间的通信. 在这个系统里, 集群里的其他节点使用由nodename参数(或者是name in vm.args)来作为节点ID. 比如,
riak@10.9.8.7. 在每个节点上, daemon把这些节点ID解析成一个TCP的端口. 用户可以指定一个端口范围给Riak节点来监听使用,同时也可以知道最大数量的并ports/sockets. Port Range 默认情况下 ,
epmd绑定到TCP端口4369上并且侦听通配符接口. epmd 默认使用一个不能预测的端口作为节点间的通信, 通过绑定到端口0上， 意味着会使用第一个可用的端口. 这样就使得防火墙非常难配置. 为了是防火墙配置简化,
用户可以指导Erlang虚拟机使用一个有限范围的端口或者单一端口. 这个最小和最大值可以设置在参数erlang.distribution.port_minimum和erlang.distribution.port_maximum里面.
比如, 下面的值被设为3000和5000. riak.conf app.config erlang.distribution.port_range.minimum = 3000
erlang.distribution.port_range.maximum = 5000 用户可以设置Erlang虚拟机使用一个单一端口, 如果只设置了最小值没有设置最大值,则表示使用单一端口. 比如, 下面设置使用单一端口5000.
riak.conf app.config erlang.distribution.port_range.minimum = 5000 如果最小端口没有设置, Erlang虚拟机将会在随机的高编号端口上侦听.

##### Maximum Ports

用户可以通过设置参数erlang.max_ports来指定Erlang虚拟机可以使用的最大并发的 ports/sockets数量, 范围从1024到134217727. 默认值是65536. 在vm.args里面对应的参数是+Q
或者-env ERL_MAX_PORTS. Asynchronous Thread Pool 如果Erlang虚拟机支持线程可用, 用户可以为Erlang虚拟机设置异步线程池的线程数量, 使用参数erlang.async_threads(
+A in vm.args). 线程数量范围从0至1024, 默认值是64,下面的例子是设置成600的情况. riak.conf vm.args erlang.async_threads = 600

##### Stack Size

除了可以指定异步线程的数量之外, 用户还可以为每个异步线程指定stack size. 参数是erlang.async_threads.stack_size, 对应到Erlang的+a参数. 用户可以在Riak中为这个参数指定size以KB,
MB,GB 为单位, 有效的范围值是16至8192个字, 在32位的系统上就是64至32768字节. 该参数没有默认值, 我们建议设置为16K words, 对应为64 KB在32位系统上.
我们建议这么小一个值是考虑到异步线程数量可能会很大. 注:The 64 KB default is enough for drivers delivered with Erlang/OTP but might not be large
enough to accommodate drivers that use the driver_async()functionality, documented here. We recommend setting higher
values with caution, always keeping the number of available threads in mind. Kernel Polling 如果系统支持, 用户可以在Erlang中利用内核轮询.
内核轮询可以在使用很多文件描述符的时候提高性能. 在使用中的文件描述符越多, 内核轮询发挥的作用就越大. 该选择在Riak的Erlang虚拟机中是默认打开的, 该参数对应到Erlang虚拟机中的+K参数

##### Warning Messages

Erlang虚拟机的error_logger 是一个事件管理器, 从Erlang运行时系统注册错误, 告警和信息事件. 默认情况下, error_logger的信息事件被映射为告警,但是用户可以设置映射成错误或者信息.
该设置为参数erlang.W, 可以设置的值为w(warning), errors 或者i(info reports).

##### Process Limit

参数erlang.process_limit可以用来设置系统同时存在的最大进程数量(对应到Erlang的+P参数), 有效范围从1024至134217727. 默认值是256000.

##### Distribution Buffer

用户可以通过参数erlang.distribution_buffer_size设置Erlang虚拟机的distribution buffer busy limit(对应到Erlang的+zdbbl参数). 修改这个参数对那些有许多busy
dist port事件的节点可能会有帮助, 默认值是32MB, 最大值是2097151KB. 增大这个参数可以允许进程缓存更多的待发消息, 当缓存满的时候，发送线程被挂起直到缓存减小到设定值. 所以,
更大的缓存有助于降低延迟以及增加吞吐量,代价就是使用了更多的RAM. 用户需要根据机器的RAM资源来考虑设定这个值.

##### Erlang Built-in Storage

Erlang使用一个内置的数据库,ets(Erlang Term Storage)用来快速访问内存(constant access time rather than logarithmic access time). erts
表的最大数量设置在参数erlang.max_erts_tables里面, 默认值是256000,这个值要大于Erlang虚拟机自身的默认值1400(对应到vm.args 的参数e).
更大的erlang.max_erts_tables值可以提供更快的数据访问,代价是消耗更高的内存.

##### Crash Dumps

默认情况下, Riak 的Erlang crash dumps文件是存放在位置./log/erl_crash.dump. 用户可以通过设置参数erlang.crash_dump来更改存放位置.
该参数对应到Erlang虚拟机的ERL_CRASH_DUMP环境变量.

##### Net Kernel Tick Time

网络内核是Erlang的一个系统进程, 提供了不同的网络监视形式. 在一个Riak集群里面, 网络内核的功能之一就是去周期性的检测节点存活. Tick time就是这个检查频度,
可以通过erlang.distribution.net_ticktime设置,单位是秒. 该参数对应到vm.args里面的参数-kernal net_ticktime.

##### Shutdown Time

用户可以设定Erlang虚拟机的关闭时间, 该设置参数为erlang.shutdown_time,默认是10秒, 一旦10秒过了, 所有存在的进程就会被杀掉. 减少关闭时间在某些情景下可能是有帮助的,
比如说在测试的时候需要频繁的启停Riak集群. 在vm.args里参数是shutdown_time, 单位是毫秒.

gen_tcp 编程接口

#### listen(Port, Options) -> {ok, ListenSocket} | {error, Reason}

    Types
    Port = inet:port_number()
    Options = [listen_option()]
    ListenSocket = socket()
    Reason = system_limit | inet:posix()
    设置一个套接字以侦听本地主机上的端口Port。
    
    用法：
    listen(Port, Options) -> {ok, ListenSocket} | {error, Reason}
    在本地开启一个监听某个端口的套接字（socket）。开启成功的话，会返回一个套接字标识符 Socket，其一般会传递给 get_tcp:accept/1 或 get_tcp:accept/2 调用。
    
    如果参数 Port 为 0，那么底层操作系统将赋值一个可用的端口号，可以使用 inet:port/1 来获取一个 socket 监听的端口。
    
    连接到IP地址为Address的主机的TCP端口Port上的服务器。参数 地址可以是主机名或IP地址。
    提供以下选项：
    {ip, Address}
    如果主机有许多网络接口，则此选项指定要使用的接口。
    {ifaddr, Address}
    与{ip，Address}相同。如果主机有许多网络接口，则此选项指定要使用的接口。
    {fd, integer() >= 0}
    如果以某种方式未使用gen_tcp连接了套接字 ，请使用此选项传递文件描述符。如果将{ip，Address}和/或 {port，port_number（）}与该选项结合使用，则 在连接前将fd绑定到指定的接口和端口。如果未指定这些选项，则假定fd已被适当绑定。
    inet
    为IPv4设置套接字。
    inet6
    设置用于IPv6的套接字。
    local
    设置Unix域套接字。见 inet：local_address（）
    {port，Port}
    指定要使用的本地端口号。
    
    {tcp_module, module()}
    覆盖使用哪个回调模块。默认为 inet_tcp IPv4和inet6_tcp使用IPv6。
    Opt
    参见 inet：setopts / 2。
    
    可以使用send / 2将数据包发送到返回的套接字Socket。 从对等方发送的数据包将作为消息传递：
    {tcp, Socket, Data}
    
    如果套接字处于{active，N}模式（有关详细信息，请参见inet：setopts / 2），并且其消息计数器降至0，则将传递以下消息以指示套接字已转换为被动（{active，false}） 模式：
    {tcp_passive, Socket}
    
    如果套接字已关闭，则会发出以下消息：
    {tcp_closed, Socket}
    
    如果套接字上发生错误，则会传递以下消息（除非在套接字的选项列表中指定了{active，false}，在这种情况下，可通过调用recv / 2来检索数据包）：
    {tcp_error, Socket, Reason}
    
    可选的Timeout参数指定超时（以毫秒为单位）。默认为infinity。
    注意：：：
        请记住，如果底层OS connect（）的调用返回超时，调用gen_tcp：连接也将返回超时（即{错误，ETIMEDOUT} ），即使较大的超时指定。
        指定要连接的选项的默认值会受到内核配置参数 inet_default_connect_options的影响。有关详细信息，请参见 inet（3）。
        
    参数 Options 的一些常用选项：
  
    {active, true}：套接字设置为主动模式。所有套接字接收到的消息都作为 Erlang 消息转发到拥有这个套接字进程上。当开启一个套接字时，默认是主动模式。
    {active, false}：设置套接字为被动模式。套接字收到的消息被缓存起来，进程必须通过调用函数 gen_tcp:recv/2 或 gen_tcp:recv/3 来读取这些消息。
    {active, once}：将设置套接字为主动模式，但是一旦收到第一条消息，就将其设置为被动模式，并使用 gen_tcp:recv/2 或 gen_tcp:recv/3 函数来读取后续消息。
    {keepalive, true}：当没有转移数据时，确保所连接的套接字发送保持活跃（keepalive）的消息。因为关闭套接字消息可能会丢失，如果没有接收到保持活跃消息的响应，那么该选项可确保这个套接字能被关闭。默认情况下，该标签是关闭的。
    {nodelay, true}：数据包直接发送到套接字，不过它多么小。在默认情况下，此选项处于关闭状态，并且与之相反，数据被聚集而以更大的数据块进行发送。
    {packet_size, Size}：设置数据包允许的最大长度。如果数据包比 Size 还大，那么将认为这个数据包无效。
    {packet, 0}：表示 Erlang 系统会把 TCP 数据原封不动地直接传送给应用程序
    {reuseaddr, true}：允许本地重复使用端口号
    {delay_send, true}：数据不是立即发送，而是存到发送队列里，等 socket 可写的时候再发送
    {backlog, 1024}：缓冲区的长度
    {exit_on_close, false}：设置为 flase，那么 socket 被关闭之后还能将缓冲区中的数据发送出去
    {send_timeout, 15000}：设置一个时间去等待操作系统发送数据，如果底层在这个时间段后还没发出数据，那么就会返回 {error,timeout}
    
    {Rand, _RandSeed} = random:uniform_s(9999, erlang:now()),  
    Port = 40000 + Rand,  
    gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}]).  

#### accept(ListenSocket) -> {ok, Socket} | {error, Reason} accept(ListenSocket, Timeout) -> {ok, Socket} | {error, Reason}

    Types
    ListenSocket = socket() Returned by listen/2.
    Timeout = timeout() Socket = socket() Reason = closed | timeout | system_limit | inet:posix()
    在侦听套接字上接受传入的连接请求。 套接字必须是从listen / 2返回的套接字 。 超时以毫秒为单位指定超时值。默认为infinity。
    
    返回值：
    {ok, Socket} if a connection is established
    {error, closed} if ListenSocket is closed
    {error, timeout} if no connection is established within the specified time
    {error, system_limit} if all available ports in the Erlang emulator are in use
    A POSIX error value if something else goes wrong, see inet(3) for possible error values
    
    用法：
    该函数会引起进程阻塞，直到有一个连接请求发送到监听的套接字。
    
    如果连接已建立，则返回 {ok，Socket}；
    或如果 ListenSocket 已经关闭，则返回{error，closed}；
    或如果在指定的时间内连接没有建立，则返回{error，timeout}；
    或如果 Erlang 虚拟机里可用的端口都被使用了，则返回 {error, system_limit}；
    如果某些东西出错，也可能返回一个 POSIX 错误。一些有可能的错误请查看 inet 模块的相关说明。
    
    使用 gen_tcp:send/2 向该函数返回的套接字 Socket 发送数据包。往端口发送的数据包会以下面格式的消息发送：
    {tcp, Socket, Data}
    如果在建立套接字 Socket 的时候选项列表中指定了 {active，false}，这样就只能使用 gen_tcp:recv/2 或 gen_tcp:recv/3 来接收数据包了。
    
    {Rand, _RandSeed} = random:uniform_s(9999, erlang:now()),
    Port = 40000 + Rand,
    case gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}]) of
        {ok, ListenSocket} ->
            case gen_tcp:accept(ListenSocket) of
                {ok, Socket} ->
                    Socket;
                {error, SocketAcceptFail} ->
                    SocketAcceptFail
            end;
        _ ->
            socket_listen_fail
    end.

#### connect(Address, Port, Options) -> {ok, Socket} | {error, Reason}

#### connect(Address, Port, Options, Timeout) ->  {ok, Socket} | {error, Reason}

    Types 
    Address = inet:socket_address() |
    inet:hostname() Port = inet:port_number()
    Options = [connect_option()]
    Timeout = timeout()
    Socket = socket()
    Reason = timeout | inet:posix()
    连接一个 TCP 端口
    
    用法：
    connect(Address, Port, Options, Timeout) -> {ok, Socket} | {error, Reason}
    用给出的端口 Port 和 IP 地址 Address 连接到一个服务器上的 TCP 端口上。参数 Address 即可以是一个主机名，也可以是一个 IP 地址。
    提供以下选项：
    {ip, Address}
    如果主机有许多网络接口，则此选项指定要使用的接口。
    {ifaddr, Address}
    与{ip，Address}相同。如果主机有许多网络接口，则此选项指定要使用的接口。
    {fd, integer() >= 0}
    如果以某种方式未使用gen_tcp连接了套接字 ，请使用此选项传递文件描述符。如果将{ip，Address}和/或 {port，port_number（）}与该选项结合使用，则 在连接前将fd绑定到指定的接口和端口。如果未指定这些选项，则假定fd已被适当绑定。
    inet
    为IPv4设置套接字。
    inet6
    设置用于IPv6的套接字。
    local
    设置Unix域套接字。见 inet：local_address（）
    {port，Port}
    指定要使用的本地端口号。
    
    {tcp_module, module()}
    覆盖使用哪个回调模块。默认为 inet_tcp IPv4和inet6_tcp使用IPv6。
    Opt
    参见 inet：setopts / 2。
    
    可以使用send / 2将数据包发送到返回的套接字Socket。 从对等方发送的数据包将作为消息传递：
    {tcp, Socket, Data}
    
    如果套接字处于{active，N}模式（有关详细信息，请参见inet：setopts / 2），并且其消息计数器降至0，则将传递以下消息以指示套接字已转换为被动（{active，false}） 模式：
    {tcp_passive, Socket}
    
    如果套接字已关闭，则会发出以下消息：
    {tcp_closed, Socket}
    
    如果套接字上发生错误，则会传递以下消息（除非在套接字的选项列表中指定了{active，false}，在这种情况下，可通过调用recv / 2来检索数据包）：
    {tcp_error, Socket, Reason}
    
    可选的Timeout参数指定超时（以毫秒为单位）。默认为infinity。
    注意：：：
        请记住，如果底层OS connect（）的调用返回超时，调用gen_tcp：连接也将返回超时（即{错误，ETIMEDOUT} ），即使较大的超时指定。
        指定要连接的选项的默认值会受到内核配置参数 inet_default_connect_options的影响。有关详细信息，请参见 inet（3）。

#### gen_tcp:close/1

    Types
     Socket = socket()
     关闭一个 TCP 套接字 
     请注意，在大多数TCP实现中，执行关闭操作并不能保证在远程端检测到关闭之前，已发送的任何数据都会传递给接收方。如果要保证将数据传递给收件人，可以通过两种常用方法来实现。
      使用gen_tcp：shutdown（Sock，write）发出信号，表明不再发送任何数据，并等待套接字的读取端关闭。
      使用套接字选项{packet，N}（或类似的选项）可以使接收器在知道已接收到所有数据时关闭连接。

#### recv(Socket, Length) -> {ok, Packet} | {error, Reason}

#### recv(Socket, Length, Timeout) -> {ok, Packet} | {error, Reason}

    Types
    Socket = socket()
    Length = integer() >= 0
    Timeout = timeout()
    Packet = string() | binary() | HttpPacket
    Reason = closed | timeout | inet:posix()
    HttpPacket = term() 看到的描述 HttpPacket中 的erlang：decode_packet / 3 在ERTS。
    
    在被动模式下从套接字接收数据包。
    返回值{error，closed}指示关闭的套接字。
    Argument Length is only meaningful when the socket is in raw mode and denotes the number of bytes to read.
    参数 Length 仅在套接字处于 raw mode 时才有意义，它表示要读取的字节数。
     
     Length为0，则返回所有可用字节。
     如果Length > 0，则返回确切的 Length字节，否则返回错误;
     从另一侧关闭套接字时，可能会丢弃少于长度字节的数据
    可选的Timeout参数指定超时（以毫秒为单位）。默认为infinity。
    
    用法：
    recv(Socket, Length, Timeout) -> {ok, Packet} | {error, Reason}
    这个函数是从一个被动模式的套接字接受一个数据包。如果返回一个 {error, closed} 的返回值，那表明 Socket 已经关闭。
    
    当 Socket 是 raw 模式下，参数 Length 才有意义的，并且 Length 表示接收字节的大小。如果 Length = 0，所有有效的字节数据都会被接收。如果 Length > 0，则只会接收 Length 长度的字节，或发生错误；当另一端 Socket 关闭时，接收的数据长度可能会小于 Length。
    
    选项 Timeout 是一个以毫秒为单位的超时值，默认值是 infinity。
    
    {Rand, _RandSeed} = random:uniform_s(9999, erlang:now()),
    Port = 40000 + Rand,
    case gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}]) of
        {ok, ListenSocket} ->
            case gen_tcp:accept(ListenSocket) of
                {ok, Socket} ->
                    gen_tcp:recv(Socket, 0, 5000);
                {error, SocketAcceptFail} ->
                    SocketAcceptFail
            end;
        _ ->
            socket_listen_fail
    end.

#### send(Socket, Packet) -> ok | {error, Reason}

    Types
        Socket = socket()
        Packet = iodata()
        Reason = closed | inet:posix()
    在一个套接字 Socket 发送一个数据包
    用法：
    send(Socket, Packet) -> ok | {error, Reason}
    在一个套接字 Socket 发送一个数据包。

#### shutdown(Socket, How) -> ok | {error, Reason}

    Types
        Socket = socket()
        How = read | write | read_write
        Reason = inet:posix()
    在一个或两个方向上关闭socket
   
    以某种方式半关闭一个套接字。
    
    如果参数 How 为 write 的形式，则套接字 socket 会关闭数据写入，读取仍可以正常执行。
    如果How == read或Socket端口没有缓冲传出数据，则套接字将立即关闭，并且Reason中将返回遇到的任何错误。
    要实现套接字半打开， 那么套接字要设置 {exit_on_close, false} 这个参数。
    如果套接字端口中缓冲了数据，则将尝试关闭套接字的操作推迟到该数据写入内核套接字发送缓冲区中为止。
    如果遇到任何错误，则关闭套接字，并在下一个recv / 2或 send / 2上返回 {error，closed}。
    
    如果对等方在写端执行了关闭操作，则选项{exit_on_close，false}很有用。

#### gen_tcp:controlling_process/2

    改变一个套接字的控制进程
    
    将新的控制过程Pid分配给 Socket。控制过程是从套接字接收消息的过程。
    如果由当前控制进程以外的任何其他进程调用， 则返回{error，not_owner}。
    如果由Pid标识的进程不是现有的本地pid， 则返回{error，badarg}。
    在某些情况下，在执行此函数期间关闭Socket时，也可能返回{error，badarg}。

    如果套接字设置为活动模式，则此功能会将呼叫者邮箱中的所有消息传送到新的控制进程。
    如果在传输过程中有任何其他进程正在与套接字交互，则传输可能无法正常进行，并且消息可能会保留在呼叫者的邮箱中。
    例如，在传输完成之前更改套接字活动模式可能会导致此情况  

#### 套接字选项

    {active, true | false | once | -32768..32767} |
        如果值为true，这是默认值，则将从套接字接收的所有内容作为消息发送到接收进程。
        如果值为false（被动模式），则该进程必须通过调用gen_tcp：recv / 2,3， gen_udp：recv / 2,3或gen_sctp：recv / 1,2来显式接收传入的数据 （取决于套接字的类型） ）。
        如果该值为一次（{active，once}）， 则套接字中的一条数据消息将发送到该进程。要接收更多消息， 必须使用选项{active，一次}再次调用 setopts / 2。
        
        如果该值是-32768到32767（含）之间的整数N，则将该值添加到发送到控制进程的套接字的数据消息计数中。
         套接字的默认消息计数为0。如果指定了负值，并且其大小等于或大于套接字的当前消息计数，则套接字的消息计数将设置为0。
         一旦套接字的消息计数达到0，则可能是由于 向进程发送接收到的数据消息或通过显式设置该消息，
         然后通过特定于套接字类型的特殊消息通知该进程套接字已进入被动模式。 一旦套接字进入被动模式，为了接收更多消息，
         必须再次调用setopts / 2才能将套接字设置回主动模式。
        如果该值是-32768到32767（含）之间的整数N，则将该值添加到发送到控制进程的套接字的数据消息计数中。套接字的默认消息计数为0。
        如果指定了负值，并且其大小等于或大于套接字的当前消息计数，则套接字的消息计数将设置为0。一旦套接字的消息计数达到0，
        要么是由于向进程发送接收到的数据消息，要么是因为已显式设置它，然后通过特定于套接字类型的特殊消息通知该进程该套接字已进入被动模式。
        一旦套接字进入被动模式，为了接收更多消息，必须再次调用setopts / 2才能将套接字设置回主动模式。
         使用{active，一次}或{active，N}时，套接字在接收到数据时会自动更改行为。与面向连接的套接字（即gen_tcp）结合使用时，
         可能会造成混淆，因为具有{active，false}行为的套接字报告的关闭方式与具有{active，true} 行为的套接字关闭的方式不同。为了简化编程，
         当套接字在{active，false}模式下被关闭且对等方关闭时， 在设置为{active，一旦}时仍会生成消息 {tcp_closed，Socket }，
          {active，true}或{active，N}模式。因此可以肯定地假设，当套接字在{active，true}和 {active，false}模式之间来回切换时，
          消息 {tcp_closed，Socket}可能最终会出现套接字端口终止（取决于选项exit_on_close）。
          但是， 当检测到对等关闭时，完全取决于基础的TCP / IP堆栈和协议。
         注意{active，true}模式不提供流量控制；快速的发送者可以轻松地使接收者的传入消息溢出。对于 {active，N}模式，消息数大于零时也是如此。
         仅当高级协议提供自己的流控制（例如，确认收到的消息）或交换的数据量很少时，才使用活动模式。{active，false} 模式，
         使用{active，一旦}模式或{active，N} 模式（具有适用于应用程序的N值）提供流量控制。另一端发送的速度不能超过接收器可以读取的速度。
    
    {broadcast, Boolean} (UDP sockets)
        启用/禁用发送广播的权限。
    {buffer, integer() >= 0} |
        驱动程序使用的用户级缓冲区的大小。不要与sndbuf 和recbuf选项混淆，它们与内核套接字缓冲区相对应。对于TCP，建议使用val（buffer）> = val（recbuf），
        以避免由于不必要的复制而导致的性能问题。对于UDP，适用相同的建议，但最大值不应大于网络路径的MTU。 
        设置recbuf时，val（buffer）会自动设置为上述最大值。但是，为Recbuf设置的大小 通常变大，建议您使用 getopts / 2 来分析操作系统的行为。
         请注意，这也是从单个recv调用可以接收的最大数据量。如果您使用的MTU高于正常值，请考虑将缓冲区设置为更高。
    {delay_send, boolean()} |
        通常，当Erlang进程发送到套接字时，驱动程序会尝试立即发送数据。如果失败，驱动程序将使用任何可用方法将要发送的消息排队，
        只要操作系统表示可以处理该消息。设置{delay_send，true} 会使所有消息排队。这样，发送到网络的消息就更大，
        但更少。该选项将影响发送请求与Erlang进程的调度，而不是更改套接字的任何实际属性。该选项是特定于实现的。默认为false。
    {deliver, port | term} |
        当{active，true}时，数据在以下端口上传递 {S, {data, [H1,..Hsz | Data]}} or term : {tcp, S, [H1..Hsz | Data]}.
    {dontroute, boolean()} |
        启用/禁用传出消息的路由旁路
    {exit_on_close, boolean()} |
        默认情况下，此选项设置为true。
        将其设置为false的唯一原因是，如果要在检测到关闭后继续向套接字发送数据，例如，如果对等方使用 gen_tcp：shutdown / 2 关闭写端。
    {header, integer() >= 0} |
        仅当创建套接字时指定了选项binary 时，此选项才有意义。如果指定了选项 header，
        则从套接字接收的数据的第一个 Size Number字节是列表的元素，其余数据是指定为同一列表尾部的二进制文件。例如，如果Size == 2，则接收到的数据与[Byte1，Byte2 | Binary]匹配 
    {high_msgq_watermark, integer() >= 1} |
        当消息队列上的数据量达到此限制时，套接字消息队列将设置为繁忙状态。请注意，此限制仅涉及尚未达到ERTS内部套接字实现的数据。默认为8 kB。
         如果套接字消息队列繁忙或套接字本身繁忙，则挂起套接字的数据发送器。
         有关更多信息，请参见选项low_msgq_watermark， high_watermark和low_watermark。
         Notice that distribution sockets disable the use of high_msgq_watermark and low_msgq_watermark. Instead use the distribution buffer busy limit, which is a similar feature.
    {high_watermark, integer() >= 0} |
        当ERTS套接字实现在内部排队的数据量达到此限制时，将套接字设置为繁忙状态。默认为8 kB。
         如果套接字消息队列繁忙或套接字本身繁忙，则挂起套接字的数据发送器。
         有关更多信息，请参见选项low_watermark， high_msgq_watermark和low_msqg_watermark。
    {ipv6_v6only, Boolean}
        限制套接字仅使用IPv6，禁止任何IPv4连接。这仅适用于IPv6套接字（选项inet6）。
        在大多数平台上，必须先在套接字上设置此选项，然后才能将其与地址关联。因此，仅在创建套接字时指定它，而在调用包含此描述的函数（setopts / 2）时不使用它是合理的。
        将此选项设置为true的套接字的行为 是唯一可移植的行为。现在，FreeBSD不建议使用IPv6的初衷是将IPv6用于所有流量（您可以使用 {ipv6_v6only，false}来覆盖建议的系统默认值），但OpenBSD（受支持的GENERIC内核）禁止使用，并且在Windows（具有单独的IPv4和IPv6协议栈）。大多数Linux发行版的系统默认值仍为false。逐渐改变了操作系统之间从IPv4流量中分离IPv6流量的策略，因为逐渐证明，要确保正确，安全地实现双堆栈实施是困难而复杂的。
        在某些平台上，此选项唯一允许的值为true，例如OpenBSD和Windows。在这种情况下，尝试在创建套接字时将此选项设置为false会失败。
        在不存在的平台上设置此选项将被忽略。使用getopts / 2获取此选项 不会返回任何值，即返回的列表不包含 {ipv6_v6only，_}元组。在Windows上，该选项不存在，但会将其模拟为值为true的只读选项。
        因此， 在创建套接字时将此选项设置为true永远不会失败，除非可能是在您已将内核自定义为仅允许false的平台上进行，但在OpenBSD上这是可行的（但尴尬）。
        如果使用getopts / 2读回选项值 而没有获取任何值，则该选项在主机操作系统中不存在。IPv6和IPv4套接字在同一端口上侦听的行为以及获取IPv4流量的IPv6套接字的行为不再可预测。     
    {keepalive, boolean()} |
        没有其他数据交换时，启用/禁用连接的套接字上的定期传输。如果另一端没有响应，则认为连接已断开，并且将错误消息发送到控制过程。默认为禁用。
    {linger, {boolean(), integer() >= 0}} |
        确定在close / 1套接字调用中刷新未发送数据的超时（以秒为单位）。
        第一个组件是如果启用了延迟，第二个组件是刷新超时（以秒为单位）。有3种选择：
        {false，_}
        close / 1或shutdown / 2会立即返回，而不是等待刷新数据，而在后台进行关闭。
        {true，0}
        关闭连接时中止连接。丢弃仍保留在发送缓冲区中的所有数据，并将RST发送给对等方。
        这避免了TCP的TIME_WAIT状态，但是使创建该连接的另一个“化身”成为可能。
        当时间> 0时，{true，时间}
        在成功发送了套接字的所有排队消息或达到了超时（时间）之前，close / 1或shutdown / 2不会返回。
    {low_msgq_watermark, integer() >= 1} |
        如果套接字消息队列处于繁忙状态，则当消息队列中排队的数据量低于此限制时，套接字消息队列将设置为不繁忙状态。请注意，此限制仅涉及尚未达到ERTS内部套接字实现的数据。默认为4 kB。
        当套接字消息队列和套接字不繁忙时，将恢复由于繁忙的消息队列或繁忙的套接字而挂起的发件人。
        有关更多信息，请参见选项high_msgq_watermark， high_watermark和low_watermark。
        请注意，分发套接字禁止使用 high_msgq_watermark和low_msgq_watermark。而是使用 分配缓冲区繁忙限制，这是一个类似功能。
    {low_watermark, integer() >= 0} |
        如果套接字处于繁忙状态，则当ERTS套接字实现在内部排队的数据量低于此限制时，会将套接字设置为不繁忙状态。默认为4 kB。
        当套接字消息队列和套接字不繁忙时，将恢复由于繁忙的消息队列或繁忙的套接字而挂起的发件人。
        有关更多信息，请参见选项high_watermark， high_msgq_watermark和low_msgq_watermark
    {mode, list | binary} |
        接收到的数据包按照list或者binary的定义进行传递。
    list |
        接收到的数据包以列表形式发送。
    binary |
        接收到的数据包以二进制形式传送
    {bind_to_device，Ifname :: binary（）}
        将套接字绑定到特定的网络接口。必须在创建套接字的函数调用中使用此选项，即 gen_tcp：connect / 3,4， gen_tcp：listen / 2， gen_udp：open / 1,2或 gen_sctp：open / 0,1,2。
        与getifaddrs / 0不同，Ifname编码为二进制。如果系统在网络设备名称中使用非7位ASCII字符（这种情况不太可能发生），则在对该参数进行编码时必须格外小心。
        此选项使用特定于Linux的套接字选项 SO_BINDTODEVICE，例如在Linux内核2.0.30或更高版本中，因此仅在针对此类操作系统编译运行时系统时才存在。
        在Linux 3.8之前，可以设置此套接字选项，但无法使用getopts / 2进行检索。从Linux 3.8开始，它是可读的。
        虚拟机还需要提升的特权，这些特权可以以超级用户身份运行，或者（对于Linux）具有CAP_NET_RAW能力 。
        此选项的主要用例是将套接字绑定到 Linux VRF实例。    
    {nodelay, boolean()} |
        {nodelay，布尔值}（TCP / IP套接字）
        如果Boolean == true， 则为套接字打开选项TCP_NODELAY，这意味着也会立即发送少量数据。
    {nopush，布尔型}（TCP / IP套接字）
        这相当于TCP_NOPUSH在BSD和TCP_CORK在Linux上。
        如果Boolean == true，则为套接字打开相应的选项，这意味着将累积少量数据，直到可用完整的MSS数据为止或关闭此选项。
        请注意，虽然OSX上提供了TCP_NOPUSH套接字选项，但其语义却大不相同（例如，取消设置它不会导致立即发送累积的数据）。因此，在OSX上有意忽略了nopush选项    
    {packet, 0 | 1 | 2 | 4 | raw | sunrm | asn1 | cdr | fcgi | line | tpkt | http | httph | http_bin | httph_bin} |
        raw | 0 没有包装。
        1 | 2 | 4 数据包包含一个标头，该标头指定了数据包中的字节数，然后是该字节数。标头长度可以是一个，
                两个或四个字节，并包含一个按big-endian字节顺序排列的无符号整数。每个发送操作都会生成标头，并且在每个接收操作上都会剥离标头。4字节的标头限制为2Gb。
        asn1 | cdr | sunrm | fcgi | tpkt | line
              这些数据包类型仅对接收有效。发送数据包时，应用程序有责任提供正确的标头。但是，在接收时，对于接收到的每个完整数据包，将一条消息发送到控制过程，并且类似地，对gen_tcp：recv / 2,3的每次调用都 返回一个完整数据包。标头未剥离。
              数据包类型的含义如下：
              asn1 -ASN.1 BER
              sunrm -Sun的RPC编码
              CDR -CORBA（GIOP 1.1）
              fcgi-快速CGI
              tpkt -TPKT格式[RFC1006]
              line-行模式，数据包以换行符结尾的行，比接收缓冲区长的行被截断      
        http | http_bin
        超文本传输​​协议。按照ERTS的erlang：decode_packet / 3中 描述的 HttpPacket格式返回数据包。被动模式下的套接字从gen_tcp：recv返回{ok，HttpPacket}， 而主动套接字发送诸如 {http，Socket，HttpPacket}之类的消息。
        httph | httph_bin
        通常不需要这两种类型，因为在读取第一行之后，套接字会在内部自动从http / http_bin切换到 httph / httph_bin。但是，有时可能有用，例如从分块编码中解析预告片      
    {packet_size, integer() >= 0} |
        设置数据包主体的最大允许长度。如果数据包头指示数据包的长度大于最大允许长度，则该数据包被视为无效。如果数据包头对于套接字接收缓冲区太大，则会发生相同的情况。
        对于面向行的协议（line，http *），选项packet_size还可以保证接受指定长度的行，并且由于内部缓冲区的限制，该行不会被视为无效。
    {line_delimiter, Char}(TCP/IP sockets)
         设置面向行的协议（line）的行定界字符。默认为$ \ n。   
    {priority, integer() >= 0} |
        在实现此功能的平台上设置SO_PRIORITY套接字级别选项。行为和允许范围在不同系统之间有所不同。该选项在未实现的平台上被忽略。请谨慎使用。
    {raw,Protocol :: integer() >= 0,OptionNum :: integer() >= 0, ValueBin :: binary()} |
    {read_packets，Integer}（UDP套接字）
        设置在数据可用时无需套接字就可以读取的最大UDP数据包数。当读取了这么多的数据包并将其传送到目标进程后，新的数据包才被读取，直到有可用数据的新通知到达为止。默认为5。如果此参数设置得太高，由于UDP数据包泛洪，系统可能会变得无响应。
        
    {recbuf, integer() >= 0} |
        用于套接字的接收缓冲区的最小大小。建议您使用 getopts / 2 来检索操作系统设置的大小。
    {reuseaddr, boolean()} |
        允许或禁止端口号的本地重用。默认情况下，不允许重用。
    {send_timeout, integer() >= 0 | infinity} |
        仅允许用于面向连接的套接字。
        指定最长时间等待基础TCP堆栈接受发送操作。当超过限制时，发送操作将返回 {error，timeout}。未知发送了多少数据包；因此，只要发生超时，套接字就将关闭（请参见 下面的send_timeout_close）。默认为infinity。
    {send_timeout_close, boolean()} |
        仅允许用于面向连接的套接字。
        与send_timeout一起使用，以指定当send操作返回{error，timeout}时是否自动关闭套接字。推荐的设置为 true，它将自动关闭套接字。由于向后兼容，默认为false。
    {show_econnreset, boolean()} |
        当此选项设置为false时（默认情况下），将从TCP对等方接收到的RST视为正常关闭（就像已发送FIN一样）。gen_tcp：recv / 2的调用者 获得{错误，关闭}。在活动模式下，控制进程收到 {tcp_closed，Socket}消息，指示对等方已关闭连接。
        将此选项设置为true可让您区分正常关闭的连接和TCP对等方中止（有意或无意）的连接。调用 gen_tcp：recv / 2 返回{error，econnreset}。在活动模式下，控制过程会在通常的{tcp_closed，Socket}之前接收到 {tcp_error，Socket，econnreset}消息，就像其他套接字错误一样。调用 gen_tcp：send / 2 也会返回{error，econnreset} 当检测到TCP对等体已发送RST时。
        从gen_tcp：accept / 1返回的已连接套接字 从侦听套接字 继承了show_econnreset设置。
    {sndbuf, integer() >= 0} |
        用于套接字的发送缓冲区的最小大小。鼓励您使用 getopts / 2来检索操作系统设置的大小。
    {tos, integer() >= 0} |
        在实现此功能的平台上设置IP_TOS IP级别选项。行为和允许范围在不同系统之间有所不同。该选项在未实现的平台上被忽略。请谨慎使用。
    {tclass, integer() >= 0} |
        在实现此功能的平台上 设置IPV6_TCLASS IP级别选项。行为和允许范围在不同系统之间有所不同。该选项在未实现的平台上被忽略。请谨慎使用。
    {ttl, integer() >= 0} |
    {recvtos, boolean()} |
    {recvtclass, boolean()} |
    {recvttl, boolean()} |

option_name() = active | buffer | delay_send | deliver | dontroute | exit_on_close | header | high_msgq_watermark |
high_watermark | keepalive | linger | low_msgq_watermark | low_watermark | mode | nodelay | packet | packet_size |
pktoptions | priority | {raw,Protocol :: integer() >= 0, OptionNum :: integer() >= 0, ValueSpec ::(ValueSize ::
integer() >= 0) | (ValueBin :: binary())} | recbuf | reuseaddr | send_timeout | send_timeout_close | show_econnreset |
sndbuf | tos | tclass | ttl | recvtos | recvtclass | recvttl | pktoptions | ipv6_v6only

connect_option() = {ip, inet:socket_address()} | {fd, Fd :: integer() >= 0} | {ifaddr, inet:socket_address()} | inet:
address_family() | {port, inet:port_number()} | {tcp_module, module()} | {netns, file:filename_all()} | {bind_to_device,
binary()} | option()
listen_option() = {ip, inet:socket_address()} | {fd, Fd :: integer() >= 0} | {ifaddr, inet:socket_address()} | inet:
address_family() | {port, inet:port_number()} | {backlog, B :: integer() >= 0} | {tcp_module, module()} | {netns, file:
filename_all()} | {bind_to_device, binary()} | option()
socket()
As returned by accept/1,2 and connect/3,4.         