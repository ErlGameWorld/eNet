%% 令牌桶算法
%% 基本过程
%%    假如用户配置的平均发送速率为rate，则每隔1/rate秒一个令牌被加入到桶中；
%%    假设桶最多可以存发b个令牌。如果令牌到达时令牌桶已经满了，那么这个令牌会被丢弃；
%%    当一个n个字节的数据包到达时，就从令牌桶中删除n个令牌，并且数据包被发送到网络；
%%    如果令牌桶中少于n个令牌，那么不会删除令牌，并且认为这个数据包在流量限制之外；
%%    算法允许最长b个字节的突发，但从长期运行结果看，数据包的速率被限制成常量r。
%%    对于在流量限制外的数据包可以以不同的方式处理：
%%       它们可以被丢弃；
%%       它们可以排放在队列中以便当令牌桶中累积了足够多的令牌时再传输；
%%       它们可以继续发送，但需要做特殊标记，网络过载的时候将这些特殊标记的包丢弃。
%%    注意：令牌桶算法不能与另外一种常见算法“漏桶算法（Leaky Bucket）”相混淆。
%%       这两种算法的主要区别在于“漏桶算法”能够强行限制数据的传输速率，
%%       而“令牌桶算法”在能够限制数据的平均传输数据外，还允许某种程度的突发传输。
%%       在“令牌桶算法”中，只要令牌桶中存在令牌，那么就允许突发地传输数据直到达到用户配置的门限，因此它适合于具有突发特性的流量。
-module(nlTokenBucket).
-include("erlNetLib.hrl").

-export([
   new/1
   , new/2
   , check/2
   , check/3
]).

-type(tokenBucket() :: #tokenBucket{}).
-type(tbConfig() :: {pos_integer(), pos_integer()}).

-spec(new(tbConfig()) -> tokenBucket()).
new({Rate, BucketSize}) ->
   new(Rate, BucketSize).

-spec(new(pos_integer(), pos_integer()) -> tokenBucket()).
new(Rate, BucketSize) when is_integer(BucketSize), 0 < Rate andalso Rate =< BucketSize ->
   #tokenBucket{
      rate = Rate
      , tokens = BucketSize
      , lastTime = erlang:system_time(milli_seconds)
      , bucketSize = BucketSize
   }.

-spec(check(pos_integer(), tokenBucket()) -> {non_neg_integer(), tokenBucket()}).
check(Consume, TokenBucket) ->
   check(Consume, erlang:system_time(milli_seconds), TokenBucket).

-spec(check(pos_integer(), integer(), tokenBucket()) -> {non_neg_integer(), tokenBucket()}).
check(Consume, Now, #tokenBucket{rate = Rate, tokens = Tokens, lastTime = LastTime, bucketSize = BucketSize} = TokenBucket) ->
   AvailableToken = erlang:min(BucketSize, Tokens + (Rate * (Now - LastTime)) div 1000),
   case AvailableToken >= Consume of
      true  ->
         %% Tokens available
         {0, TokenBucket#tokenBucket{tokens = AvailableToken - Consume, lastTime = Now}};
      false ->
         %% Tokens not enough
         %% 计算需要等待的时间 单位毫秒
         WaitTime = (Consume - AvailableToken) * 1000 div Rate,
         {WaitTime, TokenBucket#tokenBucket{tokens = 0, lastTime = Now}}
   end.
