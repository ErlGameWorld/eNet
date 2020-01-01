-module(nlNetCom).

-compile([export_all]).

-spec mergeOpts(Defaults :: #{}, Options :: #{}) -> #{}.
mergeOpts(Defaults, Options) ->
   maps:merge(Defaults, Options).

mergeAddr({Addr, _Port}, SockOpts) ->
   lists:keystore(ip, 1, SockOpts, {ip, Addr});
mergeAddr(_Port, SockOpts) ->
   SockOpts.

getPort({_Addr, Port}) -> Port;
getPort(Port) -> Port.


parseAddr({Addr, Port}) when is_list(Addr), is_integer(Port) ->
   {ok, IPAddr} = inet:parse_address(Addr),
   {IPAddr, Port};
parseAddr({Addr, Port}) when is_tuple(Addr), is_integer(Port) ->
   case isIpv4OrIpv6(Addr) of
      true ->
         {Addr, Port};
      false ->
         error(invalid_ipaddr)
   end;
parseAddr(Port) ->
   Port.

isIpv4OrIpv6({A, B, C, D}) ->
   A >= 0 andalso A =< 255 andalso
   B >= 0 andalso B =< 255 andalso
   C >= 0 andalso C =< 255 andalso
   D >= 0 andalso D =< 255;
isIpv4OrIpv6({A, B, C, D, E, F, G, H}) ->
   A >= 0 andalso A =< 65535 andalso
   B >= 0 andalso B =< 65535 andalso
   C >= 0 andalso C =< 65535 andalso
   D >= 0 andalso D =< 65535 andalso
   E >= 0 andalso E =< 65535 andalso
   F >= 0 andalso F =< 65535 andalso
   G >= 0 andalso G =< 65535 andalso
   H >= 0 andalso H =< 65535;
isIpv4OrIpv6(_) ->
   false.

%% @doc Return true if the value is an ipv4 address
isIpv4({A, B, C, D}) ->
   A >= 0 andalso A =< 255 andalso
   B >= 0 andalso B =< 255 andalso
   C >= 0 andalso C =< 255 andalso
   D >= 0 andalso D =< 255;
isIpv4(_) ->
   false.

%% @doc Return true if the value is an ipv6 address
isIpv6({A, B, C, D, E, F, G, H}) ->
   A >= 0 andalso A =< 65535 andalso
   B >= 0 andalso B =< 65535 andalso
   C >= 0 andalso C =< 65535 andalso
   D >= 0 andalso D =< 65535 andalso
   E >= 0 andalso E =< 65535 andalso
   F >= 0 andalso F =< 65535 andalso
   G >= 0 andalso G =< 65535 andalso
   H >= 0 andalso H =< 65535;
isIpv6(_) ->
   false.

-spec warnMsg(term(), string(), [term()]) -> ok.
warnMsg(Tag, Format, Data) ->
   error_logger:warning_msg("[~p] " ++ Format, [Tag | Data]).

getListValue(Key, List, Default) ->
   case lists:keyfind(Key, 1, List) of
      false ->
         Default;
      {Key, Value} ->
         Value
   end.

