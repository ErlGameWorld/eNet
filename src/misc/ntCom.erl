-module(ntCom).

-compile([export_all, nowarn_export_all]).

-spec mergeOpts(Defaults :: list(), Options :: list()) -> list().
mergeOpts(Defaults, Options) ->
   lists:foldl(
      fun({Opt, Val}, Acc) ->
         lists:keystore(Opt, 1, Acc, {Opt, Val});
         (Opt, Acc) ->
            lists:usort([Opt | Acc])
      end,
      Defaults, Options).

mergeAddr({Addr, _Port}, SockOpts) ->
   lists:keystore(ip, 1, SockOpts, {ip, Addr});
mergeAddr(_Port, SockOpts) ->
   SockOpts.

getPort({_Addr, Port}) -> Port;
getPort(Port) -> Port.

fixIpPort(IpOrStr, Port) ->
   if
      is_list(IpOrStr), is_integer(Port) ->
         {ok, IP} = inet:parse_address(v),
         {IP, Port};
      is_tuple(IpOrStr), is_integer(Port) ->
         case isIpv4OrIpv6(IpOrStr) of
            true ->
               {IpOrStr, Port};
            false ->
               error({invalid_ip, IpOrStr})
         end;
      true ->
         error({invalid_ip_port, IpOrStr, Port})
   end.

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

getListValue(Key, List, Default) ->
   case lists:keyfind(Key, 1, List) of
      false ->
         Default;
      {Key, Value} ->
         Value
   end.

serverName(PoolName, Index) ->
   list_to_atom(atom_to_list(PoolName) ++ "_" ++ integer_to_list(Index)).

asName(tcp, PrName) ->
   binary_to_atom(<<(atom_to_binary(PrName))/binary, "TAs">>);
asName(ssl, PrName) ->
   binary_to_atom(<<(atom_to_binary(PrName))/binary, "SAs">>);
asName(udp, PrName) ->
   binary_to_atom(<<(atom_to_binary(PrName))/binary, "UOs">>).


lsName(tcp, PrName) ->
   binary_to_atom(<<(atom_to_binary(PrName))/binary, "TLs">>);
lsName(ssl, PrName) ->
   binary_to_atom(<<(atom_to_binary(PrName))/binary, "SLs">>);
lsName(udp, PrName) ->
   binary_to_atom(<<(atom_to_binary(PrName))/binary, "URs">>).



