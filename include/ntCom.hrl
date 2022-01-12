-define(ntErr(Format, Args), error_logger:error_msg(Format, Args)).
-define(ntWarn(Format, Args), error_logger:warning_msg(Format, Args)).
-define(ntInfo(Format, Args), error_logger:info_msg(Format, Args)).

-define(getLValue(Key, List, Default), ntCom:getListValue(Key, List, Default)).