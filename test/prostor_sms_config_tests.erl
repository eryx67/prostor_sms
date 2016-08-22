-module(prostor_sms_config_tests).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

basic_test() ->
    Conf =
        [
         {["prostor_sms", "password"], "pass"}
        , {["prostor_sms", "login"], "user"}
        , {["prostor_sms", "address"], "addr"}
        ],

    Config = cuttlefish_unit:generate_templated_config(
               ["../priv/prostor_sms.schema"], Conf, []),

    ?debugFmt("~P", [Config, 1024]),
    ok.
