-module(edns_conversation_conf).
-export([config/0]).

config() ->
  [{ed_zone_data_server, [{"HandleDNSRequest", ["DNSZoneDataServer"]}]},
   {ed_zone_registry_server, [{"HandleDNSRequest", ["DNSZoneRegServer"]}]},
   {ed_udp_handler_server, [{"HandleDNSRequest", ["UDPHandlerServer"]}]}].
