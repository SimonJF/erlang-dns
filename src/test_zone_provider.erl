-module(test_zone_provider).
-export([get_zone/1]).

-include_lib("kernel/src/inet_dns.hrl").

get_zone(_A) ->
  {ok,
   [
     #dns_rr{domain="multiparty.session.types", type=soa, data={
             "localhost",
             "simonjf.com",
             20150320,
             1800,
             300,
             604800,
             86400
     }},
     #dns_rr{domain="multiparty.session.types", type=a, data={255,255,255,255}}
   ]
  }.

