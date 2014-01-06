%%%----------------------------------------------------------------------------
%%% @doc Erlang DNS (EDNS) Log Handler
%%%
%%%      This is a sample handler that just logs the DNS query and returns it
%%%      unchanged. In order for this handler to be invoked is has to be first
%%%      registered. This is done automatically on startup in edns:start/0.
%%%
%%%      A typical handler should resolve a DNS query and retun a DNS response.
%%%
%%%      When a DNS query is processed by ed_lkup_server it folds the query 
%%%      over all registered handlers.
%%%
%%% @author Hans Christian v. Stockhausen <hc@vst.io>
%%% @end
%%%----------------------------------------------------------------------------

-module(ed_log_handler).

-behaviour(ed_gen_handler).

-export([register/0, handle_call/1]).

-include_lib("kernel/src/inet_dns.hrl").

%%%============================================================================
%%% API
%%%============================================================================

register() ->
    ed_gen_handler:register_handler(?MODULE).

handle_call(DnsQuery) ->
  error_logger:info_msg("Got query: ~p~n", [DnsQuery]),
  DnsQuery.

%%%============================================================================
%%% Internal
%%%============================================================================
