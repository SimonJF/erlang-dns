%%%----------------------------------------------------------------------------
%%% @doc Erlang DNS (EDNS) server
%%% @author Hans Christian v. Stockhausen <hc@vst.io>
%%% @end
%%%----------------------------------------------------------------------------

-module(ed_zone_data_server).

-behaviour(ssa_gen_server).

%% API
%-export([start_link/1, get_zone/1, flush/1]).

%% behaviour callbacks
%-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
%  terminate/2, code_change/3]).

-compile(export_all).

-record(state, {zone_name, zone_provider, rr_tree}).

-include_lib("kernel/src/inet_dns.hrl").

%%%============================================================================
%%% API
%%%============================================================================

start_link(Args) ->
  Res =ssa_gen_server:start_link(?MODULE, [Args], []),
  Res.

get_zone(Pid, ConvKey) ->
  conversation:invite(ConvKey, Pid, "DNSZoneDataServer"),
  conversation:send(ConvKey, ["DNSZoneDataServer"], "GetZoneData", [], []).

flush(Pid) ->
  gen_server:cast(Pid, flush).

%%%============================================================================
%%% behaviour callbacks
%%%============================================================================

ssactor_init([{ZoneName, ZoneProvider}], MonitorPID) ->
  case ed_zone_registry_server:register(ZoneName, MonitorPID) of
    ok -> #state{zone_name=ZoneName, zone_provider=ZoneProvider};
    {error, Reason} -> {stop, Reason}
  end.

ssactor_join(_, _, _, State) -> {accept, State}.
ssactor_conversation_established(_PN, _RN, _CID, _ConvKey, State) -> {ok, State}.
ssactor_conversation_error(_, _, _, State) -> {ok, State}.

ssactor_handle_message("HandleDNSRequest", "DNSZoneDataServer", _CID, _Sender,
                       "GetZoneData", [], State, ConvKey) ->
  RRTree = State#state.rr_tree,
  conversation:send(ConvKey, ["UDPHandlerServer"], "ZoneDataResponse", ["RRTree"],
                   [RRTree]),
  State.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(flush, State) ->
  {noreply, State, 0};
handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info(timeout, #state{zone_provider={M,F,A}}=State) ->
  error_logger:info_msg("Reloading zone: ~p", [State#state.zone_name]),
  {ok, RRs} = M:F(A),
  RRs1 = convert_records_to_lowercase(RRs),
  RRTree = lists:foldr(
    fun(RR, Tree) ->
      Domain = RR#dns_rr.domain,
      case gb_trees:is_defined(Domain, Tree) of
        false ->
          gb_trees:insert(Domain, [RR], Tree);
        true ->
          gb_trees:update(Domain, [RR|gb_trees:get(Domain, Tree)], Tree)
       end
    end, gb_trees:empty(), RRs1),
  {ok, RefreshInterval} = get_zone_expiry(RRs1),
   {noreply, State#state{rr_tree=RRTree}, RefreshInterval};
handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, #state{zone_name=ZoneName}) ->
  ed_zone_registry_server:deregister(ZoneName),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%============================================================================
%%% Internal
%%%============================================================================

convert_records_to_lowercase(RRs) ->
  lists:map(fun domain_to_lowercase/1, RRs).

domain_to_lowercase(#dns_rr{domain=Domain}=RR) ->
  RR#dns_rr{domain=string:to_lower(Domain)}.

get_zone_expiry(RRs) ->
  case get_zone_expiry_from_soa(RRs) of
    undefined -> soa_missing;
    Expiry -> {ok, Expiry * 1000} % convert to milliseconds
  end.

get_zone_expiry_from_soa(RRs) ->
  lists:foldr(
    fun(RR, Acc) ->
      case RR#dns_rr.type of
        soa ->
          {_Host, _Contact, _Serial, Refresh, _Retry, _Expiry, _Min} =
            RR#dns_rr.data,
          Refresh;
        _ -> Acc
      end
    end, undefined, RRs).
