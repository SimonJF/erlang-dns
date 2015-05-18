%%%----------------------------------------------------------------------------
%%% @doc Erlang DNS (EDNS) server
%%% @author Hans Christian v. Stockhausen <hc@vst.io>
%%% @end
%%%----------------------------------------------------------------------------

-module(ed_zone_registry_server).

-behaviour(ssa_gen_server).

-compile(export_all).

%% API
%-export([start_link/0, register/2, deregister/1, get/1, find_nearest_zone/2]).

%% behaviour callbacks
%-export([ssactor_init/1, ssactor_handle_message/8,
%         handle_call/3,
%         handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%============================================================================
%%% API
%%%============================================================================

start_link() ->
  ssa_gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

register(ZoneName, Pid) ->
  gen_server:call(?SERVER, {register, ZoneName, Pid}).

deregister(ZoneName) ->
  gen_server:call(?SERVER, {deregister, ZoneName}).

get(ZoneName) ->
  gen_server:call(?SERVER, {get, ZoneName}).

find_nearest_zone(DomainName, ConvKey) ->
  conversation:send(ConvKey, ["DNSZoneRegServer"], "FindNearestZone",
                    ["DomainName"], [DomainName]).


%%%============================================================================
%%% behaviour callbacks
%%%============================================================================

ssactor_init([], _MonitorPID) ->
  %{ok, gb_trees:empty()}.
  gb_trees:empty().
ssactor_join(_, _, _, State) -> {accept, State}.
ssactor_conversation_established(_PN, _RN, _CID, _ConvKey, State) -> {ok, State}.
ssactor_conversation_error(_, _, _, State) -> {ok, State}.

ssactor_handle_message("HandleDNSRequest", "DNSZoneRegServer", _CID, _Sender,
                       "FindNearestZone", [DomainName], State, ConvKey) ->
  NameTails = ed_utils:tails(string:tokens(DomainName, ".")),
  Names = lists:map(fun(X) -> string:join(X, ".") end, NameTails),
  IsZoneNotDefined = fun(Z) -> not gb_trees:is_defined(Z, State) end,
  case lists:dropwhile(IsZoneNotDefined, Names) of
  	[] ->
      conversation:send(ConvKey, ["UDPHandlerServer"], "ZoneNotFound", [], []);
  	[H|_] ->
      Pid = gb_trees:get(H, State),
      conversation:send(ConvKey, ["UDPHandlerServer"], "ZoneResponse", ["ZonePID"], [Pid])
  end,
  {ok, State}.


handle_call({register, ZoneName, Pid}, _From, State) ->
  case gb_trees:is_defined(ZoneName, State) of
  	false ->
  	  error_logger:info_msg("Zone ~p registered.", [ZoneName]),
  	  {reply, ok, gb_trees:insert(ZoneName, Pid, State)};
  	true ->
  	  {reply, {error, zone_already_registered}, State}
  end;
handle_call({deregister, ZoneName}, _From, State) ->
  case gb_trees:is_defined(ZoneName, State) of
  	true ->
  	  error_logger:info_msg("Zone ~p deregistered.", [ZoneName]),
  	  {reply, ok, gb_trees:delete(ZoneName, State)};
  	false ->
  	  {reply, {error, zone_not_registered}, State}
  end;
handle_call({get, ZoneName}, _From, State) ->
  case gb_trees:is_defined(ZoneName, State) of
  	true ->
  	  {reply, {ok, gb_trees:get(ZoneName,State)}, State};
  	false ->
  	  {reply, {error, zone_not_registered}, State}
  end;
handle_call(_Request, _From, State) ->
  error_logger:error_msg("Unexpected request"),
  {noreply, State}.

handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
