%%%----------------------------------------------------------------------------
%%% @doc Erlang DNS (EDNS) server
%%% @author Hans Christian v. Stockhausen <hc@vst.io>
%%% @end
%%%----------------------------------------------------------------------------

-module(ed_udp_handler_server).

%% API
-export([start_link/1]).

%% Callback
%-export([init/1]).

% This is a terrible habit to be in, but manually exporting all callbacks is
% a huge PitA
-compile(export_all).

%% This is a session actor
-behaviour(ssa_gen_server).

%%%============================================================================
%%% State
%%%============================================================================
-record(dns_handler_state, {socket_triple, % {socket, ip, port}
                            query}).


%%%============================================================================
%%% API
%%%============================================================================

start_link(Packet) ->
  ssa_gen_server:start_link(?MODULE, [self(), Packet], []).

%%%============================================================================
%%% Session Actor Callbacks
%%%============================================================================

ssactor_init([Parent, {udp, Socket, IP, Port, ReqBin}], MonitorPID) ->
  %proc_lib:init_ack(Parent, {ok, self()}),
  %answer_query(Packet),
  error_logger:info_msg("Starting DNS handling session.~n", []),
  conversation:start_conversation(MonitorPID, "HandleDNSRequest", "UDPHandlerServer"),
  #dns_handler_state{socket_triple={Socket, IP, Port},
                     query=ReqBin}.
  %exit(normal).

ssactor_join(_, _, _, State) -> {accept, State}.
% Main protocol
ssactor_conversation_established("HandleDNSRequest", _RN, _CID, ConvKey, State) ->
  error_logger:info_msg("DNS Handling session established.~n", []),
  conversation:register_conversation(main_session, ConvKey),
  SocketTriple = State#dns_handler_state.socket_triple,
  Query = State#dns_handler_state.query,
  InitialQueryRes = answer_query(SocketTriple, Query, ConvKey),
  case InitialQueryRes of
    {not_done, Q} -> {ok, State#dns_handler_state{query=Q}};
    {done, Q} -> send_response(SocketTriple, Q),
                 {ok, State}
  end;
% Subsession
ssactor_conversation_established("GetZoneData", _RN, _CID, ConvKey, State) ->
  conversation:send(ConvKey, ["DNSZoneDataServer"], "ZoneDataRequest", [], []),
  {ok, State}.

ssactor_conversation_error(_, _, _, State) ->
  exit(conversation_error),
  {ok, State}.

ssactor_conversation_ended(CID, Reason, State) ->
  error_logger:info_msg("Conversation ~p ended for reason ~p.~n", [CID, Reason]),
  {ok, State}.

ssactor_handle_message("HandleDNSRequest", "UDPHandlerServer", _CID, _Sender,
                       "ZoneResponse", [Response], State, ConvKey) ->
  SocketTriple = State#dns_handler_state.socket_triple,
  Query = State#dns_handler_state.query,
  Resolver = get_resolver(),
  Q1 = Resolver:load_zone(Query, Response, ConvKey),
  {ok, State#dns_handler_state{query=Q1}};

ssactor_handle_message("GetZoneData", "UDPHandlerServer", _CID, _Sender,
                       "ZoneDataResponse", [RRTree], State, ConvKey) ->
  % Subsession's done: pass the response back
  conversation:subsession_complete(ConvKey, RRTree),
  %conversation:become(ConvKey, main_session, "UDPHandlerServer",
  %                    continue_resolution, [RRTree]),
  {ok, State};
ssactor_handle_message("HandleDNSRequest", "UDPHandlerServer", _CID, _Sender,
                       "InvalidZone", [], State, _ConvKey) ->
  SocketTriple = State#dns_handler_state.socket_triple,
  Query = State#dns_handler_state.query,
  Resolver = get_resolver(),
  RespQ = Resolver:non_existent_zone(Query),
  % Deliver the bad news to the client
  send_response(SocketTriple, RespQ),
  {ok, State}.

ssactor_subsession_setup_failed(SubsessionName, Reason, State, _ConvKey) ->
  error_logger:error_msg("Starting subsession ~p failed: ~p~n",
                         [SubsessionName, Reason]),
  exit(Reason),
  {ok, State}.

ssactor_subsession_complete(SubsessionName, RRTree, State, ConvKey) ->
  SocketTriple = State#dns_handler_state.socket_triple,
  Query = State#dns_handler_state.query,
  Resolver = get_resolver(),
  error_logger:info_msg("Complete: Name:~p~nRRTree: ~p~nState: ~p~nConvKey:~p~n",
                        [SubsessionName, RRTree, State, ConvKey]),
  % At this stage, the query will either be done (either completed or an error),
  % or will require a recursive zone lookup.
  MatchRes = Resolver:match_records(Query, RRTree, ConvKey),
  case MatchRes of
    {not_done, Q} -> {ok, State#dns_handler_state{query=Q}};
    {done, Q} -> send_response(SocketTriple, Q),
                 {ok, State}
  end.

ssactor_subsession_failed(Name, FailureName, State, _ConvKey) ->
  error_logger:error_msg("Subsession ~p failed: ~p~n", [Name, FailureName]),
  {ok, State}.

ssactor_become(ProtocolName, RoleName, Command, Value, _ConvKey, State) ->
  error_logger:warning_msg("Unhandled become: protocol ~p, role ~p, command: ~p, value: ~p~n",
                           [ProtocolName, RoleName, Command, Value]),
  {ok, State}.
%%%============================================================================
%%% Internal
%%%============================================================================

get_resolver() ->
  {ok, Resolvers} = application:get_env(edns, resolvers),
  % TODO: Put the multiple resolvers back in, but a single one should do for now
  %Resp = lists:foldl(
  % 	fun(R, Acc) ->
  % 	  R:resolve(Acc)
  % 	end, Query, Resolvers),
  [R|_RS] = Resolvers,
  R.

answer_query({Socket, IP, Port}, ReqBin, ConvKey) ->
  {ok, Query} = inet_dns:decode(ReqBin),
  % Send off to the resolver, which in the synchronous version gets us a response
  % to send back, which may indicate an error state.
  % In the async version it's a bit trickier though -- if we're successful, then
  % we'll have a partially-resolved query to store in the state for the next step.
  % If we're not, we'll have an error to send back.
  % We'll need to make this a sum type.
  R = get_resolver(),
  R:resolve(Query, ConvKey).

send_response({Socket, IP, Port}, Resp) ->
  RespBin = inet_dns:encode(Resp),
  gen_udp:send(Socket, IP, Port, RespBin),
  exit(normal).

terminate(_R, _S) -> ok.
