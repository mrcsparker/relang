-module(relang).

-author(kureikain).
-email("kurei@axcoto.com").

%%-export([connect/1]).

-export([
  start/0,
  stop/0,
  connect/0, connect/1, connect/2, connect/3,
  close/1,
  r/1, r/2,
  next/1,
  stream_stop/2,
  stream_recv/2,
  stream_poll/2
]).

%-compile(export_all). %% replace with -export() later, for God's sake!

%% From ql2.proto
-define(RETHINKDB_VERSION, 16#400c2d20).
-define(RETHINKDB_PROTOCOL, 16#7e6970c7).

-include("term.hrl").

start() ->
  application:start(relang),
  ok.

stop() ->
  application:stop(relang),
  ok.

%% http://erlang.org/pipermail/erlang-questions/2004-December/013734.html

%% @spec () -> Socket
%% @doc
%%    Connects with:
%%      <ul>
%%      <li>`Host': `127.0.0.1'</li>
%%      <li>`Port': `28015'</li>
%%      <li>`Auth': `No Authentication'</li>
%%      </ul>
connect() ->
  connect("127.0.0.1").

%% @spec (Host) -> Socket
%%    Host = string() | ip_address()
%% @doc
%%    Connects with:
%%      <ul>
%%      <li>`Port': `28015'</li>
%%      <li>`Auth': `No Authentication'</li>
%%      </ul>
connect(Host) ->
  do_connect(Host, 28015, <<"">>).

%% @spec (Host, Port) -> Socket
%%    Host = string() | ip_address()
%%    Port = integer()
%% @doc
%%    Connects with:
%%      <ul>
%%      <li>`Auth': `No Authentication'</li>
%%      </ul>
connect(Host, AuthKey) ->
  do_connect(Host, 28015, AuthKey).

%% @spec (Host, Port, AuthKey) -> Socket
%%    Host = string() | ip_address()
%%    Port = integer()
%%    AuthKey = string()
connect(Host, Port, AuthKey) ->
  do_connect(Host, Port, AuthKey).

do_connect(Host, Port, AuthKey) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, false}]),
  handshake(Socket, AuthKey),
  Socket.

%% @spec (Socket) -> ok | {error, Reason}
%%    Socket = socket()
%%    Reason = atom()
%% @doc
%%    Closes a RethinkDb connection.
%% @end
close(Socket) ->
  gen_tcp:close(Socket).

handshake(Socket, AuthKey) ->
  ok = setup_protocols(Socket, AuthKey),

  {ok, Response} = read_until_null(Socket),
  case Response == <<"SUCCESS",0>> of
    true -> ok;
    false ->
      io:fwrite("Error: ~s~n", [Response]),
      {error, Response}
  end.

setup_protocols(Socket, AuthKey) ->
  ok = send_protocol_version(Socket),
  ok = send_authorization_key(Socket, AuthKey),
  send_json_protocol(Socket).

send_protocol_version(Socket) ->
  lager:info("send_protocol_version"),
  gen_tcp:send(Socket, binary:encode_unsigned(?RETHINKDB_VERSION, little)).

send_authorization_key(Socket, AuthorizationKey) ->
  AuthorizationKeyLength = iolist_size(AuthorizationKey),
  gen_tcp:send(Socket, [<<AuthorizationKeyLength:32/little-unsigned>>, AuthorizationKey]).

send_json_protocol(Socket) ->
    gen_tcp:send(Socket, [<<?RETHINKDB_PROTOCOL:32/little-unsigned>>]).

%%% RethinkDB API
r(Q) -> query(Q).
r(Socket, RawQuery) ->
  query(Socket, RawQuery).

%%% Fetch next batch
next({Socket, Token}) ->
  Iolist = ["[2]"],
  Length = iolist_size(Iolist),
  io:format("Block socket <<< waiting for more data from stream~n"),

  ok = gen_tcp:send(Socket, [<<Token:64/little-unsigned>>, <<Length:32/little-unsigned>>, Iolist]),
  {ok, R} = recv(Socket),
  Rterm = jsx:decode(R),
  proplists:get_value(<<"r">>, Rterm).

%%% Build AST from raw query
query(RawQuery) ->
  relang_ast:make(RawQuery).

token() ->
  {A1, A2, A3} = erlang:timestamp(),
  random:seed(A1, A2, A3),
  random:uniform(3709551616).

%%% Build and Run query when passing Socket
query(Socket, RawQuery) ->
  query(Socket, RawQuery, [{}]).

query(Socket, RawQuery, Option) ->
  Token = token(),
  Query = relang_ast:make(RawQuery),

  io:format("Query = ~p ~n", [Query]),
  Iolist  = jsx:encode([?QUERYTYPE_START, Query, Option]), % ["[1,"] ++ [Query] ++ [",{}]"], % list db 
  Length = iolist_size(Iolist),

  case gen_tcp:send(Socket, [<<Token:64/little-unsigned>>, <<Length:32/little-unsigned>>, Iolist]) of
    ok -> ok;
    {error, Reason} ->
      io:fwrite("Got Error when sending query: ~s ~n", [Reason])
  end,

  handle_recv(recv(Socket), Socket, Token).
%%%

handle_recv({ok, R}, Socket, Token) ->
  Rterm = jsx:decode(R),
  handle_recv_value(proplists:get_value(<<"t">>, Rterm), Rterm, Socket, Token);
handle_recv({error, ErrReason}, _Socket, _Token) ->
  io:fwrite("Got Error when receving: ~s ~n", [ErrReason]),
      {error, ErrReason}.

handle_recv_value(?RUNTIME_ERROR, Rterm, _Socket, _Token) ->
  io:format("Error"),
  {error, proplists:get_value(<<"r">>, Rterm)};
handle_recv_value(?SUCCESS_ATOM, Rterm, _Socket, _Token) ->
  io:format("response: a single atom"),
  {ok, proplists:get_value(<<"r">>, Rterm)};
handle_recv_value(?SUCCESS_SEQUENCE, Rterm, _Socket, _Token) ->
  io:format("response: a sequence"),
  {ok, proplists:get_value(<<"r">>, Rterm)};
handle_recv_value(?SUCCESS_PARTIAL, Rterm, Socket, Token) ->
  % So we get back a stream, let continous pull query
  io:format("response: partial. Can use next here"),

  Recv = spawn(?MODULE, stream_recv, [Socket, Token]),
  Pid = spawn(?MODULE, stream_poll, [{Socket, Token}, Recv]),

  {ok, {pid, Pid}, proplists:get_value(<<"r">>, Rterm)}.

%%% When the response_type is SUCCESS_PARTIAL=3, we can call next to send more data
%next(_Query) ->
%  continue.

stream_stop(Socket, Token) ->
  Iolist = ["[3]"],
  Length = iolist_size(Iolist),
  ok = gen_tcp:send(Socket, [<<Token:64/little-unsigned>>, <<Length:32/little-unsigned>>, Iolist])
  .

%% receive data from stream, then pass to other process
stream_recv(Socket, Token) ->
  receive
    R ->
      io:fwrite("Changefeed receive item: ~p ~n",[R])
  end,
  stream_recv(Socket, Token)
  .

%Continues getting data from stream
stream_poll({Socket, Token}, PidCallback) ->
  Iolist = ["[2]"],
  Length = iolist_size(Iolist),
  io:format("Block socket <<< waiting for more data from stream~n"),

  ok = gen_tcp:send(Socket, [<<Token:64/little-unsigned>>, <<Length:32/little-unsigned>>, Iolist]),
  {ok, R} = recv(Socket),
  Rterm = jsx:decode(R),
  spawn(fun() -> PidCallback ! proplists:get_value(<<"r">>, Rterm) end),
  stream_poll({Socket, Token}, PidCallback)
  .
%% Receive data from Socket
%%Once the query is sent, you can read the response object back from the server. The response object takes the following form:
%%
%% * The 8-byte unique query token
%% * The length of the response, as a 4-byte little-endian integer
%% * The JSON-encoded response
recv(Socket) ->
  case gen_tcp:recv(Socket, 8) of
    {ok, Token} ->
      <<K:64/little-unsigned>> = Token,
      io:format("Get back token ~p ~n", [K]),
      io:format("Get back token ~p ~n", [Token]);
    {error, _Reason} ->
      io:format("Fail to parse token")
  end,

  {_RecvResultCode, ResponseLength} = gen_tcp:recv(Socket, 4),
  <<Rs:32/little-unsigned>> = ResponseLength,
  io:format("ResponseLengh ~p ~n", [Rs]),
  io:format("ResponseLengh ~p ~n", [ResponseLength]),

  {ResultCode, Response} = gen_tcp:recv(Socket, binary:decode_unsigned(ResponseLength, little)),
  case ResultCode of
    ok ->
      {ok, Response};
    error ->
      io:fwrite("Got Error ~s ~n", [Response]),
      {error, Response}
  end.

read_until_null(Socket) ->
  read_until_null(Socket, []).

read_until_null(Socket, Acc) ->
  %%{ok, Response} = gen_tcp:recv(Socket, 0),
  case gen_tcp:recv(Socket, 0) of
    {error, OtherSendError} ->
      io:format("Some other error on socket (~p), closing", [OtherSendError]),
      %%Client ! {self(),{error_sending, OtherSendError}},
      gen_tcp:close(Socket);
    {ok, Response} ->
      Result = [Acc, Response],
      case is_null_terminated(Response) of
        true -> {ok, iolist_to_binary(Result)};
        false -> read_until_null(Socket, Result)
      end
  end.

is_null_terminated(B) ->
  binary:at(B, iolist_size(B) - 1) == 0.
