-module(echo_server).
-behaviour(gen_server).

-export([
    start_link/1,
	init/1, handle_call/3, handle_cast/2, handle_info/2,
	code_change/3, terminate/2
]).

-define(SOCK(Msg), {tcp, _Port, Msg}).

-record(state, {socket}).

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
    % Because accepting a connection is a blocking function call,
    % we can not do it in here.  Forward to the server loop.
    gen_server:cast(self(), accept),
    {ok, #state{socket = Socket}}.

% We never need you, handle_call!
handle_call(_E, _From, State) ->
    {noreply, State}.

handle_cast(accept, S = #state{socket = ListenSocket}) ->
    % Block waiting for a connection.
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    echo_sup:start_socket(),
    {noreply, S#state{socket = AcceptSocket}};
handle_cast(Event, S) ->
    io:format(standard_error, "echo[~p]: ~p", [self(), Event]),
    {noreply, S}.

handle_info(E = {tcp_closed, _Socket}, S) ->
    io:format("echo[~p]: ~p~n", [self(), E]),
    {stop, normal, S};
handle_info(E = {tcp_error, _Socket, _}, S) ->
    io:format("echo[~p]: ~p~n", [self(), E]),
    {stop, normal, S};
handle_info({tcp, _Port, Line}, #state{socket = Socket} = S) ->
    io:format(standard_error, "echo[~p]: ~s", [self(), Line]),
    ok = send(Socket, Line),
    {noreply, S}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(normal, _State) ->
    ok;
terminate(_Reason, _State) ->
    io:format(standard_error, "echo[~p]: terminate ~p~n", [self(), _Reason]).

send(Socket, Line) ->
  ok = gen_tcp:send(Socket, Line),
  ok = inet:setopts(Socket, [{active, once}]),
  ok.
