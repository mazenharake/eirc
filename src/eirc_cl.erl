%% Copyright (c) 2010, Mazen Harake
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%%     * Redistributions of source code must retain the above copyright notice,
%%       this list of conditions and the following disclaimer.
%%     * Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     * Neither the name of the <ORGANIZATION> nor the names of its
%%       contributors may be used to endorse or promote products derived from
%%       this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

-module(eirc_cl).
-include("eirc.hrl").

%% Module API
%% -export([connect/4, connect_link/4, disconnect/2]).
-compile(export_all).

%% Private API
%% -export([init/1]).

%% Records
-record(state, { event_receiver, server, port, socket, nick, pass, user, name, 
		 logged_in, waiting, autoping }).

%% =============================================================================
%% Module API
%% =============================================================================
start_link(Options) ->
    gen_server:start_link(?MODULE, {self(), Options}, []).

connect(Client, Server, Port) ->
    gen_server:call(Client, {connect, Server, Port}, infinity).

logon(Client, Pass, Nick, User, Name) ->
    gen_server:call(Client, {logon, Pass, Nick, User, Name}, infinity).

quit(Client, QuitMessage) ->
    gen_server:call(Client, {quit, QuitMessage}).


%% =============================================================================
%% Behaviour callback API
%% =============================================================================
init({StarterPid, Options}) ->
    EventPid = proplists:get_value(event_receiver, Options, StarterPid),
    Autoping = proplists:get_value(autoping, Options, true),
    {ok, #state{ event_receiver = EventPid, autoping = Autoping,
		 logged_in = false }}.

handle_call({connect, Server, Port}, _From, State) ->
    case gen_tcp:connect(Server, Port, [list, {packet, line}]) of
	{ok, Socket} ->
	    {reply, ok, State#state{ server = Server, port = Port, 
				     socket = Socket } };
	Error ->
	    {reply, Error, State}
    end;
handle_call({logon, Pass, Nick, User, Name}, From, State) ->
    gen_tcp:send(State#state.socket, ?PASS(Pass)),
    gen_tcp:send(State#state.socket, ?NICK(Nick)),
    gen_tcp:send(State#state.socket, ?USER(User, Name)),
    {noreply, State#state{ pass = Pass, nick = Nick, user = User,
			   name = Name, waiting = From }};
handle_call({quit, QuitMessage}, _From, State) ->
    gen_tcp:send(State#state.socket, ?QUIT(QuitMessage)),
    {noreply, State};
handle_call(_, _, State) ->
    {reply, ok, State}.

handle_info({tcp_closed, Socket}, State) ->
    {stop, {tcp_closed, Socket}, State};
handle_info({tcp_error, Socket}, State) ->
    {stop, {tcp_error, Socket}, State};
handle_info({tcp, _, Data}, State) ->
    Msg = parse(Data),
    send_event(Msg, State),
    handle_data(Msg, State);
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

handle_cast(_Cast, State) ->
    {noreply, State}.

code_change(_Old, State, _Extra) ->
    {ok, State}.

%% =============================================================================
%% Data handling logic
%% =============================================================================
%% Message before we sent the PASS, NICK, USER (PNU) combo
handle_data(_Msg, #state{ logged_in = false, waiting = undefined } = State) ->
    {noreply, State};

%% MOTD after PNU was sent = successful
handle_data(#ircmsg{ cmd = ?RPL_WELCOME },
	    #state{ logged_in = false } = State) ->
    gen_server:reply(State#state.waiting, ok),
    {noreply, State#state{ logged_in = true, waiting = undefined }};

%% Possible PNU error _or_ a Message in between
handle_data(#ircmsg{ cmd = Cmd } = Msg,
	    #state{ logged_in = false } = State) ->
    case lists:member(Cmd, ?LOGON_ERRORS) of
	true ->
	    gen_server:reply(State#state.waiting, {error, Msg}),
	    {noreply, State#state{ waiting = undefined, logged_in = false }};
	false ->
	    {noreply, State}
    end;

%% We got a ping, reply if autoping is on.
handle_data(#ircmsg{ cmd = "PING" } = Msg, #state{ autoping = true } = State) ->
    case Msg of
	#ircmsg{ args = [From] } ->
	    gen_tcp:send(State#state.socket, ?PONG2(State#state.nick, From));
	_ ->
	    gen_tcp:send(State#state.socket, ?PONG1(State#state.nick))
    end,
    {noreply, State};



%% "catch-all" while logged in
handle_data(_Msg, #state{ logged_in = true } = State) ->
    {noreply, State};

%% "catch-all", period. (DEV)
handle_data(_Msg, State) ->
    {noreply, State}.

%% =============================================================================
%% Internal functions
%% =============================================================================
send_event(_Msg, #state{ event_receiver = undefined }) -> ok;
send_event(Msg, #state{ event_receiver = EventPid }) when is_pid(EventPid) ->
    EventPid ! Msg;
send_event(Msg, #state{ event_receiver = EventMod }) when is_atom(EventMod) ->
    EventMod:handle_event(Msg).

gv(Key, Options) -> proplists:get_value(Key, Options).
gv(Key, Options, Default) -> proplists:get_value(Key, Options, Default).

%% =============================================================================
%% IRC message parse
%% =============================================================================
parse(UnstrippedData) ->
    Data = string:substr(UnstrippedData,1,length(UnstrippedData)-2),
    case Data of
	[$:|_] ->
	    [[$:|From]|RestData] = string:tokens(Data," "),
	    getcmd(RestData, parsefrom(From, #ircmsg{}));
	Data ->
	    getcmd(string:tokens(Data," "), #ircmsg{})
    end.

parsefrom(FromStr, Msg) ->
    case re:split(FromStr, "(!|@)",[{return, list}]) of
	[Nick, "!", User, "@", Host] ->
	    Msg#ircmsg{ nick = Nick, user = User, host = Host };
	[Nick, "@", Host] ->
	    Msg#ircmsg{ nick = Nick, host = Host };
	[Server] ->
	    %% No nick detection... we are assuming it is the server here but it
	    %% could just as well be a user nick (let someone else decide)
	    Msg#ircmsg{ server = Server }
    end.

getcmd([Cmd|RestData], Msg) ->
    getargs(RestData, Msg#ircmsg{ cmd = Cmd }).

getargs([], Msg) ->
    Msg#ircmsg{ args = lists:reverse(Msg#ircmsg.args) };
getargs([[$:|FirstArg]|RestArgs], Msg) ->
    case lists:flatten([" "++Arg||Arg<-[FirstArg|RestArgs]]) of
	[_|[]] ->
	    getargs([], Msg#ircmsg{ args = [""|Msg#ircmsg.args] });
	[_|FullTrail] ->
	    getargs([], Msg#ircmsg{ args = [FullTrail|Msg#ircmsg.args] })
    end;
getargs([Arg|[]], Msg) ->
    getargs([], Msg#ircmsg{ args = ["",Arg|Msg#ircmsg.args] });
getargs([Arg|RestData], Msg) ->
    getargs(RestData, Msg#ircmsg{ args = [Arg|Msg#ircmsg.args] }).
