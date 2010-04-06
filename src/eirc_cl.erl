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
-export([connect/4, connect_link/4, disconnect/2]).

%% Private API
-export([init/5]).

%% Records
-record(state, { server, port, nick, socket, ctrlpid, pass, name, rf = false }).

%% =============================================================================
%% Module API
%% =============================================================================
connect_link(Server, Port, Nick, Options) ->
    proc_lib:start_link(?MODULE, init, [self(), Server, Port, Nick, Options]).

connect(Server, Port, Nick, Options) ->
    proc_lib:start(?MODULE, init, [self(), Server, Port, Nick, Options]).

disconnect(Client, QuitMsg) ->
    Client ! {send, ?QUIT(QuitMsg)}.

%% =============================================================================
%% Client Process Loop
%% =============================================================================
init(Parent, Server, Port, Nick, Options) ->
    case gen_tcp:connect(Server, Port, [list, {packet, line}]) of
	{ok, Socket} ->
	    proc_lib:init_ack(Parent, {ok, self()}),
	    St = #state{ socket = Socket, nick = Nick, server = Server,
			 port = Port, rf = false,
			 ctrlpid = gv(ctrlpid, Options, Parent),
			 pass = gv(pass, Options, "NOPASS"),
			 name = gv(name, Options, "eirc" ++ pid_to_list(self()))
		       },
	    init_connection(St);
	{error, Reason} ->
	    proc_lib:init_ack(Parent, {error, Reason})
    end.

init_connection(State) ->
    %% What a stupid protocol... we won't get an individual response here.
    %% Just fire off and pray, we find out later if we failed or not
    self() ! {send, ?PASS(State#state.pass)},
    self() ! {send, ?NICK(State#state.nick)},
    self() ! {send, ?USER(State#state.nick, State#state.name)},
    loop(State).

loop(State) ->
    receive
	{tcp, _Socket, Data} ->
	    loop(handle_data(State, parse(Data)));
	{send, Command} ->
	    io:format("SEND: ~1000p ~n",[Command]),
	    ok = gen_tcp:send(State#state.socket, Command),
	    loop(State);
	{tcp_closed, _Socket} ->
	    exit({disconnected, undefined});
	{tcp_error, _Socket, Reason} ->
	    exit({disconnected, Reason})
    end.

handle_data(#state{ rf = false } = State, #ircmsg{ cmd = 001 }) ->
    %% This means the initial commands where successful... we are registered!
    State#state{ rf = true };
handle_data(_State, #ircmsg{ cmd = "QUIT" } = Msg) ->
    exit(normal);
handle_data(State, Msg) ->
    State#state.ctrlpid ! Msg,
    State.


%% =============================================================================
%% Internal Functions
%% =============================================================================
gv(Key, Options) -> proplists:get_value(Key, Options).
gv(Key, Options, Default) -> proplists:get_value(Key, Options, Default).

%% =============================================================================
%% IRC message parse
%% =============================================================================
parse(Data) ->
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
	[Nick, "!", User] ->
	    Msg#ircmsg{ nick = Nick, user = User };
	[Nick, "@", Host] ->
	    Msg#ircmsg{ nick = Nick, host = Host };
	[Server] ->
	    Msg#ircmsg{ server = Server }
    end.

getcmd([[A,B,C]|RestData], Msg) ->
    Cmd = try list_to_integer([A,B,C]) catch error:badarg -> [A,B,C] end,
    getargs(RestData, Msg#ircmsg{ cmd = Cmd });
getcmd([Cmd|RestData], Msg) ->
    getargs(RestData, Msg#ircmsg{ cmd = Cmd }).

getargs([], Msg) ->
    Msg#ircmsg{ args = lists:reverse(Msg#ircmsg.args) };
getargs([[$:|FirstArg]|RestArgs], Msg) ->
    case lists:flatten([" "++Arg||Arg<-[FirstArg|RestArgs]]) of
	[_|[]] ->
	    getargs([], Msg);
	[_|FullTrail] ->
	    getargs([], Msg#ircmsg{ args = [FullTrail|Msg#ircmsg.args] })
    end;
getargs([Arg|RestData], Msg) ->
    getargs(RestData, Msg#ircmsg{ args = [Arg|Msg#ircmsg.args] }).

    




