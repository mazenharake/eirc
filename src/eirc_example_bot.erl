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

%% This is an example of a bot making use of the eirc library. It connects
%% to chat.freenode.net and waits for commands. 

-module(eirc_example_bot).

-behaviour(gen_eircbot).

-include("eirc.hrl").

-export([init/2, on_connect/1, on_text/4, on_notice/4, on_join/3, on_part/3,
	 on_ctcp/4, on_mode/5, on_topic/4, on_ping/1, on_nick/3, on_raw/3,
	 on_kick/5, on_quit/3, handle_call/3, terminate/2]).

-compile(export_all).

-record(botstate, { cl, nick, waiting }).

-define(VERSION, "EIRC Example Bot 0.2").

%% Options can contain: {local, Name::atom()}
%% 
start_link(IpHost, Port, Nick, _Options, Args) ->
    application:start(eirc),
    Options = [{register, {local, ?MODULE}}, {callback, ?MODULE}],
    gen_eircbot:start_link(IpHost, Port, Nick, Options, Args).

print_state() ->
    gen_eircbot:call(?MODULE, print_state).

stop(QuitMsg) ->
    gen_eircbot:call(?MODULE, {stop, QuitMsg}).

init(Client, _Args) ->
    io:format("Initiating bot...~n"),
    {ok, #botstate{ cl = Client }}.

on_connect(State) ->
    io:format("Bot logged on...~n"),
    BotNick = proplists:get_value(nick, eirc:state(State#botstate.cl)),
    {ok, State#botstate{ nick = BotNick }}.

on_text(_, _, "!JOIN "++Channel, State) ->
    eirc:join(State#botstate.cl, Channel),
    {ok, State};
on_text(_, _, "!PART "++Channel, State) ->
    eirc:part(State#botstate.cl, Channel),
    {ok, State};
on_text(_, _, "!CTCP "++Msg, State) ->
    io:format("CTCP ~p >> ~p ~n",["mazenharake", Msg]),
    eirc:ctcp(State#botstate.cl, "mazenharake", Msg),
    {ok, State};
on_text(From, To, Text, State) ->
    io:format("TEXT: From (~p) To (~p) - ~p ~n", [From, To, Text]),
    eirc:privmsg(State#botstate.cl, From, io_lib:format("You sent me: \"~s\"",[Text])),
    {ok, State}.

on_notice(From, To, Text, State) ->
    io:format("NOTICE: From (~p) To (~p) - ~p ~n", [From, To, Text]),
    {ok, State}.

on_join(User, Channel, State) ->
    io:format("JOIN: ~p joined ~p~n", [User, Channel]),
    {ok, State}.

on_part(User, Channel, State) ->
    io:format("PART: (~p) ~p parted ~p~n", [State#botstate.nick, User, Channel]),
    {ok, State}.

on_ctcp(User, "VERSION", _Args, State) ->
    eirc:ctcp(State#botstate.cl, User, "VERSION "++?VERSION),
    {ok, State};
on_ctcp(User, Cmd, Args, State) ->
    io:format("CTCP: ~p:~p - ~p~n", [User, Cmd, Args]),
    {ok, State}.

on_mode(ServerOrNick, TargetChanOrNick, ModeFlags, ModeParameters, State) ->
    io:format("MODE: ~p sets ~p on ~p (parameters: ~p)~n", 
	      [ServerOrNick, ModeFlags, TargetChanOrNick, ModeParameters]),
    {ok, State}.

on_topic(Nick, Channel, Topic, State) ->
    io:format("TOPIC: (~p) ~p set topic to: ~p~n", [Channel, Nick, Topic]),
    {ok, State}.

on_ping(State) ->
    io:format("PING? PONG!~n"),
    {ok, State}.

on_kick(User, Channel, TargetUser, Reason, State) ->
    io:format("KICK: ~p kicked ~p from ~p, reason: ~p~n", [User, TargetUser, Channel, Reason]),
    {ok, State}.

on_nick(OldNick, NewNick, State) ->
    io:format("NICK: ~p is now known as ~p~n",[OldNick, NewNick]),
    {ok, State}.

on_quit(Nick, QuitMsg, State) ->
    io:format("QUIT: ~p quit (~1000p)~n",[Nick, QuitMsg]),
    {ok, State}.

on_raw(Cmd, Args, State) ->
    io:format("RAW: ~p; ~1000p~n",[Cmd, Args]),
    {ok, State}.

handle_call({msg, _To}, _From, State) ->    
    {reply, ok, State};
handle_call(print_state, _From, State) ->
    CState = eirc:state(State#botstate.cl),
    io:format("IRC Client State: ~n~p~n",[CState]),
    {reply, ok, State};
handle_call({stop, QuitMsg}, _From, State) ->
    io:format("Stopping bot...~n"),
    eirc:quit(State#botstate.cl, QuitMsg),
    {stop, normal, ok, State}.

terminate(Reason, _State) ->
    io:format("Bot terminating ~p...~n", [Reason]),
    ok.
 


%% This happens when the bot gets a normal message sent to it
%% The first argument in #ircmsg.args is very often the bot's own nickname but
%% sometimes not. This is not consistent among servers so just ignore it.
%% The second argument is the message string; if it starts with a "!" then this
%% should be considered a command.
handle_info(#ircmsg{ cmd = "PRIVMSG", args = [_,[$!|BotCmd]] } = IMsg, State) ->
    io:format("CMD<~s> !~s~n",[IMsg#ircmsg.nick, BotCmd]),
    NState = do_cmd(State, IMsg#ircmsg{ args = string:tokens(BotCmd," ") }),
    {noreply, NState};

%% Any other normal message is sent here... reply with an error
handle_info(#ircmsg{ cmd = "PRIVMSG" } = IMsg, State) ->
    error_response(State, IMsg),
    {noreply, State};

%% If a pid exits normally (see do_cmd further down) then that is all good
handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};

%% If a pid crashes then check if there is a user waiting for a response and if
%% so then reply the error to that user. In this simple bot there will always be
%% someone waiting but you get the idea.
handle_info({'EXIT', Pid, Reason}, State) ->
    case lists:keysearch(Pid, 1, State#botstate.waiting) of
	{value, {Pid, IMsg}} ->
	    eirc:privmsg(State#botstate.cl, IMsg#ircmsg.nick, 
			 io_lib:format("ERROR: ~1000p",[Reason])),
	    NWaiting = lists:keydelete(Pid, 1, State#botstate.waiting),
	    {noreply, State#botstate{ waiting = NWaiting }};
	false ->
	    {noreply, State}
    end;

%% If we get a message from a pid containing 'response' then this is a pid
%% replying back after it has finished its work. Check if there is a user
%% waiting for a response and send the response back
handle_info({response, Pid, Result}, State) ->
    case lists:keysearch(Pid, 1, State#botstate.waiting) of
	{value, {Pid, IMsg}} ->
	    eirc:privmsg(State#botstate.cl, IMsg#ircmsg.nick,
			 io_lib:format("~1000p",[Result])),
	    NWaiting = lists:keydelete(Pid, 1, State#botstate.waiting),
            {noreply, State#botstate{ waiting = NWaiting }};
	false ->
	    {noreply, State}
    end;

%% Anything else... just ignore it!
handle_info(_, State) ->
    {noreply, State}.

%terminate(_, _) ->
%    ok.

%% This is where all the commands are handled. This is a _very_ basic way of
%% checking commands. Probably this would be more advanced based on what the
%% bot is suppose to do. Only the first part of the message is read here.

%% We got a command asking for VERSION, simply reply
do_cmd(State, #ircmsg{ args = ["VERSION"|_] } = IMsg) ->
    eirc:privmsg(State#botstate.cl, IMsg#ircmsg.nick, ?VERSION),
    State;

%% NODE query, reply with node name
do_cmd(State, #ircmsg{ args = ["NODE"|_] } = IMsg) ->
    eirc:privmsg(State#botstate.cl, IMsg#ircmsg.nick, atom_to_list(node())),
    State;

%% We got a request to do a heavy operation and calculate something. the
%% intention here is to demonstrate the asynch way of responding to a message
%% Receive the event and spawn a process to do the heavy calculation (or what
%% ever it is that might take a long time) and put that process in a waiting
%% list to indicate that the client is expecting as resposne. See handle_info
%% above which accepts a response-tuple.
do_cmd(State, #ircmsg{ args = ["CALCULATE"|_] } = IMsg) ->
    Pid = spawn_link(?MODULE, calculate, [self()]),
    State#botstate{ waiting = [{Pid, IMsg}|State#botstate.waiting] };

%% Same as previous clause but this one intentionally fails. See the 
%% handle_info above which deals with trapping exits
do_cmd(State, #ircmsg{ args = ["CRASH"|_] } = IMsg) ->
    Pid = spawn_link(?MODULE, crash, [self()]),
    State#botstate{ waiting = [{Pid, IMsg}|State#botstate.waiting] };

%% This demonstrates how to create a non-secure implementation of calling an
%% MFA. No checks!! (try init:stop() :))
do_cmd(State, #ircmsg{ args = ["MFA",StrM,StrF|UnparsedArgs] } = IMsg) ->
    try
	M = list_to_atom(StrM),
	F = list_to_atom(StrF),
	StrArgs = tl(lists:flatten([" "++Arg||Arg<-UnparsedArgs]))++".",
	case erl_scan:string(StrArgs) of
	    {ok, Tokens, _} ->
		{ok, Args} = erl_parse:parse_term(Tokens),
		R = erlang:apply(M,F,Args),
		eirc:privmsg(State#botstate.cl, IMsg#ircmsg.nick,
			     io_lib:format("~1000p",[R]));
	    Error ->
		exit({argument_error,Error})
	end
    catch
	Class:Reason ->
	    eirc:privmsg(State#botstate.cl, IMsg#ircmsg.nick,
			 io_lib:format("ERROR: ~p:~1000p - ~1000p",
				       [Class,Reason,erlang:get_stacktrace()]))
    end,
    State;

%% Everything else is ignored and an error is sent
do_cmd(State, IMsg) ->
    error_response(State, IMsg),
    State.

%% Sends a message to the user saying the command was unknown
error_response(State, IMsg) ->
    eirc:privmsg(State#botstate.cl, IMsg#ircmsg.nick, "ERROR: UNKNOWN COMMAND").

%% This is the entry point for the process called when specifying the 
%% "CALCULATE" command to this bot.
calculate(Parent) ->
    timer:sleep(2000),
    Parent ! {response, self(), random:uniform()}.

%% Entry point for pid spawned in the "CRASH" command.
crash(_Parent) ->
    timer:sleep(2000),
    exit({error, a_crash}).

%% Waits until the fun F is true or times out.
wait(N, _) when N =< 0 -> exit(timeout);
wait(N, F) ->
    case F() of	true -> ok; false -> timer:sleep(500), wait(N-500, F) end.
