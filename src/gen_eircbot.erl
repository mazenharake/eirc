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

-module(gen_eircbot).
-export([behaviour_info/1]).

-include("eirc.hrl").

%% Application API
%% -export([connect/4, connect_link/4, disconnect/2]).
-compile(export_all).

-record(st, { cbmod, cbstate, iphost, port, nick, clpid, exiting, logged_on }).

-define(CTCP_VERSION, "VERSION EIRC-BOT 0.0.1").
-define(CTCP_TIME, "TIME  "++eirc_lib:ctcp_time(calendar:local_time())).
-define(CTCP_PING(TS), "PING "++TS).

-define(MISSING_CALLBACKS, Cb == on_connect; Cb == on_text; 
	    Cb == on_server_notice; Cb == on_notice; Cb == on_join; 
	    Cb == on_part; Cb == on_mode; Cb == on_topic; Cb == on_ping; 
	    Cb == on_kick; Cb == on_nick; Cb == on_raw; Cb == on_quit; 
	    Cb == handle_call; Cb == terminate).

%% =============================================================================
%% Application API
%% =============================================================================
start_link(ClPid, Callback, InitArgs) ->
    gen_server:start_link(?MODULE, {ClPid, Callback, InitArgs}, []).

state(Server) ->
    State = call(Server, {'$gen_eircbot', state}),
    [{cbmod, State#st.cbmod},
     {cbstate, State#st.cbstate}].

call(Server, Msg) ->
    gen_server:call(Server, Msg).

call(Server, Msg, Timeout) ->
    gen_server:call(Server, Msg, Timeout).

init({ClPid, Callback, InitArgs}) ->
    eirc_cl:asynch_add_handler(ClPid, self()),
    {ok, _State} = safe_callback({init, Callback, [ClPid, InitArgs]}, undefined).

handle_call({'$gen_eircbot', state}, _From, State) ->
    {reply, State, State};
handle_call(Call, From, State) ->
    safe_callback({handle_call, [Call, From]}, State).
	 
handle_cast(_Cast, State) ->
    {no_reply, State}.

handle_info(Message, State) ->
    safe_callback(Message, State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, State) ->
    safe_callback({terminate, [Reason]}, State).

safe_callback(Args, State) ->
    try
	callback(get_call_tuple(Args, State))
    catch
	throw:{stop, normal, NState} ->
	    {stop, normal, NState};
	throw:{stop, Reason, NState} ->
	    {stop, Reason, NState};
	error:oundef ->
	    case erlang:get_stacktrace() of
		[{_, on_ctcp, _}|_] ->
		    handle_default_ctcp(Args, State),
		    {noreply, State};
		[{Mod, Cb, _}|_] when ?MISSING_CALLBACKS -> 
		    error_logger:warning_report([{unexported_callback, {Mod, Cb}}]),
		    {noreply, State};
		StackTrace ->
		    erlang:error(undef, StackTrace)
	    end
    end.

get_call_tuple({init, CBMod, Args}, undefined) ->
    {CBMod, init, Args};
get_call_tuple({connect, Server, Port}, State) ->
    {State, on_connect, [Server, Port]};
get_call_tuple({logon, Pass, Nick, User, Name}, State) ->
    {State, on_logon, [Pass, Nick, User, Name]};
get_call_tuple({handle_call, Args}, State) ->
    {State, handle_call, Args};
get_call_tuple({terminate, Args}, State) ->
    {State, terminate, Args};
get_call_tuple(#ircmsg{ cmd = ?RPL_WELCOME } = IrcMsg, State) ->
    {State, on_logon, IrcMsg};
get_call_tuple(#ircmsg{ ctcp = true } = IrcMsg, State) ->
    {State, on_ctcp, [IrcMsg#ircmsg.nick, IrcMsg#ircmsg.cmd, IrcMsg#ircmsg.args]};
get_call_tuple(#ircmsg{ cmd = "PRIVMSG" } = IrcMsg, State) ->
    FromNick = IrcMsg#ircmsg.nick,
    [ToNick|Msg] = IrcMsg#ircmsg.args,
    {State, on_text, [FromNick, ToNick, lists:flatten(Msg)]};
get_call_tuple(#ircmsg{ cmd = "NOTICE", nick = undefined } = IrcMsg, State) ->
    Server = IrcMsg#ircmsg.server,
    [_|Msg] = IrcMsg#ircmsg.args,
    {State, on_server_notice, [Server, Msg]};
get_call_tuple(#ircmsg{ cmd = "NOTICE" } = IrcMsg, State) ->
    FromNick = IrcMsg#ircmsg.nick,
    [ToNick|Msg] = IrcMsg#ircmsg.args,
    {State, on_notice, [FromNick, ToNick, lists:flatten(Msg)]};
get_call_tuple(#ircmsg{ cmd = "JOIN" } = IrcMsg, State) ->
    Nick = IrcMsg#ircmsg.nick,
    Channel = hd(IrcMsg#ircmsg.args),
    {State, on_join, [Nick, Channel]};
get_call_tuple(#ircmsg{ cmd = "PART" } = IrcMsg, State) ->
    Nick = IrcMsg#ircmsg.nick,
    Channel = hd(IrcMsg#ircmsg.args),
    {State, on_part, [Nick, Channel]};
get_call_tuple(#ircmsg{ cmd = "MODE" } = IrcMsg, State) ->
    ServerOrNick = IrcMsg#ircmsg.nick,
    [ChanOrNick,Flags|Parameters] = IrcMsg#ircmsg.args,
    {State, on_mode, [ServerOrNick, ChanOrNick, Flags, Parameters]};
get_call_tuple(#ircmsg{ cmd = "TOPIC" } = IrcMsg, State) ->
    Nick = IrcMsg#ircmsg.nick,
    [Channel|Topic] = IrcMsg#ircmsg.args,
    {State, on_topic, [Nick, Channel, hd(Topic)]};
get_call_tuple(#ircmsg{ cmd = "PING" }, State) ->
    {State, on_ping, []};
get_call_tuple(#ircmsg{ cmd = "KICK" } = IrcMsg, State) ->
    Nick = IrcMsg#ircmsg.nick,
    [Channel, TargetUser, Reason|_] = IrcMsg#ircmsg.args,
    {State, on_kick, [Nick, Channel, TargetUser, Reason]};
get_call_tuple(#ircmsg{ cmd = "NICK" } = IrcMsg, State) ->
    Nick = IrcMsg#ircmsg.nick,
    [NewNick|_] = IrcMsg#ircmsg.args,
    {State, on_nick, [Nick, NewNick]};
get_call_tuple(#ircmsg{ cmd = "QUIT" } = IrcMsg, State) ->
    Nick = IrcMsg#ircmsg.nick,
    QuitMsg = hd(IrcMsg#ircmsg.args),
    {State, on_quit, [Nick, QuitMsg]};
get_call_tuple(#ircmsg{} = IrcMsg, State) ->
    {State, on_raw, IrcMsg};
get_call_tuple(Message, State) ->
    {State, handle_info, [Message]}.

handle_default_ctcp(#ircmsg{ cmd = "VERSION" } = IrcMsg, State) ->
    eirc_cl:msg(State#st.clpid, ctcp, IrcMsg#ircmsg.nick, ?CTCP_VERSION);
handle_default_ctcp(#ircmsg{ cmd = "TIME" } = IrcMsg, State) ->
    eirc_cl:msg(State#st.clpid, ctcp, IrcMsg#ircmsg.nick, ?CTCP_TIME);
handle_default_ctcp(#ircmsg{ cmd = "PING", args = [Timestamp] } = IrcMsg, State) ->
    eirc_cl:msg(State#st.clpid, ctcp, IrcMsg#ircmsg.nick, ?CTCP_PING(Timestamp));
handle_default_ctcp(#ircmsg{ cmd = Cmd } = IrcMsg, State) ->
    eirc_cl:msg(State#st.clpid, ctcp, IrcMsg#ircmsg.nick, Cmd++" N/A").

callback({CBMod, init, [ClPid, Args]}) ->
    case erlang:apply(CBMod, init, [ClPid, Args]) of
	{ok, CBState} ->
	    {ok, #st{ clpid = ClPid, cbmod = CBMod, cbstate = CBState }};
	{stop, Reason} ->
	    throw({stop, Reason, undefined})
    end;
callback({State, on_connect, [IpHost, Port] = Args}) ->
    case erlang:apply(State#st.cbmod, on_connect, Args++[State#st.cbstate]) of
	{ok, CBState} ->
	    {noreply, State#st{ cbstate = CBState, iphost = IpHost, port = Port }};
	{stop, Reason, CBState} ->
	    throw({stop, Reason, State#st{ cbstate = CBState }})
    end;
callback({State, on_logon, Args}) when length(Args) == 4 ->
    case erlang:apply(State#st.cbmod, on_logon, Args++[State#st.cbstate]) of
	{ok, CBState} ->
	    {noreply, State#st{ cbstate = CBState }};
	{stop, Reason, CBState} ->
	    throw({stop, Reason, State#st{ cbstate = CBState }})
    end;
callback({State, handle_call, Args}) ->
    case erlang:apply(State#st.cbmod, handle_call, Args++[State#st.cbstate]) of
	{reply, Reply, CBState} ->
	    {reply, Reply, State#st{ cbstate = CBState }};
	{stop, Reason, Reply, CBState} ->
	    {stop, Reason, Reply, State#st{ cbstate = CBState }}
    end;
callback({State, terminate, Args}) ->
    (catch erlang:apply(State#st.cbmod, terminate, Args++[State#st.cbstate])),
    ok;
callback({State, on_logon, IrcMsg}) ->
    case erlang:apply(State#st.cbmod, on_logon, [State#st.cbstate]) of
	{ok, CBState} ->
	    callback({State#st{ cbstate = CBState }, on_raw, IrcMsg});
	{stop, Reason, CBState} ->
	    throw({stop, Reason, State#st{ cbstate = CBState }})
    end;
callback({State, on_raw, IrcMsg}) ->
    case erlang:apply(State#st.cbmod, on_raw, [IrcMsg#ircmsg.cmd, IrcMsg#ircmsg.args,
					       State#st.cbstate]) of
	{ok, CBState} ->
	    {noreply, State#st{ cbstate = CBState }};
	{stop, Reason, CBState} ->
	    throw({stop, Reason, State#st{ cbstate = CBState }})
    end;
callback({State, CBFunction, Args}) ->
    case erlang:apply(State#st.cbmod, CBFunction, Args++[State#st.cbstate]) of
	{ok, CBState} ->
	    {noreply, State#st{ cbstate = CBState }};
	{stop, Reason, CBState} ->
	    throw({stop, Reason, State#st{ cbstate = CBState }})
    end.    

%% =============================================================================
%% Behaviour API
%% =============================================================================
behaviour_info(callbacks) ->
    [{init, 2}, {on_connect, 3}, {on_logon, 5}, {on_logon, 1}, {on_text, 4},
     {on_server_notice, 3}, {on_notice, 4}, {on_join, 3}, {on_part, 3},
     {on_ctcp, 4}, {on_mode, 5}, {on_topic, 4}, {on_ping, 1}, {on_kick, 5},
     {on_nick, 3}, {on_quit, 3}, {on_ctcp, 4}, {on_raw, 3}, {handle_call, 3},
     {terminate, 2}].
