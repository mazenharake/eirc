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

-record(st, { cb, cbstate, iphost, port, nick, clpid, exiting }).

%% =============================================================================
%% Application API
%% =============================================================================
start_link(IpHost, Port, Nick, Options, Args) ->
    CBMod = proplists:get_value(callback, Options),
    InitArg = {callback(CBMod), IpHost, Port, Nick, Options, Args},
    case proplists:get_value(register, Options) of
	undefined ->
	    gen_server:start_link(?MODULE, InitArg, []);
	Name ->
	    gen_server:start_link(Name, ?MODULE, InitArg, [])
    end.

call(Server, Msg) ->
    gen_server:call(Server, Msg).

call(Server, Msg, Timeout) ->
    gen_server:call(Server, Msg, Timeout).

init({CB, IpHost, Port, Nick, Options, Args}) ->
    process_flag(trap_exit, true),
    {ok, ClPid} = eirc_cl:start_link(Options),
    eirc_cl:add_handler(ClPid, self()),
    Pass = proplists:get_value(logon_pass, Options, "nopass"),
    User = proplists:get_value(logon_user, Options, Nick),
    Name = proplists:get_value(logon_name, Options, "No Name"),
    case CB(init, [ClPid, Args]) of
	{ok, State} ->
	    case eirc_cl:connect(ClPid, IpHost, Port) of
		ok -> 
		    eirc_cl:logon(ClPid, Pass, Nick, User, Name),
		    {ok, #st{ cb = CB, cbstate = State, iphost = IpHost, 
			      port = Port, nick = Nick, clpid = ClPid,
			      exiting = false }};
		Error ->
		    {stop, Error}
	    end;
	{stop, Reason} ->
	    {stop, Reason}
    end.

handle_call(Call, From, State) ->
    case (State#st.cb)(handle_call, [Call, From, State#st.cbstate]) of
	{reply, Reply, NCBState} ->
	    {reply, Reply, State#st{ cbstate = NCBState }};
	{stop, Reason, Reply, NCBState} ->
	    {stop, Reason, Reply, State#st{ cbstate = NCBState }}
    end.
	 
handle_cast(_Cast, State) ->
    {no_reply, State}.

handle_info(#ircmsg{} = IrcMsg, State) ->
    case handle_ircmsg(IrcMsg, State) of
	{ok, NCBState} ->
	    {noreply, State#st{ cbstate = NCBState }};
	{stop, Reason} ->
	    {stop, Reason, State}
    end;
handle_info({'EXIT', Pid, normal}, #st{ clpid = Pid } = State) ->
    {stop, normal, State};
handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, State) ->
    (State#st.cb)(terminate, [Reason, State#st.cbstate]).


handle_ircmsg(#ircmsg{ cmd = ?RPL_WELCOME }, State) ->
    (State#st.cb)(on_connect, [State#st.cbstate]);
handle_ircmsg(#ircmsg{ ctcp = true } = IrcMsg, State) ->
    (State#st.cb)(on_ctcp, [IrcMsg#ircmsg.nick, IrcMsg#ircmsg.cmd,
			    IrcMsg#ircmsg.args, State#st.cbstate]);
handle_ircmsg(#ircmsg{ cmd = "PRIVMSG" } = IrcMsg, State) ->
    From = IrcMsg#ircmsg.nick,
    [To|Msg] = IrcMsg#ircmsg.args,
    (State#st.cb)(on_text, [From, To, lists:flatten(Msg), State#st.cbstate]);
handle_ircmsg(#ircmsg{ cmd = "NOTICE" } = IrcMsg, State) ->
    From = IrcMsg#ircmsg.nick,
    [To|Msg] = IrcMsg#ircmsg.args,
    (State#st.cb)(on_notice, [From, To, lists:flatten(Msg), State#st.cbstate]);
handle_ircmsg(#ircmsg{ cmd = "JOIN" } = IrcMsg, State) ->
    User = IrcMsg#ircmsg.nick,
    Channel = hd(IrcMsg#ircmsg.args),
    (State#st.cb)(on_join, [User, Channel, State#st.cbstate]);
handle_ircmsg(#ircmsg{ cmd = "PART" } = IrcMsg, State) ->
    User = IrcMsg#ircmsg.nick,
    Channel = hd(IrcMsg#ircmsg.args),
    (State#st.cb)(on_part, [User, Channel, State#st.cbstate]);
handle_ircmsg(IrcMsg, State) ->
    io:format("IRCMSG: ~1000p~n", [IrcMsg]),
    {ok, State#st.cbstate}.





callback(Module) ->
    fun(Function, Args) ->
	    erlang:apply(Module, Function, Args)
    end.

%% =============================================================================
%% Behaviour API
%% =============================================================================
behaviour_info(callbacks) ->
    [{init, 2},
     {on_connect, 1},
     {on_text, 4},
     {on_notice, 4},
     {on_join, 3},
     {on_part, 3},
     {on_ctcp, 4},
     {handle_call, 3},
     {terminate, 2}];
behaviour_info(_) -> undefined.

    
