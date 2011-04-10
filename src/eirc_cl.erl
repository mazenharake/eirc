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

-module(eirc_cl).
-include("eirc.hrl").

-compile(export_all).

%% =============================================================================
%% Module API
%% =============================================================================
start(Options) ->
    gen_server:start(?MODULE, Options, []).

start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).

stop(Client) ->
    gen_server:call(Client, stop).

connect(Client, Server, Port) ->
    gen_server:call(Client, {connect, Server, Port}, infinity).

logon(Client, Pass, Nick, User, Name) ->
    gen_server:call(Client, {logon, Pass, Nick, User, Name}, infinity).

msg(Client, Type, Nick, Msg) ->
    gen_server:call(Client, {msg, Type, Nick, Msg}, infinity).

cmd(Client, RawCmd) ->
    gen_server:call(Client, {cmd, RawCmd}).

join(Client, Channel, Key) ->
    gen_server:call(Client, {join, Channel, Key}, infinity).

part(Client, Channel) ->
    gen_server:call(Client, {part, Channel}, infinity).

quit(Client, QuitMsg) ->
    gen_server:call(Client, {quit, QuitMsg}, infinity).

is_logged_on(Client) ->
    gen_server:call(Client, is_logged_on).

channels(Client) ->
    gen_server:call(Client, channels).

chan_users(Client, Channel) ->
    gen_server:call(Client, {chan_users, Channel}).

chan_topic(Client, Channel) ->
    gen_server:call(Client, {chan_topic, Channel}).

chan_type(Client, Channel) ->
    gen_server:call(Client, {chan_type, Channel}).

chan_has_user(Client, Channel, Nick) ->
    gen_server:call(Client, {chan_has_user, Channel, Nick}).

add_handler(Client, Pid) ->
    gen_server:call(Client, {add_handler, Pid}).

remove_handler(Client, Pid) ->
    gen_server:call(Client, {remove_handler, Pid}).

state(Client) ->
    %% Don't expose the record because then people have to include your header
    %% file when they compile, return a proplist instead and let them use that
    State = gen_server:call(Client, state),
    [{server, State#eirc_state.server},
     {port, State#eirc_state.port},
     {nick, State#eirc_state.nick},
     {pass, State#eirc_state.pass},
     {user, State#eirc_state.user},
     {name, State#eirc_state.name},
     {autoping, State#eirc_state.autoping},
     {chprefix, State#eirc_state.chprefix},
     {channels, eirc_chan:to_proplist(State#eirc_state.channels)},
     {network, State#eirc_state.network},
     {login_time, State#eirc_state.login_time},
     {debug, State#eirc_state.debug}].


%% =============================================================================
%% Behaviour callback API
%% =============================================================================
%% INIT
init(Options) ->
    Handlers = proplists:get_value(event_handlers, Options, []),
    Autoping = proplists:get_value(autoping, Options, true),
    Debug = proplists:get_value(debug, Options, false),
    NHandlers = lists:foldl(fun do_add_handler/2, [], Handlers),
    {ok, #eirc_state{ event_handlers = NHandlers, autoping = Autoping,
		      logged_on = false, debug = Debug, 
		      channels = eirc_chan:init() }}.

%% CALLS
handle_call({connect, Server, Port}, _From, State) ->
    case gen_tcp:connect(Server, Port, [list, {packet, line}]) of
	{ok, Socket} ->
	    {reply, ok, State#eirc_state{ server = Server, port = Port, 
					  socket = Socket } };
	Error ->
	    {reply, Error, State}
    end;

handle_call({logon, Pass, Nick, User, Name}, _From, #eirc_state{ logged_on = false } = State) ->
    gen_tcp:send(State#eirc_state.socket, ?PASS(Pass)),
    gen_tcp:send(State#eirc_state.socket, ?NICK(Nick)),
    gen_tcp:send(State#eirc_state.socket, ?USER(User, Name)),
    {reply, ok, State#eirc_state{ pass = Pass, nick = Nick, user = User, name = Name }};

handle_call({add_handler, Pid}, _From, State) ->
    NHandlers = do_add_handler(Pid, State#eirc_state.event_handlers),
    {reply, ok, State#eirc_state{ event_handlers = NHandlers }};

handle_call({remove_handler, Pid}, _From, State) ->
    NHandlers = do_remove_handler(Pid, State#eirc_state.event_handlers),
    {reply, ok, State#eirc_state{ event_handlers = NHandlers }};

handle_call(state, _From, State) ->
    {reply, State, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(is_logged_on, _From, State) ->
    {reply, State#eirc_state.logged_on, State};

handle_call(_, _From, #eirc_state{ logged_on = false } = State) ->
    {reply, {error, not_connected}, State};

handle_call({quit, QuitMsg}, _From, State) ->
    gen_tcp:send(State#eirc_state.socket, ?QUIT(QuitMsg)),
    {reply, ok, State};

handle_call({msg, Type, Nick, Msg}, _From, State) ->
    Data = case Type of
	       privmsg -> ?PRIVMSG(Nick, Msg);
	       notice -> ?NOTICE(Nick, Msg)
	   end,
    gen_tcp:send(State#eirc_state.socket, Data),
    {reply, ok, State};

handle_call({join, Channel, Key}, _From, State) ->
    gen_tcp:send(State#eirc_state.socket, ?JOIN(Channel, Key)),
    {reply, ok, State};

handle_call({part, Channel}, _From, State) ->
    gen_tcp:send(State#eirc_state.socket, ?PART(Channel)),
    {reply, ok, State};

handle_call({cmd, RawCmd}, _From, State) ->
    gen_tcp:send(State#eirc_state.socket, ?CMD(RawCmd)),
    {reply, ok, State};

handle_call(channels, _From, State) ->
    {reply, eirc_chan:channels(State#eirc_state.channels), State};

handle_call({chan_users, Channel}, _From, State) ->
    {reply, eirc_chan:chan_users(State#eirc_state.channels, Channel), State};

handle_call({chan_topic, Channel}, _From, State) ->
    {reply, eirc_chan:chan_topic(State#eirc_state.channels, Channel), State};

handle_call({chan_type, Channel}, _From, State) ->
    {reply, eirc_chan:chan_type(State#eirc_state.channels, Channel), State};

handle_call({chan_has_user, Channel, Nick}, _From, State) ->
    {reply, eirc_chan:chan_has_user(State#eirc_state.channels, Channel, Nick), State}.

%% CAST
handle_cast(_Cast, State) ->
    {noreply, State}.

%% INFO
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};

handle_info({tcp_error, Socket}, State) ->
    {stop, {tcp_error, Socket}, State};

handle_info({tcp, _, Data}, State) ->
    case eirc_lib:parse(Data) of
	#ircmsg{ ctcp = true } = Msg ->
	    send_event(Msg, State),
	    handle_ctcp(Msg, State);
	#ircmsg{ ctcp = false } = Msg ->
	    send_event(Msg, State),
	    handle_data(Msg, State);
	#ircmsg{ ctcp = invalid } = Msg when State#eirc_state.debug == true ->
	    send_event(Msg, State),
	    {noreply, State};
	_ ->
	    {noreply, State}
    end;

handle_info({'DOWN', _, _, Pid, _}, State) ->
    NHandlers = do_remove_handler(Pid, State#eirc_state.event_handlers),
    {noreply, State#eirc_state{ event_handlers = NHandlers }};

handle_info(die, _) ->
    exit(dying);

handle_info(_, State) ->
    {noreply, State}.

%% TERMINATE
terminate(_Reason, _State) ->
    ok.

%% CODE CHANGE
code_change(_Old, State, _Extra) ->
    {ok, State}.

%% =============================================================================
%% Data handling logic
%% =============================================================================
%% Sucessfully logged in
handle_data(#ircmsg{ cmd = ?RPL_WELCOME },
	    #eirc_state{ logged_on = false } = State) ->
    {noreply, State#eirc_state{ logged_on = true, 
			   login_time = erlang:now() }};

%% Server capabilities
handle_data(#ircmsg{ cmd = ?RPL_ISUPPORT } = Msg, State) ->
    {noreply, eirc_lib:isup(Msg#ircmsg.args, State)};

%% We entered a channel
handle_data(#ircmsg{ nick = Nick, cmd = "JOIN" } = Msg,
	    #eirc_state{ nick = Nick } = State) ->
    Channels = eirc_chan:join(State#eirc_state.channels, hd(Msg#ircmsg.args)),
    {noreply, State#eirc_state{ channels = Channels }};

%% Someone joined a channel we are in
handle_data(#ircmsg{ nick = UserNick, cmd = "JOIN"} = Msg, State) ->
    Channels = eirc_chan:user_join(State#eirc_state.channels, hd(Msg#ircmsg.args),
				   UserNick),
    {noreply, State#eirc_state{ channels = Channels }};

%% Topic message on join
%% 3 arguments is not RFC compliant but _very_ common
%% 2 arguments is RFC compliant
handle_data(#ircmsg{ cmd = ?RPL_TOPIC } = Msg, State) ->
    case Msg#ircmsg.args of
	[_Nick, Channel, Topic] -> ok;
	[Channel, Topic] -> ok
    end,
    Channels = eirc_chan:set_topic(State#eirc_state.channels, Channel, Topic),
    {noreply, State#eirc_state{ channels = Channels }};

%% Topic message while in channel
handle_data(#ircmsg{ cmd = "TOPIC", args = [Channel, Topic] }, State) ->
    Channels = eirc_chan:set_topic(State#eirc_state.channels, Channel, Topic),
    {noreply, State#eirc_state{ channels = Channels }};

%% NAMES reply
handle_data(#ircmsg{ cmd = ?RPL_NAMREPLY } = Msg, State) ->
    case Msg#ircmsg.args of
	[_Nick, ChanType, Channel, Names] -> ok;
	[ChanType, Channel, Names] -> ok
    end,
    Channels = eirc_chan:set_type(
		 eirc_chan:users_join(State#eirc_state.channels,
				      Channel, string:tokens(Names, " ")),
		 Channel, ChanType),    
    {noreply, State#eirc_state{ channels = Channels }};

%% We successfully changed name 
handle_data(#ircmsg{ cmd = "NICK", nick = Nick, args = [NewNick] },
	    #eirc_state{ nick = Nick } = State) ->
    {noreply, State#eirc_state{ nick = NewNick }};

%% Someone we know (or can see) changed name
handle_data(#ircmsg{ cmd = "NICK", nick = Nick, args = [NewNick] }, State) ->
    Channels = eirc_chan:user_rename(State#eirc_state.channels, Nick, NewNick),
    {noreply, State#eirc_state{ channels = Channels }};

%% We left a channel
handle_data(#ircmsg{ nick = Nick, cmd = "PART" } = Msg,
	    #eirc_state{ nick = Nick } = State) ->
    Channels = eirc_chan:part(State#eirc_state.channels, hd(Msg#ircmsg.args)),
    {noreply, State#eirc_state{ channels = Channels }};

%% Someone left a channel we are in
handle_data(#ircmsg{ nick = UserNick, cmd = "PART" } = Msg, State) ->
    Channels = eirc_chan:user_part(State#eirc_state.channels, hd(Msg#ircmsg.args),
				   UserNick),
    {noreply, State#eirc_state{ channels = Channels }};
    
%% We got a ping, reply if autoping is on.
handle_data(#ircmsg{ cmd = "PING" } = Msg, #eirc_state{ autoping = true } = State) ->
    case Msg of
	#ircmsg{ args = [From] } ->
	    gen_tcp:send(State#eirc_state.socket, ?PONG2(State#eirc_state.nick, From));
	_ ->
	    gen_tcp:send(State#eirc_state.socket, ?PONG1(State#eirc_state.nick))
    end,
    {noreply, State};

handle_data(#ircmsg{ cmd = "ERROR", args = ["Closing Link"++_] }, State) ->
    {stop, normal, State};

%% "catch-all", period. (DEV)
handle_data(_Msg, State) ->
    {noreply, State}.

%% =============================================================================
%% CTCP
%% =============================================================================
handle_ctcp(#ircmsg{ cmd = "VERSION" } = Msg, State) ->
    gen_tcp:send(State#eirc_state.socket, ?NOTICE(Msg#ircmsg.nick, 
					     ?RPL_CTCP_VERSION)),
    {noreply, State};
handle_ctcp(#ircmsg{ cmd = "TIME" } = Msg, State) ->
    gen_tcp:send(State#eirc_state.socket, ?NOTICE(Msg#ircmsg.nick, 
					     ?RPL_CTCP_TIME)), 
    {noreply, State};
handle_ctcp(#ircmsg{ cmd = "PING", args = [Timestamp] } = Msg, State) ->
    gen_tcp:send(State#eirc_state.socket, 
		 ?NOTICE(Msg#ircmsg.nick, ?RPL_CTCP_PING(Timestamp))),
    {noreply, State};
handle_ctcp(_Msg, State) ->
    {noreply, State}.

%% =============================================================================
%% Internal functions
%% =============================================================================
send_event(Msg, #eirc_state{ event_handlers = Handlers }) 
  when is_list(Handlers) ->
    lists:foreach(fun({Pid, _}) -> Pid ! Msg end, Handlers).

gv(Key, Options) -> proplists:get_value(Key, Options).
gv(Key, Options, Default) -> proplists:get_value(Key, Options, Default).

do_add_handler(Pid, Handlers) ->
    case erlang:is_process_alive(Pid) andalso not lists:member(Pid, Handlers) of
	true ->
	    Ref = erlang:monitor(process, Pid),
	    [{Pid, Ref}|Handlers];
	false ->
	    Handlers
    end.

do_remove_handler(Pid, Handlers) ->
    case lists:keyfind(Pid, 1, Handlers) of
	{Pid, Ref} ->
	    erlang:demonitor(Ref),
	    lists:keydelete(Pid, 1, Handlers);
	false ->
	    Handlers
    end.
    
