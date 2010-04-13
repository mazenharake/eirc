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

-compile(export_all).

%% =============================================================================
%% Module API
%% =============================================================================
start(Options) ->
    gen_server:start(?MODULE, {self(), Options}, []).

start_link(Options) ->
    gen_server:start_link(?MODULE, {self(), Options}, []).

connect(Client, Server, Port) ->
    gen_server:call(Client, {connect, Server, Port}, infinity).

logon(Client, Pass, Nick, User, Name) ->
    gen_server:call(Client, {logon, Pass, Nick, User, Name}, infinity).

is_logged_in(Client) ->
    gen_server:call(Client, is_logged_in).

msg(Client, Type, Nick, Msg) ->
    gen_server:call(Client, {msg, Type, Nick, Msg}, infinity).

cmd(Client, RawCmd) ->
    gen_server:call(Client, {cmd, RawCmd}).

join(Client, Channel, Key) ->
    gen_server:call(Client, {join, Channel, Key}, infinity).

quit(Client, QuitMsg) ->
    gen_server:call(Client, {quit, QuitMsg}, infinity).

%% =============================================================================
%% Behaviour callback API
%% =============================================================================
%% INIT
init({StarterPid, Options}) ->
    EventPid = proplists:get_value(event_receiver, Options, StarterPid),
    Autoping = proplists:get_value(autoping, Options, true),
    {ok, #state{ event_receiver = EventPid, autoping = Autoping,
		 logged_in = false }}.

%% CALLS
handle_call({connect, Server, Port}, _From, State) ->
    case gen_tcp:connect(Server, Port, [list, {packet, line}]) of
	{ok, Socket} ->
	    {reply, ok, State#state{ server = Server, port = Port, 
				     socket = Socket } };
	Error ->
	    {reply, Error, State}
    end;

handle_call({logon, Pass, Nick, User, Name}, _From, 
	    #state{ logged_in = false } = State) ->
    gen_tcp:send(State#state.socket, ?PASS(Pass)),
    gen_tcp:send(State#state.socket, ?NICK(Nick)),
    gen_tcp:send(State#state.socket, ?USER(User, Name)),
    {reply, ok, State#state{ pass = Pass, nick = Nick, user = User,
			     name = Name }};

handle_call({quit, QuitMsg}, _From, State) ->
    gen_tcp:send(State#state.socket, ?QUIT(QuitMsg)),
    {reply, ok, State};

handle_call({msg, Type, Nick, Msg}, _From, State) ->
    Data = case Type of
	       privmsg -> ?PRIVMSG(Nick, Msg);
	       notice -> ?NOTICE(Nick, Msg)
	   end,
    gen_tcp:send(State#state.socket, Data),
    {reply, ok, State};

handle_call({join, Channel, Key}, _From, State) ->
    {reply, ok, State};

handle_call({cmd, RawCmd}, _From, State) ->
    gen_tcp:send(State#state.socket, ?CMD(RawCmd)),
    {reply, ok, State};

handle_call(is_logged_in, _From, State) ->
    {reply, State#state.logged_in, State};

handle_call(_, _, State) ->
    {reply, ok, State}.

%% CAST
handle_cast(_Cast, State) ->
    {noreply, State}.

%% INFO
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};

handle_info({tcp_error, Socket}, State) ->
    {stop, {tcp_error, Socket}, State};

handle_info({tcp, _, Data}, State) ->
    Msg = eirc_lib:parse(Data),
    send_event(Msg, State),
    handle_data(Msg, State);

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
%% MOTD after PNU was sent = successful
handle_data(#ircmsg{ cmd = ?RPL_WELCOME },
	    #state{ logged_in = false } = State) ->
    {noreply, State#state{ logged_in = true, 
			   login_time = erlang:now() }};

handle_data(#ircmsg{ cmd = ?RPL_ISUPPORT } = Msg, State) ->
    {noreply, eirc_lib:isup(Msg#ircmsg.args, State)};

%% CTCP request
handle_data(#ircmsg{ cmd = "PRIVMSG", args = [_,[1|CTCP]|Arg] } = Msg, State) ->
    case lists:reverse(CTCP) of
	[1|CTCPRev] -> 
	    handle_ctcp(Msg#ircmsg{ cmd = lists:reverse(CTCPRev), 
				    args = string:tokens(Arg," ") }, State);
	_ ->
	    %% invalid ctcp
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

handle_data(#ircmsg{ cmd = "ERROR", args = ["Closing Link"++_] }, State) ->
    {stop, normal, State};

%% "catch-all", period. (DEV)
handle_data(_Msg, State) ->
    {noreply, State}.

%% =============================================================================
%% CTCP
%% =============================================================================
handle_ctcp(#ircmsg{ cmd = "VERSION" } = Msg, State) ->
    gen_tcp:send(State#state.socket, ?NOTICE(Msg#ircmsg.nick, 
					     ?RPL_CTCP_VERSION)),
    {noreply, State};
handle_ctcp(#ircmsg{ cmd = "TIME" } = Msg, State) ->
    gen_tcp:send(State#state.socket, ?NOTICE(Msg#ircmsg.nick, 
					     ?RPL_CTCP_TIME)), 
    {noreply, State};
handle_ctcp(#ircmsg{ cmd = "PING", args = [Timestamp] } = Msg, State) ->
    gen_tcp:send(State#state.socket, ?RPL_CTCP_PING(Timestamp)),
    {noreply, State};
handle_ctcp(_Msg, State) ->
    {noreply, State}.

%% =============================================================================
%% Internal functions
%% =============================================================================
send_event(_Msg, #state{ event_receiver = undefined }) -> ok;
send_event(Msg, #state{ event_receiver = EventPid }) when is_pid(EventPid) ->
    EventPid ! Msg;
send_event(Msg, #state{ event_receiver = EventMod }) when is_atom(EventMod) ->
    EventMod:handle_event(Msg).

%% =============================================================================
%% Helper functions
%% =============================================================================
gv(Key, Options) -> proplists:get_value(Key, Options).
gv(Key, Options, Default) -> proplists:get_value(Key, Options, Default).
