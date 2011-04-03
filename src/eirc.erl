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

-module(eirc).
-include("eirc.hrl").

%% Application API
%% -export([connect/4, connect_link/4, disconnect/2]).
-compile(export_all).

%% =============================================================================
%% Application API
%% =============================================================================
start(_, _) ->
    eirc_cl_sup:start_link().

stop(_) ->
    ok.

%% =============================================================================
%% Client API
%% =============================================================================
start_client(ClientId) ->
    start_client(ClientId,[]).

start_client(ClientId, Options) ->
    eirc_cl_sup:start_client(ClientId, Options).

stop_client(ClientId) ->
    eirc_cl_sup:stop_client(ClientId).

get_client(ClientId) ->
    eirc_cl_sup:get_client(ClientId).

connect_and_logon(Client, Server, Port, Nick) ->
    connect_and_logon(Client, Server, Port, "nopass", Nick, Nick, "No Name").

connect_and_logon(Client, Server, Port, Pass, Nick, User, Name) ->
    case connect(Client, Server, Port) of
	ok -> logon(Client, Pass, Nick, User, Name);
	Error -> Error
    end.

connect(Client, Server, Port) ->
    eirc_cl:connect(Client, Server, Port).

logon(Client, Nick) ->
    logon(Client, "nopass", Nick, Nick, "No Name").

logon(Client, Pass, Nick, User, Name) ->
    eirc_cl:logon(Client, Pass, Nick, User, Name).

privmsg(Client, Nick, Msg) ->
    eirc_cl:msg(Client, privmsg, Nick, Msg).

notice(Client, Nick, Msg) ->
    eirc_cl:msg(Client, notice, Nick, Msg).

cmd(Client, RawCmd) ->
    eirc_cl:cmd(Client, RawCmd).

join(Client, Channel) ->
    join(Client, Channel, "").

join(Client, Channel, Key) ->
    eirc_cl:join(Client, Channel, Key).

part(Client, Channel) ->
    eirc_cl:part(Client, Channel).

quit(Client, QuitMsg) ->
    eirc_cl:quit(Client, QuitMsg).

is_logged_on(Client) ->
    eirc_cl:is_logged_on(Client).

channels(Client) ->
    eirc_cl:channels(Client).

chan_users(Client, Channel) ->
    eirc_cl:chan_users(Client, Channel).

chan_topic(Client, Channel) ->
    eirc_cl:chan_topic(Client, Channel).

chan_type(Client, Channel) ->
    eirc_cl:chan_type(Client, Channel).

chan_has_user(Client, Channel, Nick) ->
    eirc_cl:chan_has_user(Client, Channel, Nick).

add_handler(Client, Pid) when is_pid(Pid) ->
    eirc_cl:add_handler(Client, Pid).

remove_handler(Client, Pid) when is_pid(Pid) ->
    eirc_cl:remove_handler(Client, Pid).

state(Client) ->
    eirc_cl:state(Client).

%% =============================================================================
%% Internal Functions
%% =============================================================================
