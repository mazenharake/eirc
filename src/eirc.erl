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

-module(eirc).
-include("eirc.hrl").

%% Application API
%% -export([connect/4, connect_link/4, disconnect/2]).
-compile(export_all).

%% =============================================================================
%% Application API
%% =============================================================================
start() ->
    start([]).

start(Options) ->
    eirc_cl:start(Options).

start_link() ->
    start_link([]).

start_link(Options) ->
    eirc_cl:start_link(Options).

connect(Client, Server, Port) ->
    eirc_cl:connect(Client, Server, Port).

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

quit(Client, QuitMsg) ->
    eirc_cl:quit(Client, QuitMsg).

%% =============================================================================
%% Internal Functions
%% =============================================================================
