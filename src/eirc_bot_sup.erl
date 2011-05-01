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

-module(eirc_bot_sup).

%% Supervisor API
-compile(export_all).

%% Module Interface

%% =============================================================================
%% Supervisor API
%% =============================================================================
start_link() ->
    supervisor:start_link(?MODULE, []).

init(_) -> 
    {ok, {{one_for_one, 10, 10}, []}}.

%% =============================================================================
%% Module Interface
%% =============================================================================
start_bot(SupPid, BotId, CBMod, Args) ->
    Child = {BotId, {gen_eircbot, start_link, [self(), CBMod, Args]}, 
	     transient, 6000, worker, [gen_eircbot]},
    supervisor:start_child(SupPid, Child).

stop_bot(SupPid, BotId) ->
    supervisor:terminate_child(SupPid, BotId),
    supervisor:delete_child(SupPid, BotId).

get_bot(SupPid, BotId) ->
    case lists:keyfind(BotId, 1, supervisor:which_children(SupPid)) of
	{BotId, Pid, _, _} -> Pid;
	false -> undefined
    end.
