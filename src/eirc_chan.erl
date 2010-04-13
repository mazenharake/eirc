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

-module(eirc_chan).
-include("eirc.hrl").

-record(chan, { name, topic = "", users = [], modes = "" }).

-compile(export_all).

%% =============================================================================
%% Channel manipulation
%% =============================================================================
init() ->
    gb_trees:empty().

%% =============================================================================
%% Self JOIN/PART
%% =============================================================================
join(Struct, ChanName) ->
    Name = normalize(ChanName),
    case gb_trees:lookup(Name, Struct) of
	{value, _} ->
	    Struct;
	none ->
	    gb_trees:insert(Name, #chan{ name = Name }, Struct)
    end.

part(Struct, ChanName) ->
    Name = normalize(ChanName),
    gb_trees:delete(Name, Struct).    

%% =============================================================================
%% Channel Modes/Attributes
%% =============================================================================
set_topic(Struct, ChanName, Topic) ->
    Name = normalize(ChanName),
    Channel = gb_trees:get(Name, Struct),
    gb_trees:enter(Name, Channel#chan{ topic = Topic }, Struct).


%% =============================================================================
%% Users JOIN/PART/AKAs(namechange)
%% =============================================================================
user_join(Struct, ChanName, Nick) ->
    users_join(Struct, ChanName,[Nick]).

users_join(Struct, ChanName, Nicks) ->
    ManipFun = fun(ChanNicks) ->
		       ChanNicks ++ Nicks
	       end,
    users_manip(Struct, ChanName, ManipFun).

user_part(Struct, ChanName, Nick) ->
    users_part(Struct, ChanName, [Nick]).

users_part(Struct, ChanName, Nicks) ->
    ManipFun = fun(ChanNicks) ->
		       ChanNicks -- Nicks
	       end,
    users_manip(Struct, ChanName, ManipFun).

user_aka(Struct, ChanName, Nick, NewNick) ->
    users_aka(Struct, ChanName, [{Nick, NewNick}]).

users_aka(Struct, ChanName, NickNewNickList) ->
    ManipFun = fun(ChanNicks) ->
		       F = fun({PrevNick, NewNick}, Acc) ->
				   [NewNick|Acc--[PrevNick]]
			   end,
		       lists:foldl(NickNewNickList, ChanNicks, F)
	       end,
    users_manip(Struct, ChanName, ManipFun).

users_manip(Struct, ChanName, Fun) ->
    Name = normalize(ChanName),
    Channel = gb_trees:get(Name, Struct),
    Chanlist = Fun(Channel#chan.users),
    gb_trees:enter(ChanName, Channel#chan{ users = Chanlist }, Struct).

%% =============================================================================
%% Introspection
%% =============================================================================
channels(Struct) ->
    [ ChanName || {ChanName, _Chan} <- gb_trees:to_list(Struct) ].

chan_users(Struct, ChanName) ->
    Name = normalize(ChanName),
    case gb_trees:lookup(Name, Struct) of
        {value, Channel} -> Channel#chan.users;
	none -> {error, no_such_channel}
    end.

%% =============================================================================
%% Internal functions
%% =============================================================================
normalize(ChanName) -> string:to_lower(ChanName).

