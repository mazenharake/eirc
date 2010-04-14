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

-record(chan, { name, topic = "", users = [], modes = "", type }).

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
    Name = chan2lower(ChanName),
    case gb_trees:lookup(Name, Struct) of
	{value, _} ->
	    Struct;
	none ->
	    gb_trees:insert(Name, #chan{ name = Name }, Struct)
    end.

part(Struct, ChanName) ->
    Name = chan2lower(ChanName),
    gb_trees:delete(Name, Struct).    

%% =============================================================================
%% Channel Modes/Attributes
%% =============================================================================
set_topic(Struct, ChanName, Topic) ->
    Name = chan2lower(ChanName),
    Channel = gb_trees:get(Name, Struct),
    gb_trees:enter(Name, Channel#chan{ topic = Topic }, Struct).

set_type(Struct, ChanName, ChanType) ->
    Name = chan2lower(ChanName),
    Channel = gb_trees:get(Name, Struct),
    Type = case ChanType of
	       "@" -> secret; "*" -> private; "=" -> public
	   end,
    gb_trees:enter(Name, Channel#chan{ type = Type }, Struct).

%% =============================================================================
%% Users JOIN/PART/AKAs(namechange)
%% =============================================================================
user_join(Struct, ChanName, Nick) ->
    users_join(Struct, ChanName,[Nick]).

users_join(Struct, ChanName, Nicks) ->
    PNicks = strip_rank(Nicks),
    ManipFun = fun(ChanNicks) -> lists:usort(ChanNicks ++ PNicks) end,
    users_manip(Struct, ChanName, ManipFun).

user_part(Struct, ChanName, Nick) ->
    PNick = strip_rank([Nick]),
    ManipFun = fun(ChanNicks) -> lists:usort(ChanNicks -- PNick) end,
    users_manip(Struct, ChanName, ManipFun).

user_rename(Struct, Nick, NewNick) ->
    ManipFun = fun(ChanNicks) ->
		       case lists:member(Nick, ChanNicks) of
			   true -> lists:usort([NewNick|ChanNicks--[Nick]]);
			   false -> ChanNicks
		       end
	       end,
    FoldlFun = fun(ChanName, NewStruct) ->
		       Name = chan2lower(ChanName),
		       users_manip(NewStruct, Name, ManipFun)
	       end,
    lists:foldl(FoldlFun, Struct, channels(Struct)). 

users_manip(Struct, ChanName, Fun) ->
    Name = chan2lower(ChanName),
    Channel = gb_trees:get(Name, Struct),
    Chanlist = Fun(Channel#chan.users),
    gb_trees:enter(ChanName, Channel#chan{ users = Chanlist }, Struct).

%% =============================================================================
%% Introspection
%% =============================================================================
channels(Struct) ->
    [ ChanName || {ChanName, _Chan} <- gb_trees:to_list(Struct) ].

chan_users(Struct, ChanName) ->
    get_attr(Struct, ChanName, fun(#chan{ users = Users }) -> Users end).

chan_topic(Struct, ChanName) ->
    get_attr(Struct, ChanName, fun(#chan{ topic = Topic }) -> Topic end).

chan_type(Struct, ChanName) ->
    get_attr(Struct, ChanName, fun(#chan{ type = Type }) -> Type end).

chan_has_user(Struct, ChanName, Nick) ->
    get_attr(Struct, ChanName, fun(#chan{ users = Users }) -> 
				       lists:member(Nick, Users)
			       end).

%% =============================================================================
%% Internal functions
%% =============================================================================
chan2lower(ChanName) -> string:to_lower(ChanName).

strip_rank(Nicks) -> 
    lists:map(fun([$@|Nick]) -> Nick;
		 ([$+|Nick]) -> Nick;
		 (Nick) -> Nick end, Nicks).

get_attr(Struct, ChanName, Fun) ->
    Name = chan2lower(ChanName),
    case gb_trees:lookup(Name, Struct) of
	{value, Channel} -> Fun(Channel);
        none -> {error, no_such_channel}
    end.

