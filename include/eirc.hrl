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

-define(CRLF, "\r\n").
-define(CMD(Cmd), [Cmd, ?CRLF]).

%% IRC Command macros
-define(PASS(Pwd), ?CMD(["PASS ",Pwd])).
-define(NICK(Nick), ?CMD(["NICK ",Nick])).
-define(USER(User, Name), ?CMD(["USER ",User," 0 * :",Name])).
-define(PONG1(Nick), ?CMD(["PONG ",Nick])).
-define(PONG2(Nick, To), ?CMD(["PONG ",Nick," ",To])).
-define(QUIT(Msg), ?CMD(["QUIT :",Msg])).

%% Records
-record(ircmsg, { server, nick, user, host, cmd, args = [] }).

%% IRC Codes
-define(RPL_WELCOME, "001").
-define(RPL_YOURHOST, "002").
-define(RPL_CREATED, "003").
-define(RPL_MYINFO, "004").
%% -define(RPL_BOUNCE, "005"). %% RFC2812
-define(RPL_ISUPPORT, "005"). %% Defacto standard for server support
-define(RPL_BOUNCE, "010"). %% Defacto replacement of "005" in RFC2812

-define(ERR_NONICKNAMEGIVEN, "431").
-define(ERR_ERRONEUSNICKNAME, "432").
-define(ERR_NICKNAMEINUSE, "433").
-define(ERR_NICKCOLLISION, "436").
-define(ERR_UNAVAILRESOURCE, "437").

-define(ERR_NEEDMOREPARAMS, "461").
-define(ERR_ALREADYREGISTRED, "462").

-define(ERR_RESTRICTED, "484").


%% Code groups
-define(LOGON_ERRORS, [?ERR_NONICKNAMEGIVEN, ?ERR_ERRONEUSNICKNAME,
		       ?ERR_NICKNAMEINUSE, ?ERR_NICKCOLLISION,
		       ?ERR_UNAVAILRESOURCE, ?ERR_NEEDMOREPARAMS,
		       ?ERR_ALREADYREGISTRED, ?ERR_RESTRICTED]).
