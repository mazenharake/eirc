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

-module(eirc_lib).
-include("eirc.hrl").

-compile(export_all).

%% =============================================================================
%% Generic IRC message parse
%% =============================================================================
parse(UnstrippedData) ->
    Data = string:substr(UnstrippedData,1,length(UnstrippedData)-2),
    case Data of
	[$:|_] ->
	    [[$:|From]|RestData] = string:tokens(Data," "),
	    getcmd(RestData, parsefrom(From, #ircmsg{ ctcp = false }));
	Data ->
	    getcmd(string:tokens(Data," "), #ircmsg{ ctcp = false })
    end.

parsefrom(FromStr, Msg) ->
    case re:split(FromStr, "(!|@)",[{return, list}]) of
	[Nick, "!", User, "@", Host] ->
	    Msg#ircmsg{ nick = Nick, user = User, host = Host };
	[Nick, "@", Host] ->
	    Msg#ircmsg{ nick = Nick, host = Host };
	[Server] ->
	    %% No nick detection... we are assuming it is the server here but it
	    %% could just as well be a user nick (let someone else decide)
	    Msg#ircmsg{ server = Server }
    end.

getcmd([Cmd,Arg1,[$:,1|CTCPTrail]|RestArgs], Msg) when Cmd == "PRIVMSG"; 
						       Cmd == "NOTICE" ->
    getcmd([Cmd,Arg1,[1|CTCPTrail]|RestArgs], Msg);
getcmd([Cmd,Arg1,[1|CTCPTrail]|RestArgs], Msg) when Cmd == "PRIVMSG"; 
						    Cmd == "NOTICE" ->
    case lists:reverse(lists:flatten(CTCPTrail++[" "++Arg||Arg<-RestArgs])) of
	[1|CTCPRev] ->
	    [CTCPCmd|Args] = string:tokens(lists:reverse(CTCPRev)," "),
	    Msg#ircmsg{ cmd = CTCPCmd, args = Args, ctcp = true };
	Str ->
	    io:format("ERROR: ~1000p~n",[Str]),
	    Msg#ircmsg{ cmd = Cmd, ctcp = invalid }
    end;
getcmd([Cmd|RestData], Msg) ->
    getargs(RestData, Msg#ircmsg{ cmd = Cmd }).

getargs([], Msg) ->
    Msg#ircmsg{ args = lists:reverse(Msg#ircmsg.args) };
getargs([[$:|FirstArg]|RestArgs], Msg) ->
    case lists:flatten([" "++Arg||Arg<-[FirstArg|RestArgs]]) of
	[_|[]] ->
	    getargs([], Msg#ircmsg{ args = [""|Msg#ircmsg.args] });
	[_|FullTrail] ->
	    getargs([], Msg#ircmsg{ args = [FullTrail|Msg#ircmsg.args] })
    end;
getargs([Arg|[]], Msg) ->
    getargs([], Msg#ircmsg{ args = ["",Arg|Msg#ircmsg.args] });
getargs([Arg|RestData], Msg) ->
    getargs(RestData, Msg#ircmsg{ args = [Arg|Msg#ircmsg.args] }).

%% =============================================================================
%% RPL_ISUPPORT (005) parse
%% =============================================================================
isup([], State) -> State;
isup([Param|Rest], State) ->
    try	isup(Rest, isup_param(Param, State))
    catch _:_ -> isup(Rest, State) end.

isup_param("CHANTYPES="++ChanPrefixes, State) ->
    State#state{ chprefix = ChanPrefixes };
isup_param("NETWORK="++Network, State) ->
    State#state{ network = Network };
isup_param("PREFIX="++UserPrefixes, State) ->
    {match,[{P1,L1},{P2,L2}]} = 
	re:run(UserPrefixes, "\\((.*)\\)(.*)", [{capture, all_but_first}]),
    State#state{ usrprefix = lists:zip(string:substr(UserPrefixes,P1+1,L1),
				       string:substr(UserPrefixes,P2+1,L2)) };
isup_param(_, State) ->
    State.

%% =============================================================================
%% Helper functions
%% =============================================================================
ctcp_time({{Y,M,D},{H,N,S}}) ->
    [lists:nth(calendar:day_of_the_week(Y,M,D),
	       ["Mon","Tue","Wed","Thu","Fri","Sat","Sun"])," ",
     lists:nth(M, ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
		   "Sep","Oct","Nov","Dec"])," ",
     io_lib:format("~2..0s",[integer_to_list(D)])," ",
     io_lib:format("~2..0s",[integer_to_list(H)]),":",
     io_lib:format("~2..0s",[integer_to_list(N)]),":",
     io_lib:format("~2..0s",[integer_to_list(S)])," ",
     integer_to_list(Y)].
