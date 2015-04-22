%%% @author Dayneko Roman <me@h0.org.ua>
%%% @copyright (C) 2012, Dayneko Roman
%%% Created :  7 Sep 2012 by Dayneko Roman <me@h0.org.ua>
%%%
%%% Permission to use, copy, modify, and distribute this software and its
%%% documentation for any purpose, without fee, and without a written agreement
%%% is hereby granted, provided that the above copyright notice and this
%%% paragraph and the following two paragraphs appear in all copies.
%%%  
%%% IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
%%% DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING
%%% LOST PROFITS, ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS
%%% DOCUMENTATION, EVEN IF THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.
%%%  
%%% THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
%%% AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
%%% ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATIONS TO
%%% PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
%%%

-ifndef(LOG_HRL).
-define(LOG_HRL, "log.hrl").

-define(DBG(F, A), error_logger:info_msg("DBG: ~p:~p ~w:~b:~n" ++ F ++ "~n", [node(), self(), ?MODULE, ?LINE] ++ A)).
-define(DBG(A), error_logger:info_msg("DBG: ~p:~p ~w:~b:~n ~p ~n", [node(), self(), ?MODULE, ?LINE] ++ [A])).

-define(INFO(F, A), error_logger:info_msg("INFO: ~p:~p ~w:~b:~n" ++ F ++ "~n", [node(), self(), ?MODULE, ?LINE] ++ A)).
-define(INFO(A), error_logger:info_msg("INFO: ~p:~p ~w:~b:~n ~p ~n", [node(), self(), ?MODULE, ?LINE] ++ [A])).

-define(ERR(F, A), error_logger:error_msg("ERROR: ~p:~p ~w:~b:~n" ++ F ++ "~n", [node(), self(), ?MODULE, ?LINE] ++ A)).
-define(ERR(A), error_logger:error_msg("ERROR: ~p:~p ~w:~b:~n ~p ~n", [node(), self(), ?MODULE, ?LINE] ++ [A])).

-define(WARN(F, A), error_logger:warning_msg("WARNING: ~p:~p ~w:~b:~n" ++ F ++ "~n", [node(), self(), ?MODULE, ?LINE] ++ A)).
-define(WARN(A), error_logger:warning_msg("WARNING: ~p:~p ~w:~b:~n ~p ~n", [node(), self(), ?MODULE, ?LINE] ++ [A])).

-endif.
