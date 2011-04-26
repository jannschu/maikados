%%% This file is part of Maikados.
%%% 
%%% Maikados is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%% 
%%% Maikados is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%% 
%%% You should have received a copy of the GNU General Public License
%%% along with Maikados.  If not, see <http://www.gnu.org/licenses/>.
-module(maikados_protocol).

-export([record_to_packet/1, packet_to_record/1]).

-define(DEFINE_PROTCOL_COMMAND_FUNCTIONS, true).
-include("protocol.hrl").

record_to_packet(Record) ->
    Name = element(1, Record),
    case cmd_fields(Name) of
        error -> error;
        {ok, Fields} ->
            {ok, CmdNr} = cmd2nr(Name),
            {_, FullPacket} = lists:foldl(fun(Field, {N, Packet}) ->
                Value = element(N, Record),
                Key = atom_to_binary(Field, utf8),
                {N + 1, [ {Key, Value} | Packet ]}
            end, {2, [{<<"__cmd">>, CmdNr}]}, Fields),
            {ok, FullPacket}
    end.

packet_to_record(Proplist) ->
    CmdNr = proplists:get_value(<<"__cmd">>, Proplist),
    case nr2cmd(CmdNr) of
        error -> error;
        {ok, Command} ->
            try
                {ok, Fields} = cmd_fields(Command),
                ElemList = lists:foldr(fun(Field, List) ->
                    case proplists:get_value(atom_to_binary(Field, utf8), Proplist) of
                        undefined -> throw(packet_error);
                        Value ->
                            [Value | List]
                    end
                end, [], Fields),
                {ok, list_to_tuple([Command|ElemList])}
            catch
                throw:packet_error -> error
            end
    end.
