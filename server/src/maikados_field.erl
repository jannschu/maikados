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
-module(maikados_field).

-export([create/0, get_field_msg/1]).

-record(field, {pieces = dict:new()}).
-record(piece, {row, col, color, side, dragonTeeth = 0}).

create() ->
    create(0, #field{}).

create(8, Field) -> Field;
create(X, #field{pieces = Pieces} = Field) ->
    Piece0 = #piece{color = 7 - X, row = 0, col = X, side = 0},
    Piece1 = #piece{color =     X, row = 7, col = X, side = 1},
    Pieces2 = dict:store(get_piece_id(Piece0), Piece0, Pieces),
    Pieces3 = dict:store(get_piece_id(Piece1), Piece1, Pieces2),
    create(X + 1, Field#field{pieces = Pieces3}).

get_field_msg(#field{pieces = Pieces}) ->
    dict:fold(fun(_ID, #piece{row = R, col = Cl, side = S, color = Cr, dragonTeeth = D}, Acc) ->
        El = [{<<"color">>, Cr}, {<<"row">>, R}, {<<"col">>, Cl}, {<<"side">>, S}, {<<"dragonTeeth">>, D}],
        [El | Acc]
    end, [], Pieces).

get_moves_for(#field{pieces = Pieces}, {Side, ColorID}) ->
    [].

get_piece_id(#piece{side = S, color = C}) ->
    integer_to_list(S) ++ "-" ++ integer_to_list(C).

piece_on_field(Pieces, Row, Col) ->
    OnField = dict:filter(fun(_Key, #piece{row = R, col = C}) -> R =:= Row andalso C =:= Col end, Pieces),
    case dict:to_list(OnField) of
        [] -> false;
        [{_Key, Piece}|_] -> {true, Piece}
    end.
    