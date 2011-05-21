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

-export([create/0, get_field_msg/1, get_moves_for/2,
    add_dragon_tooth/2, get_dragon_teeth/2,
    get_points_for_player/2, get_color_id_for_field/1, get_color_id_for_field/2,
    move_piece/3]).

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

move_piece(Field, Piece, FieldNr) ->
    NewPieces = dict:update(Piece, fun(P) ->
        P#piece{row = FieldNr div 8, col = FieldNr rem 8}
    end, Field#field.pieces),
    Field#field{pieces = NewPieces}.

add_dragon_tooth(Field, {_Side, _ColorID} = ID) ->
    NewPieces = dict:update(ID, fun(#piece{dragonTeeth = D} = P) ->
        P#piece{dragonTeeth = D + 1}
    end, Field#field.pieces),
    Field#field{pieces = NewPieces}.

get_dragon_teeth(Field, {_Side, _ColorID} = ID) ->
    dict:fetch(ID, Field#field.pieces).

get_points_for_player(#field{pieces = Pieces}, Side) ->
    dict:fold(fun(_ID, #piece{side = S, dragonTeeth = D}, Sum) ->
        if
            S =:= Side -> Sum + trunc(math:pow(2, D) - 1);
            true -> Sum
        end
    end, 0, Pieces).

get_color_id_for_field(#field{pieces = Pieces}, {_Side, _Color} = ID) ->
    #piece{row = R, col = C} = dict:fetch(ID, Pieces),
    get_color_id_for_field(R * 8 + C).

get_color_id_for_field(Nr) ->
    FieldRows = [
        [7,6,5,4,3,2,1,0],
        [2,7,4,1,6,3,0,5],
        [1,4,7,2,5,0,3,6],
        [4,5,6,7,0,1,2,3],
        [3,2,1,0,7,6,5,4],
        [6,3,0,5,2,7,4,1],
        [5,0,3,6,1,4,7,2],
        [0,1,2,3,4,5,6,7]
    ],
    lists:nth((Nr rem 8) + 1, lists:nth((Nr div 8) + 1, FieldRows)).

get_moves_for(#field{pieces = Pieces}, {Side, _ColorID} = ID) ->
    #piece{row = Row, col = Col} = dict:fetch(ID, Pieces),%get_piece(Pieces, fun(_ID, P) -> get_piece_id(P) =:= ID end),
    {RowDiff, RowLen} = if
        Side =:= 1 -> {-1, Row}; % bottom
        Side =:= 0 -> {1, 7 - Row} % top
    end,
    Left = [{Row + (I * RowDiff), Col - I} || I <- lists:seq(1, min(Col, RowLen))],
    Right = [{Row + (I * RowDiff), Col + I} || I <- lists:seq(1, min(7 - Col, RowLen))],
    Vertical = [{Row + (I * RowDiff), Col} || I <- lists:seq(1, RowLen)],
    Filter = fun(L) -> lists:takewhile(fun({R, C}) -> is_piece_on_field(Pieces, R, C) =:= false end, L) end,
    List = Filter(Left) ++ Filter(Right) ++ filter_blocked(Pieces, dict:fetch(ID, Pieces), Vertical),
    lists:map(fun({R, C}) -> R * 8 + C end, List).

filter_blocked(Pieces, RefPiece, List) -> filter_blocked(Pieces, RefPiece, List, []).
filter_blocked(_Pieces, _RefPiece, [], Return) -> lists:reverse(Return);
filter_blocked(Pieces, RefPiece, [{Row, Col} = Field|Rest], Return) ->
    case is_piece_on_field(Pieces, Row, Col) of
        false ->
            filter_blocked(Pieces, RefPiece, Rest, [Field|Return]);
        {true, _Piece} ->
            PieceList = takewhilemap(fun({R, C}) -> is_piece_on_field(Pieces, R, C) end, [Field|Rest]),
            Count = length(PieceList),
            #piece{dragonTeeth = Dragons, side = Side} = RefPiece,
            case
                    Dragons < Count orelse
                    lists:any(fun(#piece{side = S, dragonTeeth = D}) -> S =:= Side orelse D >= Dragons end, PieceList) orelse
                    length(Rest) + 1 =:= Count
                of
                true -> filter_blocked(Pieces, RefPiece, [], Return);
                false -> filter_blocked(Pieces, RefPiece, [], [Field|Return])
            end
    end.

takewhilemap(Fun, List) -> takewhilemap(Fun, List, []).
takewhilemap(_Fun, [], R) -> lists:reverse(R);
takewhilemap(Fun, [E|Re], R) ->
    case Fun(E) of
        {true, E2} -> takewhilemap(Fun, Re, [E2|R]);
        false -> takewhilemap(Fun, [], R)
    end.

is_piece_on_field(Pieces, Row, Col) ->
    F = fun(_ID, #piece{row = R, col = C}) -> R =:= Row andalso C =:= Col end,
    get_piece(Pieces, F).

get_piece(Pieces, F) when is_function(F) ->
    OnField = dict:filter(fun(ID, Piece) -> F(ID, Piece) end, Pieces),
    case dict:to_list(OnField) of
        [] -> false;
        [{_Key, Piece}|_] -> {true, Piece}
    end.

get_piece_id(#piece{side = S, color = C}) ->
    {S, C}.