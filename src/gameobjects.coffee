###
- This file is part of Maikados.
-
- Maikados is free software: you can redistribute it and/or modify
- it under the terms of the GNU General Public License as published by
- the Free Software Foundation, either version 3 of the License, or
- (at your option) any later version.
-
- Maikados is distributed in the hope that it will be useful,
- but WITHOUT ANY WARRANTY; without even the implied warranty of
- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
- GNU General Public License for more details.
-
- You should have received a copy of the GNU General Public License
- along with Maikados.  If not, see <http://www.gnu.org/licenses/>.
###

class GamingPiece
    
    constructor: (@colorID, @row, @col, @side) ->
        @id = "#{@side}-#{@colorID}"
        @dragonTeeth = 0
    
    getColorID: () ->
        @colorID
    
    getID: () ->
        @id
    
    getDragonTeeth: () ->
        @dragonTeeth
    
    setDragonTeeth: (value) ->
        @dragonTeeth = value if 0 <= value <= 4
    
    # 0 to 7
    getRow: () -> @row
    setRow: (@row) -> this
    
    
    # 0 to 7
    getCol: () -> @col
    setCol: (@col) -> this
    
    # 0 (black) or 1 (white)
    getSide: () ->
        @side

class GameField
    
    constructor: () ->
        @pieces = {}
    
    addGamingPiece: (piece) -> @pieces[piece.getID()] = piece
    
    getGamingPiece: (id) -> @piece[id]