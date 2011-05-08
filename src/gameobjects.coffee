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
        @winPoints = 1
    
    getWinPoints: () -> @winPoints
    
    addGamingPiece: (piece) -> @pieces[piece.getID()] = piece
    
    getGamingPiece: (id) -> @pieces[id]
    
    getAllowedMovesFor: (piece) ->
        return [] if not (piece = @getGamingPiece(piece))
        
        side = piece.getSide() # 0 top, 1 bottom
        [A, B, I] = (if side is 1 then [9, 7, 8] else [-7, -9, -8])
        a = b = i = piece.getRow() * 8 + piece.getCol()
        mod = (a, m) -> a - Math.floor(a/m) * m
        fields = []
        
        while (mod (a -= A), 8) < (i % 8)
            break if @pieceOnField(a) isnt null
            fields.push a
        
        while (mod (b -= B), 8) > (i % 8) 
            break if @pieceOnField(b) isnt null
            fields.push b
        
        while 63 >= (i -= I) >= 0
            rowDiff = Math.abs((Math.floor i / 8) - piece.getRow())
            if (dt = piece.getDragonTeeth()) > 0 and rowDiff is 1
                lineUp = [i, i -= I, i -= I, i -= I]
                if lineUp[0] is null
                    fields.push lineUp[0]
                else
                    ppList = (@pieceOnField pp for pp in lineUp)
                    pieceLineLength = 0
                    maxDt = 0
                    for p in ppList
                        break if p is null or p.getSide() is piece.getSide()
                        if (dtt = p.getDragonTeeth()) > maxDt
                            maxDt = dtt
                        ++pieceLineLength
                    break unless dt >= pieceLineLength # to long to kick
                    break unless 0 <= lineUp[pieceLineLength] <= 63
                    break unless dt > maxDt
                    fields.push lineUp[0]
                    break
            else
                break if @pieceOnField i
                fields.push i 
        
        return fields
    
    pieceOnField: (nr) ->
        for id, p of @pieces
            return p if (p.getRow() * 8 + p.getCol()) is nr
        null
    
    getPointsForSide: (side) ->
        p = [0, 1, 3, 7, 15]
        points = 0
        for i in [0..7]
            piece = @getGamingPiece("#{side}-#{i}")
            points += p[piece.getDragonTeeth()]
        points
    
    getColorIDForPiece: (id) ->
        piece = @getGamingPiece(id)
        UI.fieldRows[piece.getRow()][piece.getCol()]
    
    getColorIDForMoveTo: (nr) ->
        row = Math.floor(nr / 8)
        col = nr - row * 8
        kickedPieces = if @pieceOnField(nr) then @getKickedPieces(nr) else []
        if kickedPieces.length isnt 0
            console.log "kicked"
            pieceId = undefined
            col = kickedPieces[0].getCol()
            side = kickedPieces[0].getSide()
            for p in kickedPieces
                c = p.getCol()
                if side is 0
                    col = c if c < col
                else
                    col = c if c > col
            col += if side is 0 then -1 else 1
        UI.fieldRows[row][col]
    
    getKickedPieces: (nr) ->
        inCol = []
        kickedSide = null
        row = Math.floor(nr / 8)
        col = nr - row * 8 
        for id, piece of @pieces
            if piece.getCol() is col
                inCol.push piece
                # 0 up, 1 down
                kickedSide = piece.getSide() if piece.getRow() is row
        
        diff = (ref, test) ->
            if kickedSide is 0 then ref - test else test - ref
        kickedPieces = []        
        for i in [0...3]
            for p in inCol
                kickedPieces.push p if diff(row, p.getRow()) is i
            break unless kickedPieces.length is i
        
        kickedPieces