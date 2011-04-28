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

class MaikadosGame extends GameState
    
    constructor: (@ui) ->
        super 'waitForStart'
        
        @ui.getNickName (nick, handling) =>
            handling.getNew('Dieser Nick ist leider schon weg')
            # TODO: actually do a check
            # @sendEvent('nick', nick)
            # handling.ok()
            # TODO: actually do a check
        
        @connection = new NetConnection
        @connection.onMessage (msg) =>
            @sendEvent 'message', msg
        @connection.connect()
        @connection.send new ClientLoginMsg(name: "Ralf")
        
        @setupDebug()
    
    ###
    - States
    ###
    waitForStart: (type, content) ->
        if type is 'message' # got message over the socket
            console.log content
    
    ###
    - Helper
    ###
    setupDebug: () ->
        rand = (a, b) -> Math.round(Math.random() * (b - a)) + a
        dragonPieceNr = () ->
            p = rand 1, 31
            if p < 2
                4
            else if p < 4
                3
            else if p < 8
                2
            else if p < 16
                1
            else 0
        for x in [0..7]
            a = new GamingPiece(7-x, 0, x, 0)
            b = new GamingPiece(x, 7, x, 1)
            a.setDragonTooths(dragonPieceNr())
            b.setDragonTooths(dragonPieceNr())
            @ui.addGamingPiece(a)
            @ui.addGamingPiece(b)
        
        pos = 100
        countDown = () =>
            pos = 100 if pos < 0
            @ui.setProgressBar(pos)
            pos -= 1
            window.setTimeout countDown, 500
        countDown()
        
        @ui.postNotification "Wähle einen Stein aus!"
        @ui.getPieceSelection ("1-#{p}" for p in [0..7]), (pieceId) =>
            @ui.postNotification("Piece #{pieceId} selected.")
            fields = []
            {row, col} = @ui.pieces[pieceId]
            a = b = i = row * 8 + col
            mod = (a, m) -> a - Math.floor(a/m) * m
            (fields.push a) while ((a -= 9) % 8) >= 0 and (a % 8) < (i % 8) and  a > 7
            (fields.push b) while ((b -= 7) % 8) > 0 and b > 7
            (fields.push i) while (i -= 8) > 7
                
            @ui.getMoveDestination pieceId, fields, (selectedField) =>
                @ui.doMove pieceId, selectedField

$(document).ready ->
    new MaikadosGame(new UIField('game'))
