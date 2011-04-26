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
        
        @ui.postNotification 'Offensichtlich ein Fehler. Mist.', 'warn'
        @ui.postNotification 'Sonst sieht es gut aus'
        
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
        rand = (min, max) ->
            Math.round(Math.random() * (max - min)) + min
        dA = rand(0, 7)
        dB = rand(0, 7)
        _b = [0, 0, 0, 2, 1, 0, 0, 0]
        for x in [0..7]
            a = new GamingPiece(7-x, 0, x, 0)
            b = new GamingPiece(x, 7 - _b[x], x, 1)
            a.setDragonTooths 3 if x is dA
            b.setDragonTooths 2 if x is dB
            
            @ui.addGamingPiece(a)
            @ui.addGamingPiece(b)
        
        pos = 100
        countDown = () =>
            pos = 100 if pos < 0
            @ui.setProgressBar(pos)
            pos -= 1
            window.setTimeout countDown, 500
        countDown()
        
        ui = @ui
        foo = () ->
            ui.getMoveDestination("1-2", [51, 44, 37], bar)
        bar = (destField) ->
            console.debug(destField)
            ui.doMove("1-2", destField, baz)
        baz = () ->
            ui.postNotification("Move complete.")
        @ui.getPieceSelection([ "1-2" ], foo)
        
        
        #$(@ui.paper.canvas).click (event) =>
        #    @ui.swap()
    

$(document).ready ->
    new MaikadosGame(new UIField('game'))
