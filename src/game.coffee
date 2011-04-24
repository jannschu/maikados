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
        @setupDebug()
        @ui.postNotification 'Offensichtlich ein Fehler. Mist.', 'warn'
        @ui.postNotification 'Sonst sieht es gut aus'
        @ui.getNickName (nick, handling) =>
            handling.getNew('Dieser Nick ist leider schon weg')
            # handling.ok()
            # TODO: actually do a check...
    
    ###
    - States
    ###
    waitForStart: () ->
        
    ###
    - Helper
    ###
    setupDebug: () ->
        rand = (min, max) ->
            Math.round(Math.random() * (max - min)) + min
        dA = rand(0, 7)
        dB = rand(0, 7)
        for x in [0..7]
            a = new GamingPiece(7-x, 0 + rand(0, 3), x, 0)
            b = new GamingPiece(x, 7 - rand(0, 3), x, 1)
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
        
        $(@ui.paper.canvas).click (event) =>
            @ui.swap()
    

$(document).ready ->
    new MaikadosGame(new UIField('game'))
    
    # socket = new io.Socket()
    # socket.connect()
    # socket.on 'connect', () ->
    #     console.log('connection')
    #     socket.send(from: 'me', for: 13)
    # 
    # socket.on 'message', (msg) ->
    #     console.log('received', msg)
    
    
