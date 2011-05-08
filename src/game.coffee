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
    
    constructor: (@ui, @field) ->
        super 'waitForNickResponse'
        
        @loggedIn = false
        @ui.getNickName (nick, ai, handling) =>
            if @loggedIn
                handling.ok()
                return
            if ClientLoginMsg.isValidNickname nick
                @nickHandling = handling
                @setupConnection(if ai then new AIConnectionObject() else new NetConnection())
                @connection.send new ClientLoginMsg(name: nick)
                @nick = nick
                @ui.setLoading on
            else
                handling.getNew 'Ungültiger Nickname'
    
    setupConnection: (@connection) ->
        @connection.onMessage (msg) =>
            @sendEvent 'message', msg
        
        @connection.onFailure () =>
            @ui.postNotification 'Verbindungsfehler', 'warn'
        
        @connection.connect()
    
    ###
    - States
    ###
    waitForNickResponse: (type, msg) ->
        if type is 'message' and msg instanceof ResponseCodeMsg
            @ui.setLoading off
            {code} = msg
            if code is ResponseCodeMsg.codes.OK
                @nickHandling.ok()
                @loggedIn = true
                delete @nickHandling
                return 'waitForGameStart'
            else
                @nickHandling.getNew 'Dieser Nick ist vergeben'
        'waitForNickResponse'
    
    waitForGameStart: (type, msg) ->
        if type is 'message' and msg instanceof ServerGameStartMsg
            {opponent, side, pieces} = msg
            @opponent = opponent
            @side = side
            for {color, row, col, side, dragonTeeth} in pieces
                piece = new GamingPiece color, row, col, side
                piece.setDragonTeeth dragonTeeth
                @field.addGamingPiece piece
            @ui.update()
            @ui.postNotification "Das Spiel beginnt, du spielst gegen <em>#{opponent}</em>"
            infos = {}
            infos["player#{1 - side}Name"] = opponent
            infos["player#{side}Name"] = @nick
            @ui.setGameInformation(infos)
            return 'waitForGameControl'
        'waitForGameStart'
    
    waitForGameControl: (type, msg) ->
        if type is 'message' and msg instanceof ServerGameControlMsg
            switch msg.code
                when ServerGameControlMsg.codes.WaitForOpponent
                    {data} = msg
                    @setCountdown data
                    return 'waitForGameAction'
                when ServerGameControlMsg.codes.ChoosePiece
                    sendSelection = (piece) =>
                        nr = parseInt(piece.split('-')[1])
                        @connection.send new GameActionMsg(action: GameActionMsg.actions.PieceChosen, data: nr)                    
                        @resumeFSM()
                    {data} = msg
                    @pauseFSM()
                    @ui.postNotification "Wähle einen Stein aus! (#{data}s)", 'game'
                    @setCountdown data, () =>
                        @ui.postNotification 'Zeit überschritten, zufälliger Stein ausgewählt ☺', 'game'
                        @ui.stop()
                        sendSelection "#{@side}-#{Math.round Math.random() * 7}"
                    @ui.getPieceSelection ("#{@side}-#{p}" for p in [0..7]), (selectedPiece) =>
                        @stopCountdown()
                        sendSelection selectedPiece
                    return 'waitForGameControl'
                when ServerGameControlMsg.codes.ChooseField
                    {data} = msg
                    [piece, fields, time] = data
                    sendSelection = (nr) =>
                        @connection.send new GameActionMsg(action: GameActionMsg.actions.FieldChosen, data: nr)
                        @animateMove piece, nr, () => @resumeFSM()
                    @pauseFSM()
                    if fields.length is 0
                        @ui.postNotification 'Stein blockiert', 'game'
                        @ui.getUIPiece(piece).animateBlocked(() => @resumeFSM())
                        return 'waitForGameControl'
                    else
                        @ui.getMoveDestination piece, fields, (p) =>
                            @stopCountdown()
                            sendSelection p
                        @setCountdown time, () =>
                            @ui.postNotification 'Zeit überschritten, zufälliger Zug ausgewählt ☺', 'game'
                            @ui.stop()
                            sendSelection fields[Math.round Math.random() * (fields.length - 1)]
                    return 'waitForGameControl'
                when ServerGameControlMsg.codes.AddDragonTooth
                    {data} = msg
                    [piece, val] = data
                    @pauseFSM()
                    @field.getGamingPiece(piece).setDragonTeeth(val)
                    @ui.update () => @resumeFSM()
                    return 'waitForGameControl'
                # TODO: implement others
        'waitForGameControl'
    
    waitForGameAction: (type, msg) ->
        if type is 'message' and msg instanceof GameActionMsg
            switch msg.action
                when GameActionMsg.actions.PieceChosen
                    {data} = msg
                    @ui.postNotification "<em>#{@opponent}</em> hat einen Stein gewählt", 'game'
                    @setCountdown data
                    return 'waitForGameAction'
                when GameActionMsg.actions.FieldChosen
                    @pauseFSM()
                    {data} = msg
                    [piece, nr] = data
                    @animateMove(piece, nr, () => @resumeFSM())
                    return 'waitForGameControl'
                
        @stopCountdown()
        
        'waitForGameAction'
    
    ###
    - Helper
    ###
    
    animateMove: (piece, nr, callback) ->
        kickedPieces = @field.getKickedPieces(nr)
        if kickedPieces.length isnt 0
            side = kickedPieces[0].getSide()
            for p in kickedPieces
                p.setRow(p.getRow() - (if side is 0 then 1 else -1))
        row = Math.floor(nr / 8)
        col = nr - row * 8
        @field.getGamingPiece(piece).setRow(row).setCol(col)
        @ui.update(() => callback)
    
    setCountdown: (seconds, callback) ->
        @stopCountdown()
        
        diff = seconds * 10 # 1000 * 0.1
        @ui.setProgressBar(@countdownVal = 100)
        @countdown = window.setInterval((() =>
            if @countdownVal < 0
                @stopCountdown()
                window.setTimeout((() -> callback()), 0) if callback
            else
                @ui.setProgressBar(--@countdownVal)
        ), diff)
    
    stopCountdown: () ->
        window.clearInterval(@countdown) if @countdown isnt undefined
        @ui.setProgressBar 0
        delete @countdown
        delete @countdownVal

$( ->
    field = new GameField()
    new MaikadosGame(new UIField('game', field), field))
