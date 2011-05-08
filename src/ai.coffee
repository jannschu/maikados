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

class AIConnectionObject

    constructor: () ->
        @callbacks =
            msg: []
            connection: []
        
        @logic = new AIGameLogic(this)
    
    connect: () ->
        for obj in @callbacks.connection
            obj()
    
    onMessage: (callback) ->
        @callbacks.msg.push callback
    
    onConnect: (callback) ->
        @callbacks.connection.push callback
    
    send: (msg) ->
        @logic.sendEvent 'message', msg
    
    onFailure: (callback) ->
        # should be no connection failures :) 
    
    sendToClient: (msg) ->
        for obj in @callbacks.msg
            obj(msg)

class AIGameLogic extends GameState
    
    timeForMove = 60 # seconds
    
    constructor: (@connection) ->
        ###
        - General information:
        -   ai is player 0, human is player 1
        ###
        @aiPlayer = new AIPlayer()
        
        @field = new GameField()
        @movingBlockedPieces = 0
        
        super 'waitForLoginRequest'
    
    waitForLoginRequest: (type, msg) ->
        if type is 'message' and msg instanceof ClientLoginMsg
            {name} = msg
            if ClientLoginMsg.isValidNickname(name) and @aiPlayer.name isnt name
                @connection.sendToClient new ResponseCodeMsg(code: ResponseCodeMsg.codes.OK)
                @connection.sendToClient new ServerGameStartMsg(opponent: @aiPlayer.getName(), side: 1, pieces: @getStartPieces())
                @connection.sendToClient new ServerGameControlMsg(code: ServerGameControlMsg.codes.ChoosePiece, data: timeForMove / 2)
                return 'waitForPieceChosen'
            else
                @connection.sendToClient new ResponseCodeMsg(code: ResponseCodeMsg.codes.Illegal)
        'waitForLoginRequest'
    
    waitForPieceChosen: (type, msg) ->
        if type is 'message' and msg instanceof GameActionMsg and msg.action is GameActionMsg.actions.PieceChosen
            @piece = "1-#{msg.data}"
            fields = @field.getAllowedMovesFor @piece
            data = [@piece, fields, timeForMove]
            @connection.sendToClient new ServerGameControlMsg(code: ServerGameControlMsg.codes.ChooseField, data: data)
            return 'waitForFieldChosen'
        'waitForPieceChosen'
    
    waitForFieldChosen: (type, msg) ->
        if type is 'message' and msg instanceof GameActionMsg and msg.action is GameActionMsg.actions.FieldChosen
            {data} = msg # field nr
            side = @field.getGamingPiece(@piece).getSide()
            
            nextPiece = unless data is null # was blocked
                @movingBlockedPieces = 0
                row = Math.floor(data / 8)
                col = data - row * 8
                p = @field.getGamingPiece(@piece)
                
                if row is 7 or row is 0
                    p.setDragonTeeth(p.getDragonTeeth() + 1)
                
                if @field.getPointsForSide(side) >= @field.getWinPoints()
                    false
                else
                    n = "#{1 - side}-#{@field.getColorIDForMoveTo data}"
                    p.setRow(row).setCol(col)
                    n
            else
                if ++@movingBlockedPieces is 2
                    false
                else
                    "#{1 - side}-#{@field.getColorIDForPiece @piece}"
            
            @piece = nextPiece
            
            if side is 0 # notify client about AI move
                @connection.sendToClient(new GameActionMsg(action: GameActionMsg.actions.FieldChosen, data: data))
            
            if nextPiece is false # double block situation
                msg = if side is 0
                    ServerGameControlMsg.codes.YouLost
                else
                    @aiPlayer.youFailedNotification(@nick) if side is 1
                    ServerGameControlMsg.codes.YouWin
                @connection.sendToClient new ServerGameControlMsg(code: msg)
                return 'IDLE'
            else # next player
                if side is 0
                    fields = @field.getAllowedMovesFor @piece
                    data = [@piece, fields, timeForMove]
                    @connection.sendToClient new ServerGameControlMsg(code: ServerGameControlMsg.codes.ChooseField, data: data)
                else # ai has to move
                    @connection.sendToClient new ServerGameControlMsg(code: ServerGameControlMsg.codes.WaitForOpponent, data: [timeForMove, @piece])
                    @pauseFSM()
                    aiMove = @aiPlayer.getMove @field.getGamingPiece(@piece), @field, (m) =>
                        @sendEvent 'message', new GameActionMsg(action: msg.action, data: m)
                        @resumeFSM()
            return 'waitForFieldChosen'
        'waitForFieldChosen'
    
    getStartPieces: () ->
        pieces = []
        for x in [0..7]
                               # colorID, row, col, side
            a = new GamingPiece(7-x, 0, x, 0)
            b = new GamingPiece(  x, 7, x, 1)
            
            @field.addGamingPiece(a)
            @field.addGamingPiece(b)
            
            pieces.push (color: a.getColorID(), row: a.getRow(), col: a.getCol(), side: a.getSide(), dragonTeeth: a.getDragonTeeth())
            pieces.push (color: b.getColorID(), row: b.getRow(), col: b.getCol(), side: b.getSide(), dragonTeeth: b.getDragonTeeth())
        
        return pieces

class AIPlayer
    
    constructor: () ->
        @name = 'Larry II.'
    
    getName: () -> @name
    
    ###
    - Arguments:
    -   piece: GamingPiece (see gameobjects.coffee)
    -   field: GameField (see gameobjects.coffee)
    -   callback: some function
    - Notice: You have to copy the objects before manipulating them :)
    ###
    getMove: (piece, field, callback) ->
        # Why do we do that?!
        # Answer: Execution is single-threaded and we don't want to
        #         block animations etc. ( = everything else).
        #         So split the calculation up.
        calc = () =>
            # TODO!!!1!elf!: find the move a bit
            moves = field.getAllowedMovesFor(piece.getID())
            moveFound = true
            if moves.length is 0
                move = null
            else
                move = moves[Math.round(Math.random() * (moves.length - 1))]
            
            if moveFound
                # ´move' should be a field number. Field:
                #          0  1  2  3  4  5  6  7 (black (AI) starts from here)
                #          8  9 10 11 12 13 14 15
                #         16 17 18 19 20 21 22 23
                #         24 25 26 27 28 29 30 31
                #         32 33 34 35 36 37 38 39
                #         40 41 42 43 44 45 46 47
                #         48 49 50 51 52 53 54 55
                #         56 57 58 59 60 61 62 63 (white from here)
                window.setTimeout (() -> callback(move))
            else
                window.setTimeout (() -> calc()), 0
        window.setTimeout (() -> calc()), 0
    
    # Is called by the logic to say the AI: you were beaten by `name'
    youFailedNotification: (name) ->
        return "Do'h! Screw you #{name}."
