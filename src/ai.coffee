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
            piece = "1-#{msg.data}"
            fields = @field.getAllowedMovesFor piece
            data = [piece, fields, timeForMove]
            @connection.sendToClient new ServerGameControlMsg(code: ServerGameControlMsg.codes.ChooseField, data: data)
            return 'waitForFieldChosen'
        'waitForPieceChosen'
    
    waitForFieldChosen: (type, msg) ->
        # check Win/Lost? DragonTooth?
        'waitForFieldChosen'
    
    getStartPieces: () ->
        pieces = []
        for x in [0..7]
                               # colorID, row, col, side
            a = new GamingPiece(7-x, 0, x, 0)
            b = new GamingPiece(x, 7, x, 1)
            
            @field.addGamingPiece(a)
            @field.addGamingPiece(b)
            
            pieces.push (color: a.getColorID(), row: a.getRow(), col: a.getCol(), side: a.getSide(), dragonTeeth: a.getDragonTeeth())
            pieces.push (color: b.getColorID(), row: b.getRow(), col: b.getCol(), side: b.getSide(), dragonTeeth: b.getDragonTeeth())
        
        return pieces
 
class AIPlayer
    
    constructor: () ->
        @name = 'Larry II.'
    
    getName: () -> @name