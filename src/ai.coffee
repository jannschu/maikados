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
    
    constructor: (@connection) ->
        @aiPlayer = new AIPlayer()
        super 'waitForLoginRequest'
    
    waitForLoginRequest: (type, msg) ->
        if type is 'message' and msg instanceof ClientLoginMsg
            {name} = msg
            if ClientLoginMsg.isValidNickname(name) and @aiPlayer.name isnt name
                @connection.sendToClient new ServerResponseCode(code: ServerResponseCode.codes.OK)
                return 'waitForLoginRequest' # TODO
            else
                @connection.sendToClient new ServerResponseCode(code: ServerResponseCode.codes.Illegal)
        'waitForLoginRequest'

class AIPlayer
    
    constructor: () ->
        @name = 'Larry II.'