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

class NetConnection

    constructor: () ->
        @socket = new io.Socket(undefined, (reconnect: false, transports: ['websocket', 'flashsocket']))
        @callbacks =
            msg: []
            failure: []
            connection: []
        
        @socket.on 'message', (msg) =>
            return unless (cmd = ProtocolMessages[msg.__cmd])
            if cmd instanceof CloseConnectionMessage
                @socket.close()
            else
                result = new cmd(msg)
                result.decodeFromJSONObject()
                for obj in @callbacks.msg
                    obj(result)
        
        @socket.on 'connect', () =>
            for obj in @callbacks.connection
                obj()
        
        failureHandler = () =>
            for obj in @callbacks.failure
                obj()
        @socket.on 'connect_failed', failureHandler
        @socket.on 'disconnect', failureHandler
    
    connect: () ->
        @socket.connect()
    
    close: () ->
        @callbacks.failure = []
        @socket.disconnect()
    
    onMessage: (callback) ->
        @callbacks.msg.push callback
    
    onFailure: (callback) ->
        @callbacks.failure.push callback
    
    onConnect: (callback) ->
        @callbacks.connection.push callback
    
    send: (aMsgObj) ->
        @socket.send aMsgObj.toJSONObject()
    

class ProtocolMessage
    
    traverseObject = (obj, f) ->
        switch typeof obj # be beware of self references :)
            when 'boolean' then obj
            when 'number' then obj
            when 'undefined' then obj
            when 'function' then obj
            when 'string' then f(obj)
            when 'array' then return (traverseObject(el, f) for el in obj)
            when 'object'
                for name, value of obj then if obj.hasOwnProperty(name)
                    obj[name] = traverseObject(value, f)
                obj
    
    toJSONObject: () ->
        obj = {'__cmd': @__cmd}
        for name, value of this
            if @hasOwnProperty(name) and name.toString()[0] isnt '_'
                obj[name] = traverseObject(value, encodeURIComponent)
        obj
    
    decodeFromJSONObject: () ->
        for name, value of this
            if @hasOwnProperty(name) and name.toString()[0] isnt '_'
                this[name] = traverseObject(value, decodeURIComponent)
    

ProtocolMessages = {}

ProtocolMessages[0] = class ClientLoginMsg extends ProtocolMessage
    constructor: ({@name}) ->

ClientLoginMsg.isValidNickname = (nick) -> (/^[- a-z0-9_ÖÄÜöäüß@.]{1,15}$/i).test nick


ProtocolMessages[1] = class ResponseCodeMsg extends ProtocolMessage
    constructor: ({@code}) ->

ResponseCodeMsg.codes =
    OK: 0
    Illegal: 1
    Yes: 2
    No: 3
    Left: 4
    Right: 5


ProtocolMessages[2] = class ServerGameStartMsg extends ProtocolMessage
    constructor: ({@opponent, @side, @pieces}) ->


ProtocolMessages[3] = class ServerGameControlMsg extends ProtocolMessage
    constructor: ({@code, @data}) ->

ServerGameControlMsg.codes =
    WaitForOpponent: 0
    LostOpponentConnection: 1
    ChoosePiece: 2
    ChooseField: 3
    YouLost: 4
    YouWin: 5
    NextModeQuestion: 6
    NextMode: 7


ProtocolMessages[4] = class GameActionMsg extends ProtocolMessage
    constructor: ({@action, @data}) ->

GameActionMsg.actions =
    PieceChosen: 0
    FieldChosen: 1


ProtocolMessages[5] = class LobbySetPlayerMsg extends ProtocolMessage
    constructor: ({@list}) ->

ProtocolMessages[6] = class LobbyPlayerLeftMsg extends ProtocolMessage
    constructor: ({@name}) ->

ProtocolMessages[7] = class LobbyChallengePlayerMsg extends ProtocolMessage
    constructor: ({@name}) ->

ProtocolMessages[8] = class LobbyAcceptChallengeMsg extends ProtocolMessage
    constructor: ({@name}) ->

ProtocolMessages[9] = class CloseConnectionMessage extends ProtocolMessage

for cmdNr, msg of ProtocolMessages
    msg::.__cmd = parseInt(cmdNr)
     