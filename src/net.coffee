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
        @socket = new io.Socket()
        @callbacks =
            msg: []
            failure: []
            connection: []
        
        @socket.on 'message', (msg) =>
            return unless (cmd = ProtocolMessages[msg.__cmd])
            result = new cmd(msg)
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
    
    onMessage: (callback) ->
        @callbacks.msg.push callback
    
    onFailure: (callback) ->
        @callbacks.failure.push callback
    
    onConnect: (callback) ->
        @callbacks.connection.push callback
    
    send: (aMsgObj) ->
        @socket.send aMsgObj.toJSONObject()

class ProtocolMessage

    toJSONObject: () ->
        obj = {'__cmd': @__cmd}
        for name, value of this
            if @hasOwnProperty(name) and name.toString()[0] isnt '_'
                obj[name] = value
        obj


ProtocolMessages = {}

ProtocolMessages[0] = class ClientLoginMsg extends ProtocolMessage
    constructor: ({@name}) ->

ClientLoginMsg.isValidNickname = (nick) -> (/^[- a-z0-9_öäüß@.]{1,15}$/i).test nick


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


ProtocolMessages[5] = class LobbySetPlayerMsg extends ProtocolMessages
    constructor: ({@list}) ->

ProtocolMessages[6] = class LobbyPlayerLeftMsg extends ProtocolMessages
    constructor: ({@name}) ->

ProtocolMessages[7] = class LobbyChallengePlayerMsg extends ProtocolMessages
    constructor: ({@name}) ->

ProtocolMessages[8] = class LobbyAcceptChallengeMsg extends ProtocolMessages
    constructor: ({@name}) ->

for cmdNr, msg of ProtocolMessages
    msg::.__cmd = parseInt(cmdNr)
     