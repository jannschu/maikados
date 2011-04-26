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
        @callbackObjects = []
        
        @socket.on 'message', (msg) =>
            return unless (cmd = ProtocolMessages[msg.__cmd])
            result = new cmd(msg)
            for obj in @callbackObjects
                obj(result)
    
    connect: () ->
        @socket.connect()
    
    # callback should be an instance of GameState
    onMessage: (callback) ->
        @callbackObjects.push callback
    
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

ProtocolMessages[0] =
class ClientLoginMsg extends ProtocolMessage
    
    constructor: ({@name}) ->
        

ProtocolMessages[1] =
class ServerResponseCode extends ProtocolMessages
    
    constructor: ({@code}) ->
        

for cmdNr, msg of ProtocolMessages
    msg.prototype.__cmd = parseInt(cmdNr)
     