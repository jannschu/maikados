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
        @socket.connect()
        
        @socket.on 'message', (msg) =>
            (obj.sendEvent 'packet', msg) for obj in @callbackObjects
        
        
        
    # callback should be an instance of GameState
    onMessage: (callback) ->
        @callbackObjects.push callback
    