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

class GameState
    
    constructor: (initState) ->
        @_currentState = initState
        @_paused = false
        @_msgStack = []
    
    sendEvent: (type, msg) ->
        if @_paused
            @_msgStack.push [type, msg]
        else
            window.setTimeout((() =>
                if @_paused
                    @_msgStack.push [type, msg]
                else
                    @_currentState = this[@_currentState](type, msg)
            ), 0)
    
    pauseFSM: () ->
        @_paused = true
    
    resumeFSM: () ->
        @_paused = false
        processStack = () =>
            if @_msgStack.length isnt 0 and !@_paused
                [type, msg] = @_msgStack.shift()
                @_currentState = this[@_currentState](type, msg)
                window.setTimeout (() -> processStack()), 0
        processStack()
    
    IDLE: () -> 'IDLE'