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
                    @aiPlayer.getMove @field.getGamingPiece(@piece), @field, (m) =>
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
        
        @maxDepth = 6 # maximum depth of tree stuff
        @allFields = [] # potential fields, that is
        @iterIndices = [0] # indices of the currently selected potential fields of individual depth layers
        @depthIndex = 0 # index of the current depth layer
        @bestMove = null # {score: x, move: y)
    
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
        depthIndex = @depthIndex
        maxDepth = @maxDepth
        #console.debug("getmove")
        calcRecursively = (curPiece, curField, depth) ->
            depthIndex++
            depth++
            
            moves = curField.getAllowedMovesFor(curPiece.getID())
            bestMove = null
            worstMove = null
            totalScore = 0
            totalN = 0
            if moves.length == 0
                bestMove = {score: -5000, at_depth: depth-1, move: null} # TODO: calculate stuff correctly, while we're at it (depth factor)
                worstMove = {score: -50000, at_depth: depth-1, move: null}
                totalScore -= 2500
                totalN++
                if depth == 1
                    depthIndex--
                    depth--
                    return {best: bestMove, worst: worstMove, total: totalScore}
                #console.debug("block")
                if curPiece.getID()[0] == "1" # white blocked, neat-o
                    bestMove.score = 200
                    worstMove.score = 10 # TODO: calculate moves for the other side, then
                    totalScore += 2501
                if depth < maxDepth
                    # prepare for next turn-thingy
                    newField = owl.deepCopy(curField)
                    curPiece = newField.getGamingPiece(curPiece.getID())
                    newPieceID = curPiece.getID()[0]
                    # get tha new colah
                    newPieceID += ("-" + UI.fieldRows[curPiece.getRow()][curPiece.getCol()])
                    result = calcRecursively(newField.getGamingPiece(newPieceID), newField)
                    if result.worst.score + result.best.score > bestMove.score + worstMove.score
                        bestMove = result.best
                        bestMove.move = candidate
                    if result.worst.score + result.best.score < bestMove.score + worstMove.score
                        worstMove = result.worst
                        worstMove.move = candidate
                    totalScore += result.total
                    totalN++
            else
                # for a start, check if there is a winning move up in this %&!$
                bestMove = {score: 0, at_depth: depth-1, move: moves[0]}
                worstMove = {score: 0, at_depth: depth-1, move: moves[0]}
                #console.debug(curPiece.getID())
                calcCandidates = [] # candidates to be used in the next calculation
                isWinOrLose = false
                nSinceLoopStart = 0
                nFails = 0
                for candidate in moves
                    isWinOrLose = false
                    totalN++
                    nSinceLoopStart++
                    if curField.getGamingPiece(curPiece.getID()).getSide() == 0 && (candidate - 55) >= 1
                        # epic win
                        bestMove = {score: bestMove.score+(50*(maxDepth-depth)), at_depth: depth, move: candidate}
                        totalScore += 2600
                        if depth == 1
                            depthIndex--
                            depth--
                            return {best: bestMove, worst: worstMove, total: 100000000} # instawin
                        isWinOrLose = true
                        return {best: bestMove, worst: worstMove, total: totalScore}
                    if curField.getGamingPiece(curPiece.getID()).getSide() == 1 && candidate <= 7
                        # epic fail
                        totalN -= nSinceLoopStart
                        nSinceLoopStart = 1
                        nFails++
                        worstMove = {score: worstMove.score-(10000)*(maxDepth-depth), at_depth: depth, move: candidate}
                        totalScore -= 5000
                        #console.log("lose")
                        if depth == 2
                            #console.log("losedepth<2")
                            bestMove = {score: -30000000, at_depth: depth, move: candidate}
                            #alert("d:" + depth + "; cP:" + curPiece.getID() + "; tar:" + candidate)
                            worstMove.score = bestMove.score
                            depthIndex--
                            depth--
                            return {best: bestMove, worst: worstMove, total: -9999999999999}
                        isWinOrLose = true
                        return {best: bestMove, worst: worstMove, total: totalScore}
                    if not isWinOrLose
                        calcCandidates.push(candidate)
                
                # if not, then continue building the tree as long as the maximum depth has not been reached
                #bestMove = {score: 0, at_depth: depthIndex-1, move: moves[0]}
                #worstMove = {score: 0, at_depth: depthIndex-1, move: moves[0]}
                if depth < maxDepth
                    bestMoveThisRound = {score: 0, at_depth: depth, move: moves[0]}
                    worstMoveThisRound = {score: 0, at_depth: depth, move: moves[0]}
                    bestTotalThisRound = {move: moves[0], total: -9999999999999}
                    for candidate in calcCandidates
                        newField = owl.deepCopy(curField)
                        curPiece = newField.getGamingPiece(curPiece.getID())
                        
                        newField.doMove(curPiece.getID(), candidate)
                        #console.debug("curp")
                        #console.debug(curPiece.getID())
                        newPieceID = null
                        if not (bestMove.move == null) # not blocked
                            if curPiece.getID()[0] == "1"
                                newPieceID = "0"
                            else
                                newPieceID = "1"
                        else
                            newPieceID = curPiece.getID()[0]
                            #console.debug("newp")
                            #console.debug(newPieceID)
                        # get tha new colah
                        newPieceID += ("-" + UI.fieldRows[curPiece.getRow()][curPiece.getCol()])
                        #console.debug(newPieceID)
                        result = calcRecursively(newField.getGamingPiece(newPieceID), newField, depth)
  
                        if depth == 0
                            alert("this must not happen")
                        if depth == 1
                            console.log(bestTotalThisRound.total)
                            console.log(result.total)
                        if result.total > bestTotalThisRound.total
                            bestTotalThisRound.move = result.best.move
                            bestTotalThisRound.total = result.total
                            bestMove = result.best
                            bestMove.move = candidate
                        totalScore += result.total
                        totalN++
            #console.debug(depth)
            depthIndex--
            depth--
            bestMove.score += worstMove.score
            return {best: bestMove, worst: worstMove, total: totalScore/totalN}
        
        calc = () =>
            moveResult = calcRecursively(piece, field, 0)
            moveFound = true
            
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
                console.debug("foo")
                console.debug(moveResult)
                console.debug("total")
                console.debug(moveResult.total)
                #alert(moveResult.best.score)
                #alert(moveResult.worst.score)
                if moveResult.best.move == moveResult.worst.move
                    alert("shit")
                window.setTimeout (() -> callback(moveResult.best.move))
            else
                window.setTimeout (() -> calc()), 0
        window.setTimeout (() -> calc()), 0
    
    # Is called by the logic to say the AI: you were beaten by `name'
    youFailedNotification: (name) ->
        return "Do'h! Screw you #{name}."
