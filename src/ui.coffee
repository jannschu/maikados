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

UI = # UI constants
    colorMap: [
        '#F00', '#744700','#FF8000', '#F3F000',
        '#80FF00', '#C9FFEB', '#8D0DCE', '#0017F1']
    
    swapTime: 2000

class UIField
    # TODO: full swap support (to be considered at drawing etc.)
    
    fieldRows = [
        [7,6,5,4,3,2,1,0],
        [2,7,4,1,6,3,0,5],
        [1,4,7,2,5,0,3,6],
        [4,5,6,7,0,1,2,3]
    ]
    fieldRows = fieldRows.concat [row.reverse()] for row in $.extend(true, [], fieldRows).reverse()
    
    constructor: (element) ->
        @progressBar = new Raphael(element, 600, 10)
        @paper = new Raphael(element, 600, 600)
        @pieces = {}
        @fieldSize = @paper.width / 8 # should be a square
        
        @swapped = false
        @backgroundPieces = ([] for col in [0..7])
        @drawBackground()
        @progressBarBalls = @drawProgressBar()
    
    getFieldSize: () ->
        @fieldSize
    
    addGamingPiece: (piece) ->
        @pieces[piece.getID()] ?= new UIGamingPiece(piece, this)
    
    drawProgressBar: () ->
        width = @progressBar.width
        r = @progressBar.height / 2
        a = r / 2
        n = Math.round((width + a) / (2 * r + a))
        a = (width - n * 2 * r) / (n - 1)
        hue = 0
        step = 360 / n
        balls = []
        for i in [1..n]
            newHue = hue + step
            balls.push @progressBar.circle((2 * r + a) * (i - 1) + r, r, r).attr(fill: "0-hsb(#{hue}°, .5, .5)-hsb(#{newHue}°, .5, .5)", 'fill-opacity': '50%')
            hue = newHue
        balls
    
    setProgressBar: (val) ->
        return unless 0 <= val <= 100
        balls = Math.round((val / 100) * @progressBarBalls.length)
        r = @progressBar.height / 2
        for ball, nr in @progressBarBalls
            if nr < balls
                ball.animate(('r': r), 300)
            else
                ball.animate(('r': 0), 300)
    
    drawBackground: ->
        for rowData, rowNr in fieldRows
            for colorIndex, col in rowData
                rect = @paper.rect(@fieldSize * col, @fieldSize * rowNr, @fieldSize, @fieldSize, 5)
                rect.attr fill: UI.colorMap[colorIndex]
                @backgroundPieces[rowNr][col] = rect
    
    swapBackground: (callback) ->
        animationObj = null
        time = UI.swapTime
        @swapped = !@swapped
        
        for row in [0..7]
            for col in [0..7]
                me = @backgroundPieces[row][col]
                if row > 3
                    @backgroundPieces[row][col] = @backgroundPieces[7 - row][7 - col]
                    @backgroundPieces[7 - row][7 - col] = me
                attr =
                    "50%" : (x: (7 - col) * @fieldSize, rotation: 45)
                    "100%": (y: (7 - row) * @fieldSize, rotation: 0)
                if animationObj
                   me.animateWith animationObj, attr, time
                else
                    animationObj = me
                    attr["100%"].callback = () =>
                        callback?()
                    me.animate attr, time
    
    swapPieces: () ->
        for pieceID, piece of @pieces
            piece.swap(@backgroundPieces["1-7"])
    
    swap: (callback) ->
        this.swapBackground(callback)
        this.swapPieces()


class UIGamingPiece
    
    constructor: (@piece, @field) ->
        @swapped = false
        @row = @piece.getRow()
        @col = @piece.getCol()
        @set = @drawPiece()
    
    drawPiece: () ->
        fieldSize = @field.getFieldSize()
        paper = @field.paper
        
        [bg, color] = if @piece.getSide() is 0 then ['#1B1B1B', '#333-#1B1B1B'] else ['#EEE', '#CCC-#EEE']
        strokeAttr =
            stroke: (if @piece.getSide() is 0 then 'black' else '#737373'),
            'stroke-width': 1
        
        diff = 0.15
        
        set = []
        rx = fieldSize * 0.5 * 0.9
        ry = fieldSize * 0.5 * 0.6
        top = fieldSize * (@row + 0.49)
        
        bottom = paper.path(@getBottomEllipsePathStr(@row, @col, fieldSize, fieldSize)).
            attr(fill: "15-#{color}").attr(strokeAttr)
        
        ellipseTop = paper.ellipse(fieldSize * (@col + 0.5), top, rx, ry).
            attr(fill: "60-#{color}").attr(strokeAttr)
        
        bg = Raphael.getRGB(bg)
        ellipseColor = paper.ellipse(fieldSize * (@col + 0.5), top, rx, ry).
            attr(fill: "r(.5,.6)#{UI.colorMap[@piece.getColorID()]}:5%-rgba(#{bg.r},#{bg.g},#{bg.b},0)", stroke: 'none', opacity: 0)
        
        # dragon tooths
        r = fieldSize * 0.15
        dragonPositions = [
            [],
            [[fieldSize * (@col + 0.38), top - ry * 0.9]],
            [[fieldSize * (@col + 0.34), top - ry], [fieldSize * (@col + 0.66), top - ry]],
            [[fieldSize * (@col + 0.5), top - ry], [fieldSize * (@col + 0.25), top - ry * 0.6], [fieldSize * (@col + 0.75), top - ry * 0.6]],
            [[fieldSize * (@col + 0.35), top - ry * 0.9], [fieldSize * (@col + 0.18), top - ry / 3], [fieldSize * (@col + 0.65), top - ry * 0.9], [fieldSize * (@col + 0.82), top - ry / 3]]
        ][@piece.getDragonTooths()]
        
        dragonTeeth = []
        
        for [x, y] in dragonPositions
            deg = '200°'
            dragonTeeth.push( [
                paper.ellipse(x, y + r, r, r / 1.5).
                    attr(fill: "rhsb(#{deg},1,.25)-hsb(#{deg},1,.25)", stroke: 'none', opacity: 0),
                paper.ellipse(x, y, r, r).
                    attr(fill: "r(.5,.9)hsb(#{deg},1,.75)-hsb(#{deg},.5,.25)", stroke: 'none'),
                paper.ellipse(x, y, r - r/5, r - r/20).
                    attr(stroke: 'none', fill: 'r(.5,.1)#ccc-#ccc', opacity: 0)
            ] )
        
        set.push(ellipseTop, bottom, ellipseColor, dragonTeeth)
        set
    
    swap: (withExtObj) ->
        time = UI.swapTime
        
        fieldSize = @field.getFieldSize()
        paper = @field.paper
        
        oldCol = @col
        oldRow = @row
        if @swapped
            oldCol = 7 - oldCol
            oldRow = 7 - oldRow
        
        @swapped = !@swapped
        
        getEllipseAnimAttr = (obj) =>
            animAttr = 
                "50%" : (cx: (7 - oldCol) * fieldSize + (obj.attr('cx')-oldCol*fieldSize))
                "100%": (cy: (7 - oldRow) * fieldSize + (obj.attr('cy')-oldRow*fieldSize))
            return animAttr
        
        animElems = []
        
        animTopEllipse =
            elem: @set[0]
            attr: getEllipseAnimAttr(@set[0])
        animElems.push(animTopEllipse)
        
        yDiffPath = @getBottomEllipsePathStr(oldRow, 7-oldCol)
        animBottomEllipse =
            elem: @set[1]
            attr:
                "50%" : (path: yDiffPath, callback: () => @set[1].attr(path: yDiffPath))
                "100%": (path: @getBottomEllipsePathStr(7-oldRow, 7-oldCol))
        animElems.push(animBottomEllipse)
        
        animEllipseColor =
            elem: @set[2]
            attr: getEllipseAnimAttr(@set[2])
        animElems.push(animEllipseColor)
        
        
        for tooth in @set[3]
            for elem in tooth
                animDragonTooth =
                    elem: elem
                    attr: getEllipseAnimAttr(elem)
                animElems.push(animDragonTooth)
        
        for {elem, attr} in animElems
            if withObj
                elem.animateWith withObj, attr, time
            else
                elem.animate attr, time
                withObj = elem
    
    getBottomEllipsePathStr: (row, col) ->
        fieldSize = @field.getFieldSize()
        diff = 0.15
        rx = fieldSize * 0.5 * 0.9
        ry = fieldSize * 0.5 * 0.6
        topX = fieldSize * (row + 0.49)
        length = fieldSize * diff
        
        left = fieldSize * (col + (1 - 0.9) * 0.5)
        
        "M#{left},#{topX} v#{length} a#{rx},#{ry} 0 0 0 #{fieldSize * 0.9},0 v-#{length}Z"
