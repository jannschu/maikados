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

class UIField
    # TODO: full swap support (to be considered at drawing etc.)
    
    colorMap = [
        '#F00', '#744700','#FF8000', '#F3F000',
        '#80FF00', '#C9FFEB', '#8D0DCE', '#0017F1']
        
    
    fieldRows = [
        [7,6,5,4,3,2,1,0],
        [2,7,4,1,6,3,0,5],
        [1,4,7,2,5,0,3,6],
        [4,5,6,7,0,1,2,3]
    ]
    fieldRows = fieldRows.concat [row.reverse()] for row in $.extend(true, [], fieldRows).reverse()
    
    constructor: (@paper) ->
        @pieces = {}
        @width = @paper.width / 8
        @height = @paper.height / 8
        @swapped = false
        @backgroundPieces = ([] for col in [0..7])
        @drawBackground()
    
    addGamingPiece: (piece) ->
        if oldPiece = @pieces[piece.getID()]
            oldPiece.set.remove()
        
        @pieces[piece.getID()] =
            obj: piece,
            row: piece.getRow(),
            col: piece.getCol(),
            set: @createPieceSet(piece)
    
    drawBackground: ->
        for rowData, rowNr in fieldRows
            for colorIndex, col in rowData
                rect = @paper.rect(@width * col, @height * rowNr, @width, @height, 5)
                rect.attr fill: colorMap[colorIndex]
                @backgroundPieces[rowNr][col] = rect
    
    swapBackground: (callback) ->
        animationObj = null
        time = 2000
        @swapped = !@swapped
        
        for row in [0..7]
            for col in [0..7]
                me = @backgroundPieces[row][col]
                if row > 3
                    @backgroundPieces[row][col] = @backgroundPieces[7 - row][7 - col]
                    @backgroundPieces[7 - row][7 - col] = me
                attr =
                    "50%" : (x: (7 - col) * @width, rotation: 45)
                    "100%": (y: (7 - row) * @height, rotation: 0)
                if animationObj
                   me.animateWith animationObj, attr, time
                else
                    animationObj = me
                    attr["100%"].callback = () =>
                        callback?()
                    me.animate attr, time                        
    
    createPieceSet: (piece) ->
        row = piece.getRow()
        col = piece.getCol()
        [bg, color] = if piece.getSide() is 0 then ['#1B1B1B', '#333-#1B1B1B'] else ['#EEE', '#CCC-#EEE']
        strokeAttr =
            stroke: (if piece.getSide() is 0 then 'black' else '#737373'),
            'stroke-width': 1
        
        diff = 0.15
        
        set = @paper.set()
        rx = @width * 0.5 * 0.9
        ry = @height * 0.5 * 0.6
        top = @height * (row + 0.49)
        
        drawBottomEllipse = () =>
            path = @paper.path(getBottomEllipsePathStr(row, col, @height, @width))
            path.attr(fill: "15-#{color}").attr(strokeAttr)
            return path
        
        bottom = drawBottomEllipse()
        
        ellipseTop = @paper.ellipse(@width * (col + 0.5), top, rx, ry)
        ellipseTop.attr(fill: "60-#{color}").attr(strokeAttr)
        
        ellipseColor = @paper.ellipse(@width * (col + 0.5), top, rx, ry)
        bg = Raphael.getRGB(bg)
        ellipseColor.attr(fill: "r(.5,.6)#{colorMap[piece.getColorID()]}:5%-rgba(#{bg.r},#{bg.g},#{bg.b},0)", stroke: 'none', opacity: 0)
        
        # dragon tooths
        r = (@width * 0.15 + @height * 0.15) / 2
        dragonPositions = [
            [],
            [[@width * (col + 0.38), top - ry * 0.9]],
            [[@width * (col + 0.34), top - ry], [@width * (col + 0.66), top - ry]],
            [[@width * (col + 0.5), top - ry], [@width * (col + 0.25), top - ry * 0.6], [@width * (col + 0.75), top - ry * 0.6]],
            [[@width * (col + 0.35), top - ry * 0.9], [@width * (col + 0.18), top - ry / 3], [@width * (col + 0.65), top - ry * 0.9], [@width * (col + 0.82), top - ry / 3]]
        ][piece.getDragonTooths()]
        lastBall = ellipseColor
        for [x, y] in dragonPositions
            deg = '200°'
            ball = @paper.set(
                @paper.ellipse(x, y + r, r, r / 1.5).
                    attr(fill: "rhsb(#{deg},1,.25)-hsb(#{deg},1,.25)", stroke: 'none', opacity: 0),
                @paper.ellipse(x, y, r, r).
                    attr(fill: "r(.5,.9)hsb(#{deg},1,.75)-hsb(#{deg},.5,.25)", stroke: 'none'),
                @paper.ellipse(x, y, r - r/5, r - r/20).
                    attr(stroke: 'none', fill: 'r(.5,.1)#ccc-#ccc', opacity: 0)
            )
            lastBall = ball
        
        set.push(ellipseTop, bottom, ball)
        return set
