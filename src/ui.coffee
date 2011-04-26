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
        '#F00', '#F305FC', '#FF8000', '#F3F000',
        '#80FF00', '#C9FFEB', '#8D0DCE', '#0017F1']
    
    swapTime: 2000
    moveTime: 1500

class UIField
    # TODO: full swap support (to be considered at drawing etc.)
    
    fieldRows = [
        [7,6,5,4,3,2,1,0],
        [2,7,4,1,6,3,0,5],
        [1,4,7,2,5,0,3,6],
        [4,5,6,7,0,1,2,3]
    ]
    fieldRows = fieldRows.concat [row.reverse()] for row in $.extend(true, [], fieldRows).reverse()
    
    signs =
        info: ['M16,4.938c-7.732,0-14,4.701-14,10.5c0,1.981,0.741,3.833,2.016,
            5.414L2,25.272l5.613-1.44c2.339,1.316,5.237,2.106,8.387,2.106c7.732,0,14-4.701,
            14-10.5S23.732,4.938,16,4.938zM16.868,21.375h-1.969v-1.889h1.969V21.375zM16.772,
            18.094h-1.777l-0.176-8.083h2.113L16.772,18.094z',
            {fill: '#F5F6F5', stroke: 'none'}]
        warn: ['M29.225,23.567l-3.778-6.542c-1.139-1.972-3.002-5.2-4.141-7.172l-3.778-6.542
            c-1.14-1.973-3.003-1.973-4.142,0L9.609,9.853c-1.139,1.972-3.003,
            5.201-4.142,7.172L1.69,23.567c-1.139,1.974-0.207,3.587,2.071,3.587h
            23.391C29.432,27.154,30.363,25.541,29.225,23.567zM16.536,24.58h-2.241
            v-2.151h2.241V24.58zM16.428,20.844h-2.023l-0.201-9.204h2.407L16.428,20.844z',
            {fill: '#BA4143', stroke: 'none'}]
    
    constructor: (element) ->
        @progressBar = new Raphael(element, 600, 10)
        @paper = new Raphael(element, 600, 600)
        
        for name, data of signs
            [path, attrs] = data
            (new Raphael('uiElements', 30, 30)).path(path).attr(attrs)
            $("#uiElements svg:first").attr(id: "svg-#{name}")
        
        $('#nicknameErrorMsg').prepend($('#svg-warn'))
        
        $.jnotify.setup(delay: 5000)
        
        @pieces = {}
        @fieldSize = @paper.width / 8 # should be a square
        @swapped = false
        @backgroundPieces = ([] for col in [0..7])
        
        @_drawBackground()
        @progressBarBalls = @_drawProgressBar()
    
    getFieldSize: () ->
        @fieldSize
    
    addGamingPiece: (piece) ->
        @pieces[piece.getID()] ?= new UIGamingPiece(piece, this)
    
    setProgressBar: (val) ->
        return unless 0 <= val <= 100
        balls = Math.round((val / 100) * @progressBarBalls.length)
        r = @progressBar.height / 2
        for ball, nr in @progressBarBalls
            if nr < balls
                ball.animate(('r': r - 1), 300)
            else
                ball.animate(('r': 0), 300)
    
    swap: (callback) ->
        @_swapBackground(callback)
        @_swapPieces()
    
    getNickName: (testCallback) ->
        $('#startButton').click () =>
            getNickBox = $.fancybox(
                title: 'Nicknamen wählen…',
                href: '#getPlayerName',
                transitionIn: 'none',
                transitionOut: 'none',
                modal: on,
                'onStart': (() ->
                    ok = $.fancybox.close
                    getNew = (errorMsg) ->
                        error = $('#nicknameErrorMsg')
                        $('span.msg', error).text(errorMsg)
                        if error.is(':hidden')
                            error.slideDown('slow')
                        else
                            error.effect('highlight', {}, 2000)
                    event = () ->
                        nick = $('#nickname').val()
                        testCallback(nick, (ok: ok, getNew: getNew))
                    $('#chooseNickname').click(event))
            )
    
    postNotification: (msg, sign = 'info') ->
        sign = if signs[sign] then sign else 'info'
        elem = $("#svg-#{sign}").clone().attr(id: null)
        $.jnotify(msg, create: (e) -> $('.jnotify-message', e).prepend(elem))
    
    getPieceSelection: (validPieces, callback) ->
        fields = []
        for piece in validPieces
            $("#piece-" + piece).click( () =>
                $("#piece-" + piece).unbind("click")
                callback()
            )
            fields.push(@pieces[piece].row * 8 + @pieces[piece].col) # TODO: swap
        @_highlightFields(fields)
    
    getMoveDestination: (pieceID, validFields, callback) ->
        clickableBackgroundPieces = []
        for field in validFields
            if @swapped
                field = 63-field
            row = Math.floor field / 8
            col = field - row * 8
            clickableBackgroundPieces.push(@backgroundPieces[row][col].node)
            do (field) =>
                elem = @backgroundPieces[row][col].node
                $(elem).click( () ->
                    for tile in clickableBackgroundPieces
                        $(tile).unbind("click")
                    callback(field)
                )
        @_highlightFields(validFields)
    
    doMove: (pieceID, destField, callback) ->
        if @swapped
            field = 63-destField
        else
            field = destField
        row = Math.floor field / 8
        col = field - row * 8
        @pieces[pieceID].insertAfter(@pieces["1-7"])
        @pieces[pieceID].move(row, col, () =>
            @_highlightFields([0..63])
            @pieces[pieceID].insertBefore(@pieces["1-7"])
            callback()
        )
    
    ###
    - private methods
    ###
    
    _registerHoverFun: () ->
        registerHoverFor = (ui, piece, nr) ->
            fields = []
            row = Math.floor(nr / 8)
            col = nr - row * 8
            color = fieldRows[row][col]
            for x in [0..7]
                for y in [0..7]
                    fields.push y * 8 + x if fieldRows[y][x] is color
            over = () ->
                ui._highlightFields fields
            out = () ->
                ui._highlightFields [0..63]
            piece.hover over, out
        
        for x in [0..7]
            for y in [0..7]
                registerHoverFor this, @backgroundPieces[y][x], y * 8 + x
    
    _highlightFields: (fields) ->
        fieldMap = {}
        for i in [0..63]
            fieldMap[i] = off
        for i in fields
            fieldMap[i] = on

        for i, status of fieldMap
            row = Math.floor i / 8
            col = i - row * 8
            piece = @backgroundPieces[row][col]
            opacity = if status then 1 else 0.2
            piece.stop()
            piece.animate(('fill-opacity': opacity), 300)
    
    _swapBackground: (callback) ->
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
                    attr["100%"].callback = callback if callback
                    me.animate attr, time
    
    _drawProgressBar: () ->
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
            balls.push @progressBar.circle((2 * r + a) * (i - 1) + r, r, r - 1).
                attr(fill: "0-hsb(#{hue}°, .5, .5)-hsb(#{newHue}°, .5, .5)", 'fill-opacity': '50%', stroke: 'none')
            hue = newHue
        balls
    
    _drawBackground: ->
        for rowData, rowNr in fieldRows
            for colorIndex, col in rowData
                rect = @paper.rect(@fieldSize * col, @fieldSize * rowNr, @fieldSize, @fieldSize, 5)
                rect.attr fill: UI.colorMap[colorIndex]
                @backgroundPieces[rowNr][col] = rect
    
    _swapPieces: () ->
        for pieceID, piece of @pieces
            piece.swap(@backgroundPieces["1-7"])

class UIGamingPiece
    
    constructor: (@piece, @field) ->
        @swapped = false
        @row = @piece.getRow()
        @col = @piece.getCol()
        @set = @_drawPiece()
    
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

        yDiffPath = @_getBottomEllipsePathStr(oldRow, 7-oldCol)
        animBottomEllipse =
            elem: @set[1]
            attr:
                "50%" : (path: yDiffPath, callback: () => @set[1].attr(path: yDiffPath))
                "100%": (path: @_getBottomEllipsePathStr(7-oldRow, 7-oldCol))
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
    
    # TODO: create _move() function which does ONLY does the animation etc. so we can use that in swap() and move()
    move: (destRow, destCol, callback) -> # uses post-swap positions (i.e. gfx positions, not the "true" ones)
        time = UI.moveTime
        
        fieldSize = @field.getFieldSize()
        paper = @field.paper

        oldCol = @col
        oldRow = @row
        
        
        if @swapped
            @col = 7 - destCol
            @row = 7 - destRow
        else
            @col = destCol
            @row = destRow
        
        @piece.col = @col
        @piece.row = @row
        
        getEllipseAnimAttr = (obj) =>
            animAttr = 
                cx: destCol * fieldSize + (obj.attr('cx')-oldCol*fieldSize)
                cy: destRow * fieldSize + (obj.attr('cy')-oldRow*fieldSize)
            return animAttr

        animElems = []

        animTopEllipse =
            elem: @set[0]
            attr: getEllipseAnimAttr(@set[0])
        animElems.push(animTopEllipse)

        yDiffPath = @_getBottomEllipsePathStr(oldRow, 7-oldCol)
        animBottomEllipse =
            elem: @set[1]
            attr: {path: @_getBottomEllipsePathStr(destRow, destCol)}
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
                elem.animate attr, time, callback # callback here because it will only be executed once, hopefully
                withObj = elem
    
    insertAfter: (other) ->
        lastOthElem = other.set[2]
        for tooth in other.set[3]
            lastOthElem = tooth[2]
        this.forEachSetElem( (elem, prev) =>
            if prev
                lastOthElem = prev
            elem.insertAfter(lastOthElem)
        )
    
    insertBefore: (other) ->
        lastOthElem = other.set[0]
        this.forEachSetElem( (elem, prev) =>
            elem.insertBefore(lastOthElem)
        )
    
    forEachSetElem: (func) ->
        func(@set[1])
        func(@set[0], @set[1])
        func(@set[2], @set[0])
        previous = @set[2]
        for tooth in @set[3]
            for elem in tooth
                func(elem, previous)
                previous = elem
    
    
    ###
    - private methods
    ###
    
    _drawPiece: () ->
        fieldSize = @field.getFieldSize()
        paper = @field.paper
        
        # create a new SVG group for this piece
        svgns = "http://www.w3.org/2000/svg"
        group = document.createElementNS(svgns, "g")
        group.id = "piece-" + @piece.getID()
        paper.canvas.appendChild(group)
        
        [bg, color] = if @piece.getSide() is 0 then ['#1B1B1B', '#333-#1B1B1B'] else ['#EEE', '#CCC-#EEE']
        strokeAttr =
            stroke: (if @piece.getSide() is 0 then 'black' else '#737373'),
            'stroke-width': 1
        
        diff = 0.15
        
        set = []
        rx = fieldSize * 0.5 * 0.9
        ry = fieldSize * 0.5 * 0.6
        top = fieldSize * (@row + 0.49)
        
        bottom = paper.path(@_getBottomEllipsePathStr(@row, @col, fieldSize, fieldSize)).
            attr(fill: "15-#{color}").attr(strokeAttr)
        group.appendChild(bottom.node)
        
        ellipseTop = paper.ellipse(fieldSize * (@col + 0.5), top, rx, ry).
            attr(fill: "60-#{color}").attr(strokeAttr)
        group.appendChild(ellipseTop.node)
        
        bg = Raphael.getRGB(bg)
        ellipseColor = paper.ellipse(fieldSize * (@col + 0.5), top, rx, ry).
            attr(fill: "r(.5,.6)#{UI.colorMap[@piece.getColorID()]}:5%-rgba(#{bg.r},#{bg.g},#{bg.b},0)", stroke: 'none', opacity: 0)
        group.appendChild(ellipseColor.node)
        
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
        currentTooth = []
        
        for [x, y] in dragonPositions
            deg = '200°'
            currentTooth = [
                paper.ellipse(x, y + r, r, r / 1.5).
                    attr(fill: "rhsb(#{deg},1,.25)-hsb(#{deg},1,.25)", stroke: 'none', opacity: 0),
                paper.ellipse(x, y, r, r).
                    attr(fill: "r(.5,.9)hsb(#{deg},1,.75)-hsb(#{deg},.5,.25)", stroke: 'none'),
                paper.ellipse(x, y, r - r/5, r - r/20).
                    attr(stroke: 'none', fill: 'r(.5,.1)#ccc-#ccc', opacity: 0)
            ]
            dragonTeeth.push(currentTooth)
            group.appendChild(currentTooth[0].node)
            group.appendChild(currentTooth[1].node)
            group.appendChild(currentTooth[2].node)
        
        set.push(ellipseTop, bottom, ellipseColor, dragonTeeth)
        set
    
    _getBottomEllipsePathStr: (row, col) ->
        fieldSize = @field.getFieldSize()
        diff = 0.15
        rx = fieldSize * 0.5 * 0.9
        ry = fieldSize * 0.5 * 0.6
        topX = fieldSize * (row + 0.49)
        length = fieldSize * diff
        
        left = fieldSize * (col + (1 - 0.9) * 0.5)
        
        "M#{left},#{topX} v#{length} a#{rx},#{ry} 0 0 0 #{fieldSize * 0.9},0 v-#{length}Z"
