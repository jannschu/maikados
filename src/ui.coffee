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
            '#FF8000', '#F00', '#1FA53A', '#FC599F',
            '#F6AB00', '#68CCFF', '#0048AB', '#5D1E9B']
    
    fieldRows: [
        [7,6,5,4,3,2,1,0],
        [2,7,4,1,6,3,0,5],
        [1,4,7,2,5,0,3,6],
        [4,5,6,7,0,1,2,3]
    ]
    
    swapTime: 2000
    moveTime: 1500

UI.fieldRows = UI.fieldRows.concat [row.reverse()] for row in $.extend(true, [], UI.fieldRows).reverse()

class UIField
    # TODO: full swap support (to be considered at drawing etc.)
    
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
    
    constructor: (element, @gameField) ->
        @progressBar = new Raphael(element, 600, 10)
        @paper = new Raphael(element, 600, 600)
        
        for name, data of signs
            [path, attrs] = data
            (new Raphael('uiElements', 30, 30)).path(path).attr(attrs)
            $("#uiElements svg:first").attr(id: "svg-#{name}")
        
        $('#nicknameErrorMsg').prepend($('#svg-warn'))
        
        $.jnotify.setup(delay: 8000)
        
        @pieces = {}
        
        @fieldSize = @paper.width / 8 # should be a square
        @swapped = false
        @backgroundPieces = ([] for col in [0..7])
        
        @_drawBackground()
        @progressBarBalls = @_drawProgressBar()
        
        @loading = off
    
    getFieldSize: () ->
        @fieldSize
    
    addGamingPiece: (piece) ->
        @pieces[piece.getID()] ?= new UIGamingPiece(piece, this)
        @lastPieceID = piece.getID()
    
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
    
    setLoading: (value) ->
        if value != @loading
            @loading = value
            if value
                $("*").addClass 'wait-cursor'
            else
                $("*").removeClass 'wait-cursor'
    
    getNickName: (testCallback) ->
        $('#startButton').click () =>
            getNickBox = $.fancybox(
                title: 'Nicknamen wählen…',
                href: '#getPlayerName',
                modal: on,
                transitionIn: 'none',
                transitionOut: 'none',
                'onStart': (() ->
                    ok = $.fancybox.close
                    getNew = (errorMsg) ->
                        $('#chooseNickname').attr('disabled', '')
                        error = $('#nicknameErrorMsg')
                        $('span.msg', error).text(errorMsg)
                        if error.is(':hidden')
                            error.slideDown('slow')
                        else
                            error.effect('highlight', {}, 2000)
                    event = () ->
                        $('#chooseNickname').attr('disabled', 'disabled')
                        nick = $('#nickname').val()
                        ai = $('#getPlayerName input[name=ai]').val() is 'yes'
                        testCallback(nick, ai, (ok: ok, getNew: getNew))
                    $('#chooseNickname').click(event))
            )
    
    postNotification: (msg, sign = 'info') ->
        sign = if signs[sign] then sign else 'info'
        elem = $("#svg-#{sign}").clone().attr(id: null)
        $.jnotify(msg, create: (e) -> $('.jnotify-message', e).prepend(elem))
    
    getPieceSelection: (validPieces, callback) ->
        # TODO: swap
        fields = []
        elements = []
        b = $('body')
        for piece in validPieces
            do (piece) =>
                {row, col} = @pieces[piece]
                fields.push(row * 8 + col)
                
                el = $("#piece-#{piece}").add(@backgroundPieces[row][col].node)
                elements.push(el)
                el.hover((() => b.addClass('pointer-cursor') unless @loading),
                         (() -> b.removeClass('pointer-cursor')))
                
                el.click () =>
                    for e in elements
                        e.unbind('click').unbind('mouseenter').unbind('mouseleave')
                    b.removeClass('pointer-cursor')
                    @_highlightFields([0..63])
                    window.setTimeout (() -> callback(piece) if callback), 0
        @_highlightFields(fields)
    
    getMoveDestination: (pieceID, validFields, callback) ->
        clickableBackgroundPieces = []
        b = $('body')
        for field in validFields
            if @swapped
                field = 63-field
            row = Math.floor field / 8
            col = field - row * 8
            do (field) =>
                clickableBackgroundPieces.push(elem = @backgroundPieces[row][col].node)
                $(elem).click( () =>
                    for tile in clickableBackgroundPieces
                        $(tile).unbind('click').unbind('mouseenter').unbind('mouseleave')
                    b.removeClass('pointer-cursor')
                    @_highlightFields([0..63])
                    window.setTimeout (() -> callback(field) if callback), 0
                ).hover((() => b.addClass('pointer-cursor') unless @loading),
                        (() -> b.removeClass('pointer-cursor')))
        @_highlightFields(validFields)
    
    doMove: (pieceID, destField, callback) ->
        if @swapped
            field = 63-destField
        else
            field = destField
        row = Math.floor field / 8
        col = field - row * 8
        @pieces[pieceID].move(row, col, callback)
    
    ###
    - private methods
    ###
    
    _registerHoverFun: () ->
        registerHoverFor = (ui, piece, nr) ->
            fields = []
            row = Math.floor(nr / 8)
            col = nr - row * 8
            color = UI.fieldRows[row][col]
            for x in [0..7]
                for y in [0..7]
                    fields.push y * 8 + x if UI.fieldRows[y][x] is color
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
        
        size = 2
        
        for i, status of fieldMap
            row = Math.floor i / 8
            col = i - row * 8
            piece = @backgroundPieces[row][col]
            
            attrs = if status
                (
                    opacity: 1
                    x: (col * @fieldSize)
                    y: (row * @fieldSize)
                    height: @fieldSize
                    width: @fieldSize)
            else
                (
                    opacity: 0.5
                    x: (col * @fieldSize) + size / 2
                    y: (row * @fieldSize) + size / 2
                    height: @fieldSize - size
                    width: @fieldSize - size)
            
            piece.stop()
            piece.animate(attrs, 300)
    
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
        for rowData, rowNr in UI.fieldRows
            for colorIndex, col in rowData
                rect = @paper.rect(@fieldSize * col, @fieldSize * rowNr, @fieldSize, @fieldSize, 5)
                rect.attr fill: UI.colorMap[colorIndex]
                @backgroundPieces[rowNr][col] = rect
    
    _swapPieces: () ->
        for pieceID, piece of @pieces
            piece.swap()

class UIGamingPiece
    
    constructor: (@piece, @field) ->
        @swapped = false
        @row = @piece.getRow()
        @col = @piece.getCol()
        @set = @_drawPiece()
    
    swap: () ->
        time = UI.swapTime
        
        fieldSize = @field.getFieldSize()
        paper = @field.paper
        
        if @swapped
            oldCol = 7 - @col
            oldRow = 7 - @row
        else
            oldCol = @col
            oldRow = @row
        
        @swapped = !@swapped
        
        dx = (7 - 2 * oldCol) * fieldSize
        dy = (7 - 2 * oldRow) * fieldSize
        
        id = "#{@piece.getID()}"
        
        getAnimAttr = (obj, opc) ->
            fll = obj.attr 'fill'
            animAttr = 
                "10%": (opacity: 0, fill: 'none')
                "90%": (callback: () -> obj.translate(dx, dy))
                "100%": (opacity: opc, fill: fll)
        
        getNoAnimAttr = (obj) ->
            attrs =
                "0%": (callback: () -> obj.attr(opacity: 0).translate(dx, dy))
                "100%": (callback: () -> obj.attr opacity: 1)
        
        animElems = [
            ( # the shadow
                elem: @set[0]
                attr: getAnimAttr(@set[0], 0)),
            ( # the top circle
                elem: @set[1]
                attr: getAnimAttr(@set[1], 1))
            ( # the color sign
                elem: @set[2]
                attr: getNoAnimAttr(@set[2]))
                    
        ]
        
        for tooth in @dragonToothPieces
            for n in tooth
                animElems.push(
                    elem: n
                    attr: getNoAnimAttr(n))
        
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
        
        dx = (@col - oldCol) * fieldSize
        dy = (@row - oldRow) * fieldSize
        attrs = (translation: "#{dx} #{dy}") 
        
        animElems = [
            ( # shadow
                elem: @set[0]
                attr: attrs),
            ( # top circle
                elem: @set[1]
                attr: attrs),
            ( # color sign
                elem: @set[2]
                attr: attrs)
        ]
        
        for tooth in @dragonToothPieces
            for elem in tooth
                animElems.push(
                    elem: elem
                    attr: attrs)
        
        
        for {elem, attr} in animElems
            elem.toFront()
            if withObj
                elem.animateWith withObj, attr, time
            else
                elem.animate attr, time, callback # callback here because it will only be executed once, hopefully
                withObj = elem
    
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
        
        set = []
        r = fieldSize * 0.5 * 0.8
        
        cx = fieldSize * (@col + 0.5)
        cy = fieldSize * (@row + 0.5)
        
        set.push paper.circle(cx + r / 7, cy + r / 7, r * 1.05).
            attr(fill: 'rblack:80%-rgba(0, 0, 0, 0)', stroke: 'none')
        bg = ['#1B1B1B', '#EEE']
        set.push paper.circle(cx, cy, r).
            attr(
                stroke: '#7E7E7E',
                fill: bg[@piece.getSide()],
                'stroke-width': 1)
        
        colorID = @piece.getColorID()
        colorChar = @_fitPathInto(UICharaters[colorID], cx, cy, r)
        set.push colorChar.attr(fill: UI.colorMap[colorID], stroke: 'none')
        
        $(group).append(setItem) for setItem in set
        
        # dragon tooths
        @dragonToothPieces = []
        
        dragonTeeth = @piece.getDragonTooths()
        if dragonTeeth > 0
            rr = r * 0.82
            deltaDeg = 360 / 8
            diffDeg = 360 / (4 * 8)
            rad = Raphael.rad(diffDeg / 2)
            size = Math.sin(rad) * rr * 2
            y = cy - (size / 2 + Math.cos(rad) * rr)
            path = 'M 4 0L0 6L4 5L8 6z'
            initDegOffset = -(diffDeg / 2 * (dragonTeeth - 1)) 
            for n in [1..dragonTeeth]
                dragonToothPiece = []
                for i in [0..7]
                    angle = initDegOffset + i * deltaDeg + (n - 1) * diffDeg
                    el = @_fitPathInto(path, cx, y, size).
                        rotate(angle, cx, cy).attr(fill: bg[1 - @piece.getSide()], stroke: 'none')
                    dragonToothPiece.push el
                    $(group).append(el)
                @dragonToothPieces.push dragonToothPiece
        
        set
    
    _fitPathInto: (path, cx0, cy0, width0, height0 = width0) ->
        
        elem = @field.paper.path(path)
        {width, height} = elem.getBBox()
        
        elem.scale(Math.max(width0, height0) / Math.max(width, height))
        {width, height, x, y} = elem.getBBox()
        elem.translate((cx0 - width / 2) - x, (cy0 - height / 2) - y)

