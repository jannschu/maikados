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
            '#FFE122', '#68CCFF', '#0048AB', '#5D1E9B']
    
    fieldRows: [
        [7,6,5,4,3,2,1,0],
        [2,7,4,1,6,3,0,5],
        [1,4,7,2,5,0,3,6],
        [4,5,6,7,0,1,2,3]
    ]
    
    swapTime: 2000

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
        game: ['M15.999,22.77l-8.884,6.454l3.396-10.44l-8.882-6.454l10.979,
            0.002l2.918-8.977l0.476-1.458l3.39,10.433h10.982l-8.886,6.454l3.397,
            10.443L15.999,22.77L15.999,22.77z',
            {fill: '#FFE800', stroke: 'none'}]
    
    constructor: (element, @gameField) ->
        @progressBar = new Raphael(element, 600 + 2, 10)
        @paper = new Raphael(element, 600 + 2, 600 + 2)
        
        for name, data of signs
            [path, attrs] = data
            (new Raphael('uiElements', 30, 30)).path(path).attr(attrs)
            $("#uiElements svg:first").attr(id: "svg-#{name}")
        
        $('#nicknameErrorMsg').prepend($('#svg-warn'))
        
        $.jnotify.setup(delay: 5000)
        
        @pieces = {}
        
        @fieldSize = (@paper.width - 2) / 8 # should be a square
        @swapped = false
        @backgroundPieces = ([] for col in [0..7])
        
        @_drawBackground()
        @progressBarBalls = @_drawProgressBar()
        
        @loading = off
        @stoppers = {}
    
    clearField: () ->
        @swapped = false
        for id, piece of @pieces
            piece.remove()
        @pieces = {}
    
    setLobby: (users, mode = 'set') ->
        @lobbyUsers ?= {}
        unless @callbacks
            f = () ->
            @lobbyCallbacks ?= (onRequestChallenge: f, onStartChallenge: f)
        
        lobby = $('#lobby').show()
        if mode is 'del'
            for name in users
                continue unless button = @lobbyUsers[name]
                $('a', button).unbind('click')
                delete @lobbyUsers[name]
                button.parent().slideUp(1000, () =>
                    button.parent().remove())
            length = 0
            for _, _ of @lobbyUsers
                ++length
            if length is 0
                $("ul", lobby).append('<li class="empty">Lobby leer… *hust*</li>')
        else if users.length isnt 0
            createButton = (name, status) =>
                span = (s) -> "<span class=\"buttons\">#{s}</span>"
                elem = switch status
                    when 'asksMe'
                        $(span '<a href="#">Spiel starten</a>').bind 'click', () =>
                            @lobbyCallbacks.onStartChallenge(name)
                            false
                    when 'askPlayer'
                        $(span '<a href="#">Herausfordern</a>').bind 'click', () =>
                            @lobbyCallbacks.onRequestChallenge(name)
                            false
                    when 'waitForAnswer' then $('<span class="buttons">Warte auf Antwort…</span>')
                elem.status = status
                return elem
            for {name, status}, pos in users
                buttons = @lobbyUsers[name]
                if buttons
                    continue if buttons.status == status
                    $("a", buttons).unbind 'click'
                    newButtons = createButton(name, status)
                    buttons.replaceWith(newButtons)
                    buttons = newButtons
                else
                    buttons = createButton(name, status)
                    li = $('<li><span class="name">' + name + '</span></li>').append(buttons).hide()
                    $('ul', lobby).append(li)
                do (li) ->
                    li.slideDown(1000, () -> li.effect('highlight', 1000))
                @lobbyUsers[name] = buttons
        
        @lobbyCallbacks
    
    clearLobby: () ->
        delete @lobbyUsers
        delete @lobbyCallbacks
        $("#lobby").hide()
        @_emptyLobby()
    
    _emptyLobby: () ->
        lobby = $("#lobby")
        $("a", lobby).unbind('click')
        $("li", lobby).remove()
    
    getFieldSize: () ->
        @fieldSize
    
    gameEndedNotice: (forWhat) ->
        # TODO
        if forWhat is 'won'
            alert 'Glückwunsch, gewonnen'
        else
            alert 'Leider verloren'
    
    update: (callback) ->
        callbackList = []
        traverse = () ->
            if callbackList.length is 0
                window.setTimeout (() -> callback()), 0 if callback
            else
                f = callbackList.shift()
                f(traverse)
        movePiecesList = []
        dragonTeethList = []
        for pieceID, piece of @gameField.pieces
            do (pieceID, piece) =>
                uiPiece = (@pieces[pieceID] ?= new UIGamingPiece(piece, this))
                row = piece.getRow()
                col = piece.getCol()
                # move
                if row != uiPiece.row or col != uiPiece.col
                    movePiecesList.push(@pieces[pieceID])
                # dragon tooth
                dragonTeeth = piece.getDragonTeeth()
                if dragonTeeth != uiPiece.dragonToothPieces.length
                    dragonTeethList.push uiPiece
        callbackList.push (f) ->
            last = movePiecesList.length - 1
            window.setTimeout((() -> f()), 0) if last < 0
            for p, i in movePiecesList
                p.move(if i is last then f else undefined)
        callbackList.push (f) ->
            last = dragonTeethList.length - 1
            window.setTimeout((() -> f()), 0) if last < 0
            for p, i in dragonTeethList
                p.updateDragonTeeth(if i is last then f else undefined)
        traverse()
    
    getUIPiece: (id) -> @pieces[id]
    
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
                    ok = () ->
                        $('#chooseNickname').prop('disabled', false)
                        $.fancybox.close()
                    getNew = (errorMsg) ->
                        $('#chooseNickname').prop('disabled', false)
                        error = $('#nicknameErrorMsg')
                        $('span.msg', error).text(errorMsg)
                        if error.is(':hidden')
                            error.slideDown('slow')
                        else
                            error.effect('highlight', (color: '#FCD9DA'), 2000)
                    event = () ->
                        $('#chooseNickname').prop('disabled', true)
                        nick = $('#nickname').val()
                        ai = $('#getPlayerName input[name=ai]:checked').val() is 'yes'
                        testCallback(nick, ai, (ok: ok, getNew: getNew))
                    $('#chooseNickname').click(event))
            )
    
    postNotification: (msg, sign = 'info') ->
        sign = if signs[sign] then sign else 'info'
        elem = $("#svg-#{sign}").clone().removeProp('id')
        $.jnotify(msg, create: (e) -> $('.jnotify-message', e).prepend(elem))
    
    stop: () ->
        for action, methods of @stoppers
            for m in methods
                m()
        @stoppers = {}
    
    getPieceSelection: (validPieces, callback) ->
        # TODO: swap
        fields = []
        elements = []
        b = $('body')
        @stoppers['getPieceSelection'] = [() -> b.removeClass('pointer-cursor')]
        @stoppers['getPieceSelection'].push () =>
            for e in elements
                e.unbind('click').unbind('mouseenter').unbind('mouseleave')
            @_highlightFields([0..63])
        
        for piece in validPieces
            do (piece) =>
                {row, col} = @pieces[piece]
                fields.push(row * 8 + col)
                
                el = $("#piece-#{piece}").add(@backgroundPieces[row][col].node)
                elements.push(el)
                el.hover((() => b.addClass('pointer-cursor') unless @loading),
                         (() -> b.removeClass('pointer-cursor')))
                
                el.click () =>
                    @stop()
                    window.setTimeout (() -> callback(piece)), 0 if callback
        @_highlightFields(fields)
    
    getMoveDestination: (pieceID, validFields, callback) ->
        clickableBackgroundPieces = []
        b = $('body')
        @stoppers['getMoveDestination'] = [() -> b.removeClass('pointer-cursor')]
        @stoppers['getMoveDestination'].push () =>
            for tile in clickableBackgroundPieces
                tile.unbind('click').unbind('mouseenter').unbind('mouseleave')
            @_highlightFields [0..63]
            @_highlightFields [], true
        
        for field in validFields
            if @swapped
                field = 63-field
            row = Math.floor field / 8
            col = field - row * 8
            do (field) =>
                elem = $(@backgroundPieces[row][col].node)
                if (p = @gameField.pieceOnField field) isnt null
                    elem = elem.add($("#piece-#{p.getID()}"))
                clickableBackgroundPieces.push(elem)
                $(elem).click( () =>
                    @stop()
                    window.setTimeout (() -> callback(field)), 0 if callback
                ).hover((() => b.addClass('pointer-cursor') unless @loading),
                        (() -> b.removeClass('pointer-cursor')))
        nr = (p = @gameField.getGamingPiece(pieceID)).getRow() * 8 + p.getCol()
        list = [0..63]
        list.splice(nr, 1)
        @_highlightFields list
        @_highlightFields validFields, true
    
    setStatus: (text) ->
        unless text
            $('#status').hide()
        else
            $('#status').show().text(text)
    
    setGetNickNameActive: (val) -> $('#startButton')[if val then 'show' else 'hide']()
    
    setGameInformation: (info) ->
        info = {} unless info
        updatePoints = () =>
            max = @gameField.getWinPoints()
            $('#player0Points').text("#{@gameField.getPointsForSide(0)} / #{max}")
            $('#player1Points').text("#{@gameField.getPointsForSide(1)} / #{max}")
        
        if info is 'update'
            updatePoints()
        else
            {player0Name, player1Name} = info
            
            unless player0Name or player1Name
                $('#gameInformation').hide()
            else
                (e = $('#gameInformation')).show()
                updatePoints()
                $('#player0Name').text(player0Name) if player0Name
                $('#player1Name').text(player1Name) if player1Name
    
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
    
    _highlightFields: (fields, rings = false) ->
        @_highlightRings ?= {}
        fieldMap = {}
        for i in [0..63]
            fieldMap[i] = off
        for i in fields
            fieldMap[i] = on
        
        size = 2
        for i, status of fieldMap
            row = Math.floor i / 8
            col = i - row * 8
            if rings
                ring = @_highlightRings[i]
                if !ring and status
                    cx = (col + 0.5) * @fieldSize + 1
                    cy = (row + 0.5) * @fieldSize + 1
                    strokeWidth = 3
                    @_highlightRings[i] = ring = @paper.circle(cx, cy, @fieldSize * 0.5 * 0.8)
                    ring.attr 'stroke-width': strokeWidth, 'stroke-opacity': 0, 'opacity': '0.3'
                ring.stop() if ring
                if status
                    ring.animate 'stroke-opacity': 1, 300
                else if ring
                    do (ring, i) =>
                        ring.animate 'stroke-opacity': 0, 300, callback: () =>
                            delete @_highlightRings[i]
                            ring.remove()
            else
                piece = @backgroundPieces[row][col]
            
                attrs = if status
                    (
                        opacity: 1
                        x: (col * @fieldSize + 1)
                        y: (row * @fieldSize + 1)
                        height: @fieldSize
                        width: @fieldSize)
                else
                    (
                        opacity: 0.4
                        x: (col * @fieldSize) + size / 2 + 1
                        y: (row * @fieldSize) + size / 2 + 1
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
                    "50%" : (x: (7 - col) * @fieldSize + 1, rotation: 45)
                    "100%": (y: (7 - row) * @fieldSize + 1, rotation: 0)
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
            balls.push @progressBar.circle((2 * r + a) * (i - 1) + r, r, 0).
                attr(fill: "0-hsb(#{hue}°, .3, 1)-hsb(#{newHue}°, .3, 1)", 'fill-opacity': '50%', stroke: 'none')
            hue = newHue
        balls
    
    _drawBackground: ->
        for rowData, rowNr in UI.fieldRows
            for colorIndex, col in rowData
                rect = @paper.rect(@fieldSize * col + 1, @fieldSize * rowNr + 1, @fieldSize, @fieldSize, 5)
                rect.attr fill: UI.colorMap[colorIndex]
                @backgroundPieces[rowNr][col] = rect
    
    _swapPieces: () ->
        for pieceID, piece of @pieces
            piece.swap()

class UIGamingPiece
    
    bg = ['#1B1B1B', '#EEE']
    
    constructor: (@piece, @field) ->
        @swapped = false
        @row = @piece.getRow()
        @col = @piece.getCol()
        
        fieldSize = @field.getFieldSize()
        @r = fieldSize * 0.5 * 0.8
        @_setCenterPositions()
        
        @set = @_drawPiece()
    
    getPiece: () -> @piece
    
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
    move: (callback) -> # uses post-swap positions (i.e. gfx positions, not the "true" ones)
        fieldSize = @field.getFieldSize()
        paper = @field.paper
        
        oldCol = @col
        oldRow = @row
        
        if @swapped
            @col = 7 - @piece.getCol()
            @row = 7 - @piece.getRow()
        else
            @col = @piece.getCol()
            @row = @piece.getRow()
        
        dx = (@col - oldCol) * fieldSize
        dy = (@row - oldRow) * fieldSize
        
        time = 1000 * Math.sqrt(dx * dx + dy * dy) / 180 # distance / speed
        
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
        
        toFront = (group) ->
            
        (group = $("#piece-#{@piece.getID()}")).parent("svg").append(group)
        for {elem, attr} in animElems
            if withObj
                elem.animateWith withObj, attr, time
            else
                elem.animate attr, time, callback # callback here because it will only be executed once, hopefully
                withObj = elem
    
    updateDragonTeeth: (callback) ->
        @_setCenterPositions()
        
        current = @dragonToothPieces.length
        goal = @piece.getDragonTeeth()
        
        diff = goal - current
        
        if diff is 0 or !(0 <= goal <= 4)
            window.setTimeout (() -> callback()), 0
        else if diff < 0
            for n in [diff...0]
                pieces = @dragonToothPieces.pop()
                for el in pieces
                    el.remove()
            
            time = 1500
            for elems in @dragonToothPieces
                for elem in elems
                    elem.animate({rotation: "#{(-360 + 360 / 64) * (-diff)} #{@cx} #{@cy}"}, time, '<>', callback)
        else
            time = 2000
            
            addCallback = () =>
                time2 = time * 0.2
                
                for j in [1..(Math.abs diff)]
                    @_addDragonTooth(current, goal)
                    attrs2 = {rotation: "#{(360 / 32) * j} #{@cx} #{@cy}", easing: '<>'}
                    defined = false
                    for el in @dragonToothPieces[current + j - 1]
                        if defined
                            el.animate(attrs2, time2)
                        else
                            defined = true
                            el.animate(attrs2, time2, callback)
            if current is 0
                current = goal
                diff = 0
                for n in [1..goal]
                    @_addDragonTooth(n, goal)
            
            attrs = {rotation: "#{360 - (360 / 64) * diff} #{@cx} #{@cy}", easing: '<>'}
            for n in [0...current]
                for el, i in @dragonToothPieces[n]
                    if n is current - 1 && i is 7
                        attrs.callback = if diff is 0 then callback else addCallback
                    el.animate(("80%": attrs), time)
    
    animateBlocked: (callback) ->
        duration = 2000
        offset = 1
        attrs = {}
        for i in [0..24]
            x = i * 4
            attrs[x + 2] = (translation: "#{- 2 * offset}")
            attrs[x + 4] = (translation: "#{2 * offset}")
        attrs[2].translation = "#{-offset}"
        attrs[100].translation = "#{offset}"
        
        jitterElem = (elem, setCallback = false) ->
            attrs[100].callback = callback if setCallback
            elem.animate(attrs, duration)
        
        elems = []
        
        for elem in @set
            elems.push elem
        
        for pieces, a in @dragonToothPieces
            for elem, b in pieces
                elems.push elem
        
        l = elems.length - 1
        for elem, i in elems
            jitterElem elem, (i == l)
        
    
    remove: () ->
        $("#piece-#{@piece.getID()}").remove()
    
    ###
    - private methods
    ###
    
    _setCenterPositions: () ->
        fieldSize = @field.getFieldSize()
        @cx = fieldSize * (@col + 0.5) + 1
        @cy = fieldSize * (@row + 0.5) + 1
    
    _drawPiece: () ->
        fieldSize = @field.getFieldSize()
        paper = @field.paper
        
        # create a new SVG group for this piece
        svgns = "http://www.w3.org/2000/svg"
        group = document.createElementNS(svgns, "g")
        group.id = "piece-" + @piece.getID()
        paper.canvas.appendChild(group)
        
        set = []
        
        set.push paper.circle(@cx + @r / 7, @cy + @r / 7, @r * 1.05).
            attr(fill: 'rblack:80%-rgba(0, 0, 0, 0)', stroke: 'none')
        
        set.push paper.circle(@cx, @cy, @r).
            attr(
                stroke: '#7E7E7E',
                fill: bg[@piece.getSide()],
                'stroke-width': 1)
        
        colorID = @piece.getColorID()
        colorChar = @_fitPathInto(UICharaters[colorID], @cx, @cy, @r)
        set.push colorChar.attr(fill: UI.colorMap[colorID], stroke: 'none')
        
        $(group).append(setItem) for setItem in set
        
        # dragon tooths
        @dragonToothPieces = []
        
        dragonTeeth = @piece.getDragonTeeth()
        if dragonTeeth > 0
            for n in [1..dragonTeeth]
                @_addDragonTooth(n, dragonTeeth)
        
        set
    
    _addDragonTooth: (n, dragonTeeth) ->
        color = bg[1 - @piece.getSide()]
        rr = @r * 0.82
        deltaDeg = 360 / 8
        diffDeg = 360 / (4 * 8)
        rad = Raphael.rad(diffDeg / 2)
        size = Math.sin(rad) * rr * 2
        y = @cy - (size / 2 + Math.cos(rad) * rr)
        path = 'M 4 0L0 6L4 5L8 6z'
        initDegOffset = -(diffDeg / 2 * (dragonTeeth - 1))
        dragonToothPiece = []
        group = $("#piece-#{@piece.getID()}")
        for i in [0..7]
            angle = initDegOffset + i * deltaDeg + (n - 1) * diffDeg
            el = @_fitPathInto(path, @cx, y, size).
                rotate(angle, @cx, @cy).attr(fill: color, stroke: 'none')
            dragonToothPiece.push el
            group.append(el)
        @dragonToothPieces.push dragonToothPiece
    
    _fitPathInto: (path, cx0, cy0, width0, height0 = width0) ->
        
        elem = @field.paper.path(path)
        {width, height} = elem.getBBox()
        
        elem.scale(Math.max(width0, height0) / Math.max(width, height))
        {width, height, x, y} = elem.getBBox()
        elem.translate((cx0 - width / 2) - x, (cy0 - height / 2) - y)

