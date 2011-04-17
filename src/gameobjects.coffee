class GamingPiece
    
    constructor: (@colorID, @row, @col, @side) ->
        @id = "#{@side}-#{@colorID}"
        @dragonTooths = 0
    
    getColorID: () ->
        @colorID
    
    getID: () ->
        @id
    
    getDragonTooths: () ->
        @dragonTooths
    
    setDragonTooths: (value) ->
        @dragonTooths = value if 0 <= value <= 4
    
    # 0 to 7
    getRow: () ->
        @row
    
    # 0 to 7
    getCol: () ->
        @col
    
    # 0 (black) or 1 (white)
    getSide: () ->
        @side