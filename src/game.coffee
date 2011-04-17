$(document).ready ->
    paper = Raphael('game', 600, 600)
    field = new UIField(paper)
    for x in [0..7]
        a = new GamingPiece(7-x, 0, x, 0)
        b = new GamingPiece(x, 7, x, 1)
        if x % 2 == 0
            a.setDragonTooths Math.round(Math.random() * 3) + 1
            b.setDragonTooths Math.round(Math.random() * 3) + 1
        field.addGamingPiece(a)
        field.addGamingPiece(b)
    `f = field`
    $(field.paper.canvas).click ->
        field.swapBackground()