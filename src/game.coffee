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

$(document).ready ->
    field = new UIField('game')
    
    rand = (min, max) ->
        Math.round(Math.random() * (max - min)) + min
    dA = rand(0, 7)
    dB = rand(0, 7)
    for x in [0..7]
        a = new GamingPiece(7-x, 0 + rand(0, 3), x, 0)
        b = new GamingPiece(x, 7 - rand(0, 3), x, 1)
        a.setDragonTooths 3 if x is dA
        b.setDragonTooths 2 if x is dB
        
        if b.getID() == "1-7"
            b.setDragonTooths 3
        
        field.addGamingPiece(a)
        field.addGamingPiece(b)
    
    pos = 100
    countDown = () ->
        pos = 100 if pos < 0
        field.setProgressBar pos
        pos -= 1
        window.setTimeout countDown, 500
    countDown()
    
    $(field.paper.canvas).click (event) ->
        field.swap()
