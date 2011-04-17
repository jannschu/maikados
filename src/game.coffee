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