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

getBottomEllipsePathStr = (row, col, height, width) =>
    diff = 0.15
    #set = @paper.set()
    rx = width * 0.5 * 0.9
    ry = height * 0.5 * 0.6
    topX = height * (row + 0.49)
    length = height*diff
    
    left = width * (col + (1 - 0.9) * 0.5)
    
    path = "M#{left},#{topX} v#{length} " +
        "a#{rx},#{ry} 0 0 0 #{width * 0.9},0 v-#{length}Z"
    #path.attr(fill: "15-#{color}").attr(strokeAttr)
    return path
