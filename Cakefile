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

fs = require 'fs'
{exec} = require 'child_process'

appFiles = [ # in order of concatenation
    'lib',
    'gameobjects',
    'ui',
    'game'
]

appName = 'maikados'

# almost completely copied from the CoffeeScript wiki
task 'build', 'Build single application file from source files', ->
    appContents = new Array(remaining = appFiles.length)
    for file, index in appFiles then do (file, index) ->
        fs.readFile "src/#{file}.coffee", 'utf8', (err, fileContents) ->
            throw err if err
            appContents[index] = fileContents
            process() if --remaining is 0
    process = ->
        fs.writeFile "#{appName}.coffee", appContents.join('\n\n'), 'utf8', (err) ->
            throw err if err
            exec "coffee --compile #{appName}.coffee", (err, stdout, stderr) ->
                throw err if err
                console.log stdout + stderr
                fs.unlink "#{appName}.coffee", (err) ->
                    throw err if err
                    console.log 'Building done.'

task 'autobuild', 'Watches for file changes and runs the build task', ->
    for file in appFiles
        fs.watchFile "src/#{file}.coffee", interval: 1, (curr, prev) ->
            unless curr.mtime.valueOf() is prev.mtime.valueOf()
                exec 'cake build', (err, stdout, stderr) ->
                    console.log stdout + stderr