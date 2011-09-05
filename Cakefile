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
    'ui-characters',
    'ui',
    'net',
    'ai',
    'game'
]

appName = 'maikados'

String::trim = () ->
    this.replace(/^\s*/, '').replace(/\s*$/, '')

# almost completely copied from the CoffeeScript wiki
task 'build', 'Build single application file from source files', ->
    appContents = new Array(remaining = appFiles.length)
    for file, index in appFiles then do (file, index) ->
        fs.readFile "src/#{file}.coffee", 'utf8', (err, fileContents) ->
            throw err if err
            appContents[index] = fileContents
            process() if --remaining is 0
    process = ->
        fs.writeFile "resources/#{appName}.coffee", appContents.join('\n\n'), 'utf8', (err) ->
            throw err if err
            exec "coffee --compile resources/#{appName}.coffee", (err, stdout, stderr) ->
                throw err if err
                console.log (stdout + stderr).trim() if stdout or stderr
                fs.unlink "resources/#{appName}.coffee", (err) ->
                    throw err if err
                    getTime = () ->
                        date = new Date()
                        fill = (m) -> if (s = String(date['get' + m]())).length is 1 then "0"+ s else s
                        "#{fill 'Hours'}:#{fill 'Minutes'}:#{fill 'Seconds'}"
                    console.log "\033[34m[#{getTime()}]\033[00m \033[1mBuild done\033[00m"

task 'autobuild', 'Watches for file changes and runs the build task', ->
    build = () ->
        exec 'cake build', (err, stdout, stderr) ->
            console.log (stdout + stderr).trim() if stdout or stderr
            console.log() if stderr
    build()
    for file in appFiles
        fs.watchFile "src/#{file}.coffee", interval: 1, (curr, prev) ->
            unless curr.mtime.valueOf() is prev.mtime.valueOf()
                build()

task 'uglify', 'Compresses JavaScript using UglifyJS', ->
    exec 'uglifyjs -mt --lift-vars --unsafe -o resources/maikados.js resources/maikados.js'
