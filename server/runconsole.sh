#!/usr/bin/env sh
echo "  \033[34mRun following commands to start the app (manual boot script):\033[00m"
echo "> \033[1mapplication:start(sasl). application:start(misultin). application:start(socketio). application:start(maikados).\033[00m";
echo
erl -pa ebin deps/socketio/ebin/ deps/socketio/deps/ossp_uuid/ebin/ deps/socketio/deps/misultin/ebin/ deps/socketio/deps/jsx/ebin