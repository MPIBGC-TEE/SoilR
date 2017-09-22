#!/usr/bin/env bash
# inspired by : http://www.whiteboardcoder.com/2015/01/tmux-scripting.html
tmux new -s tide -d
tmux split-window -h -t tide
tmux select-pane -t 0
tmux send-keys 'vim ' C-m

tmux select-pane -t 1
#tmux send-keys 'R' C-m
tmux send-keys 'cd ../pkg/inst/tests' C-m
tmux split-window -h -t tide


tmux select-pane -t 2
tmux send-keys "${venv}" C-m

tmux attach -t tide

