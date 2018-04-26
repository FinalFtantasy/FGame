#!/usr/bin/env bash
if [ -z "$1" ];then
  echo "Usage: `basename $0` NODE_NAME"
  exit 1
fi

NAME="$1"

port=`epmd -names | awk -v name=$NAME '$2==name {print $5}'`
if [ -z "$port" ];then
  	echo "ERROR: Node name not found: $NAME"
  	exit 1
else
	pid=`lsof -i TCP:$port -s TCP:LISTEN | tail -n +2 | awk '{print $2}'`
	kill $pid
	exit 0
fi