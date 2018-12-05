#!/bin/sh

ps aux | grep -v grep | grep /home/pi/gingerbread/on.sh | wc -l
