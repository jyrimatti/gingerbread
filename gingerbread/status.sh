#!/bin/sh

ps aux | grep -v grep | grep on.sh | wc -l
