#!/bin/bash

i3status | while :
do
    read line
    echo "`awk '/MemTotal/ {memtotal=$2}; /MemAvailable/ {memavail=$2}; END { printf("%f", (memtotal - memavail) / (1024 * 1024)) }' /proc/meminfo` | $line" || exit 1
done
