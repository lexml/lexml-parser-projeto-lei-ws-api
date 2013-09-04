#!/bin/sh
wget -O - "$1" | xmllint --format -
