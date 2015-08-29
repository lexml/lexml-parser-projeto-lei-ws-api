#!/bin/bash
./postembutido.sh "$1" &> .tmp
cat .tmp
URL=`grep  'Location:' .tmp | head -n 1 | sed -e 's/.*Location: \(http[^[:space:]]*\).*/\1/g'`
DELAY=${DELAY:-6}
sleep "$DELAY"
echo
echo URL="'$URL'"
./showresult.sh "$URL"
