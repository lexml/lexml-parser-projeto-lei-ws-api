#!/bin/sh
. ./defaults.sh
curl -v -F -F "requisicao=@$1" "http://$PARSER_HOST/lexml-parser/parse/parseSenado"
