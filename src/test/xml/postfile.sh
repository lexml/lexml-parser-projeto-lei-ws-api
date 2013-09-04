#!/bin/sh
. ./defaults.sh
curl -v -F "fonte=@$2" -F "requisicao=@$1" "http://$PARSER_HOST/lexml-parser/parse/parseSenado"
