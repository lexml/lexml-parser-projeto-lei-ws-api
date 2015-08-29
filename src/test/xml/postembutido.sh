#!/bin/sh
. ./defaults.sh
curl -v -H "Content-type: text/xml" \
	--data-binary "@$1" \
	"http://$PARSER_HOST/lexml-parser/parse/parseSenado"
