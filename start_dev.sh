#!/bin/sh
cd `dirname $0`
./rebar compile
exec erl -pa apps/*/ebin -boot start_sasl -config files/app.config -s reloader -s bracket -tempile root '"files/site/templates"' -bracket dispatch '"files/dispatch.conf"'