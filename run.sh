#! /bin/sh

erl +Bd +K true -noinput -sasl errlog_type error \
     -pa ebin/ deps/mochiweb/ebin/ \
     -s rpssl_app -port 8000
