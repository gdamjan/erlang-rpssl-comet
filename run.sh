#! /bin/sh
DIR=`dirname $0`
export ERL_LIBS="$DIR:$DIR/deps/mochiweb"

exec erl +Bd +K true -noinput -sasl errlog_type error \
     -s rpssl_app $@
