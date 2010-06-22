#! /bin/sh
DIR=$(readlink -f $(dirname $0))
export ERL_LIBS="$DIR:$DIR/deps/mochiweb"
cd $DIR

exec erl +Bd +K true -noinput -sasl errlog_type error \
     -s rpssl $@
