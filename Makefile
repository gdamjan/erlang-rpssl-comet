.SUFFIXES: .erl .beam .yrl

ERL_SRC := $(wildcard src/*.erl)
ERL_OBJ := $(patsubst src/%.erl,ebin/%.beam,${ERL_SRC})

all: main

ebin:
	@mkdir -p ebin

main: ebin ${ERL_OBJ}

ebin/%.beam: src/%.erl
	erlc -o `dirname $@` $<


clean:
	rm -rf ebin/
	rm -f erl_crash.dump


