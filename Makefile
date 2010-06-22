.SUFFIXES: .erl .beam .yrl

ERL_SRC := $(wildcard src/*.erl)
ERL_OBJ := $(patsubst src/%.erl,ebin/%.beam,${ERL_SRC})

all: main mochiweb ebin/rpssl.app
main: ebin/ ${ERL_OBJ}

ebin/:
	@mkdir -p ebin

ebin/%.app: src/%.app.src
	cp $< $@

ebin/%.beam: src/%.erl
	erlc -o `dirname $@` $<

mochiweb:
	(cd deps/mochiweb && make)

clean:
	rm -rf ebin/
	rm -f erl_crash.dump


