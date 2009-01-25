SOURCE_FILES := $(wildcard src/*.erl)


all: ebin

ebin: $(SOURCE_FILES:src/%.erl=ebin/%.beam)

ebin/%.beam: src/%.erl
	@test -d ebin || mkdir ebin
	erlc -W +debug_info -o ebin $<

clean:
	@rm -rf ebin erl_crash.dump

unit: all
	erl -noshell -pa ebin -s percent_unit test -s init stop

bench: all
	erl -noshell -pa ebin -s percent_bench test -s init stop
