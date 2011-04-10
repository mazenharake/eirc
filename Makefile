.PHONY: all clean

BEAMS       := $(patsubst src/%, ebin/%, $(patsubst %.erl, %.beam, $(wildcard src/*.erl)))
ECINCLUDES  := -I include
ECFLAGS     := -pa ebin +debug_info

all: $(BEAMS)

ebin/%.beam: src/%.erl
	@echo "[ERLC]" $<": "$@
	@erlc -o ebin/ $(ECINCLUDES) $(ECFLAGS) $<

clean:
	rm -f ebin/*.beam
