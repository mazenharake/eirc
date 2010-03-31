# Make file for eirc v0.0.1

.PHONY: all clean

BEAMS       := $(patsubst src/%, ebin/%, $(patsubst %.erl, %.beam, $(wildcard src/*.erl)))
ECINCLUDES  := -I include
ECFLAGS     := +debug_info +strict_record_tests +netload

all: $(BEAMS)

ebin/%.beam: src/%.erl
	@echo "[ERLC]" $<": "$@
	@erlc -o ebin/ $(ECINCLUDES) $(ECFLAGS) $<

clean:
	rm -f ebin/*.beam
