MLTON=mlton
BIN=bin
SOURCES=src/parser.sml \
	src/main.sml \
	src/version.sml \
	src/plan.sml \
	src/watch.sml \
	src/elaborate.sml \
	src/mlton.sml \
	src/smlnj.sml \
	src/polyml.sml \
	src/moscowml.sml \
	src/compiler.sml \
	src/fsutil.sml \
	src/smackage.sml \
	src/config.sml

all: mlton

mlton: $(BIN)/smbt

$(BIN)/smbt: src/smbt.mlb $(SOURCES)
	$(MLTON) -output $(BIN)/smbt src/smbt.mlb

install: $(BIN)/smbt
	@if [ ! -n "$(DESTDIR)" ]; then echo "Error: DESTDIR is not set."; false; fi
	rm -f $(DESTDIR)/bin/smbt
	mkdir -p $(DESTDIR)/bin
	cp $(BIN)/smbt $(DESTDIR)/bin/smbt

clean:
	rm -f $(BIN)/smbt

.PHONY: clean

