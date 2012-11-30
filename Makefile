MLTON=mlton
BIN=bin
SOURCES=src/parser.sml \
	src/main.sml \
	src/version.sml \
	src/plan.sml \
	src/watch.sml \
	src/config.sml

all: mlton

mlton: $(BIN)/smbt

$(BIN)/smbt: src/smbt.mlb $(SOURCES)
	$(MLTON) -output $(BIN)/smbt src/smbt.mlb

clean:
	rm -f $(BIN)/smbt

.PHONY: clean

