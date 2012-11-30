MLTON=mlton
BIN=bin
SOURCES=src/parser.sml

all: mlton

mlton: $(BIN)/smbt

$(BIN)/smbt: src/smbt.mlb $(SOURCES)
	$(MLTON) -output $(BIN)/smbt src/smbt.mlb

clean:
	rm -f $(BIN)/smbt

.PHONY: clean

