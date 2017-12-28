AS = as
HC = ghc

HFLAGS = -O2

BIN = bin/
ENVIRONMENT = bin/environment.o
COMPILER = bin/speedfuck

.PHONY: all
all: $(ENVIRONMENT) $(COMPILER)

$(BIN):
	mkdir bin

$(ENVIRONMENT): $(BIN)
	$(AS) src/environment.S -o $(BIN)/environment.o

$(COMPILER): $(BIN)
	$(HC) $(HFLAGS) -outputdir bin -isrc -o $(COMPILER) src/main.hs

.PHONY: clean
clean:
	rm -rf $(BIN)
