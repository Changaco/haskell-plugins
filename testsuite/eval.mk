include $(TOP)/config.mk
include $(TOP)/testsuite/check.mk

BIN=Main
SRC=Main.hs

BINDIR=.
REALBIN=./$(BIN)

GHCFLAGS= -O0 -cpp

.SUFFIXES : .o .hs .hi .lhs .hc .s

all: $(BIN)

$(BIN): $(SRC) $(OBJS)
	@rm -f $@
	@$(GHC) --make $(GHCFLAGS) $(PKGFLAGS) $(EXTRAFLAGS) $(SRC)

# Standard suffix rules
.o.hi:
	@:
.hs.o:
	@$(GHC) $(INCLUDES) $(PKGFLAGS) $(GHCFLAGS) $(EXTRAFLAGS) -c $<

clean:
	@rm -rf *.hi *.o *~ $(BIN)
