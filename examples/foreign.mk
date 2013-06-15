include $(TOP)/config.mk
include $(TOP)/examples/check.mk


INCLUDES=   -I$(TOP)
PKGFLAGS=   -package-conf $(TOP)/plugins.conf.inplace -package plugins

# compile with GHC to save us setting all the necessary include and
# lib flags. use ghc -v to find out what these are if you wish to go
# via gcc.
BIN=./a.out
SRC=main.c

BINDIR=		"."
REALBIN=	$(BIN)

all:  $(BIN)

$(BIN): $(SRC)
	@$(GHC) $(INCLUDES) $(PKGFLAGS) $(SRC)

clean:
	rm -rf *.hi *.o *~ $(BIN)
