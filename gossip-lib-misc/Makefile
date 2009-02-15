## Specify the name of your library

libname = misc

## List all simple blocks here.  A simple block is a block that
## consists of only one C++ source file that has the same name as the
## block plus ".cc" extension.

SIMPLE_BLOCKS = ber b2b awgn random-bits

## List all blocks written in Scheme here.  For each block b there
## must be a Scheme source file with name b.block.  They will be
## installed, nothing more.

SCHEME_BLOCKS = 

###### Configurable section ends here. ######

include config.status

INCLUDES = $(GUILE_CFLAGS) $(GOSSIP_CFLAGS)
CXXFLAGS = -O2 -g

PRIM_BLOCKS  = $(SIMPLE_BLOCKS:%=%.prim)
BLOCK_BLOCKS = $(SCHEME_BLOCKS:%=%.block)

all: $(PRIM_BLOCKS)

%.prim: %.cc
	$(CXX) -o $@ $(GOSSIP_BLOCK_LDFLAGS) $(INCLUDES) $(CXXFLAGS) $<

install: $(PRIM_BLOCKS) $(BLOCK_BLOCKS)
	./mkinstalldirs $(prefix)/share/gossip/sim/$(libname) \
                        $(exec-prefix)/libexec/gossip/sim/$(libname)
	l='$(PRIM_BLOCKS)'; for f in $$l; do \
	  cp -a $$f $(exec-prefix)/libexec/gossip/sim/$(libname)/$$f; \
	done
	l='$(BLOCK_BLOCKS)'; for f in $$l; do \
	  cp -a $$f $(prefix)/share/gossip/sim/$(libname)/$$f; \
	done

clean:
	rm -f $(PRIM_BLOCKS)

distclean: clean
	rm -f config.status

config.status:
	@echo >&2 "You need to run \`configure' as explained in INSTALL before you can build this package."
	@false
