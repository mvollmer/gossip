## Specify the name of your library

libname = ipl

## Specify the version of your library.  This is used to name the
## distribution tarball.

version = 0.0

## List all simple blocks here.  A simple block is a block that
## consists of only one C++ source file that has the same name as the
## block plus ".cc" extension.

SIMPLE_BLOCKS = rand-uniform \
                double-to-complex \
                complex-real \
                complex-imag \
                scheme-exec

IPL_SOURCES = rng-double.c

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

IPL_LIBOBJS = $(IPL_SOURCES:%.c=%.o)

all: $(PRIM_BLOCKS)

%.prim: %.cc ipl.a
	$(CXX) -o $@ $(GOSSIP_BLOCK_LDFLAGS) $(INCLUDES) $(CXXFLAGS) $< ipl.a

ipl.a: $(IPL_LIBOBJS)
	ar rc ipl.a $(IPL_LIBOBJS)

install: $(PRIM_BLOCKS) $(BLOCK_BLOCKS)
	./mkinstalldirs $(lib-prefix)/$(libname) \
                        $(lib-exec-prefix)/$(libname)
	l='$(PRIM_BLOCKS)'; for f in $$l; do \
	  cp -a $$f $(lib-exec-prefix)/$(libname)/$$f; \
	done
	l='$(BLOCK_BLOCKS)'; for f in $$l; do \
	  cp -a $$f $(lib-prefix)/$(libname)/$$f; \
	done

uninstall:
	rm -f $(lib-prefix)/$(libname)
	rm -f $(lib-exec-prefix)/$(libname)

clean:
	rm -f $(PRIM_BLOCKS)

distclean: clean
	rm -f config.status

config.status:
	@echo >&2 "You need to run \`configure' as explained in INSTALL before you can build this package."
	@false

DISTFILES = README INSTALL Makefile configure mkinstalldirs \
            $(EXTRA_DIST) \
            $(SIMPLE_BLOCKS:%=%.cc) $(BLOCK_BLOCKS)

distdir=gossip-lib-$(libname)-$(version)

dist:
	rm -rf $(distdir); \
	mkdir $(distdir); \
	for f in $(DISTFILES); do \
	  ln $$f $(distdir)/$$f; \
	done; \
	tar czf $(distdir).tar.gz $(distdir); \
	rm -rf $(distdir)
