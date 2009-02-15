libname = matlab
version = 0.0

SIMPLE_BLOCKS = matlab

matlab_OBJS = matlab.o libmateng.o

## List all blocks written in Scheme here.  For each block b there
## must be a Scheme source file with name b.block.  They will be
## installed, nothing more.

SCHEME_BLOCKS =

DEFINES = -DMEXDIR=\"$(lib-exec-prefix)/$(libname)\"

include config.status

INCLUDES = $(GUILE_CFLAGS) $(GOSSIP_CFLAGS)
CXXFLAGS = -g

PRIM_BLOCKS  = $(SIMPLE_BLOCKS:%=%.prim)
BLOCK_BLOCKS = $(SCHEME_BLOCKS:%=%.block)

all: $(PRIM_BLOCKS) matlabengine.mexglx

%.o: %.cc
	$(CXX) -c -o $@ $(INCLUDES) $(DEFINES) $(CXXFLAGS) $<

matlab.prim: $(matlab_OBJS)
	$(CXX) -o $@ $(GOSSIP_BLOCK_LDFLAGS) $(matlab_OBJS)

matlabengine.mexglx: matlabengine.c
	mex -c matlabengine.c
	gcc -nostdlib -shared -o matlabengine.mexglx matlabengine.o

matlab.o libmateng.o: libmateng.h

EXTRA_DIST = matlabengine.c matlab.cc libmateng.cc libmateng.h

###

install: $(PRIM_BLOCKS) $(BLOCK_BLOCKS) matlabengine.mexglx
	./mkinstalldirs $(lib-prefix)/$(libname) \
                        $(lib-exec-prefix)/$(libname)
	l='$(PRIM_BLOCKS) matlabengine.mexglx'; for f in $$l; do \
	  rm -f $(lib-exec-prefix)/$(libname)/$$f; \
	  cp -a $$f $(lib-exec-prefix)/$(libname)/$$f; \
	done
	l='$(BLOCK_BLOCKS)'; for f in $$l; do \
	  rm -f $(lib-prefix)/$(libname)/$$f; \
	  cp -a $$f $(lib-prefix)/$(libname)/$$f; \
	done

uninstall:
	rm -f $(lib-prefix)/$(libname)
	rm -f $(lib-exec-prefix)/$(libname)

clean:
	rm -f $(PRIM_BLOCKS) *.o matlabengine.mexglx

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
