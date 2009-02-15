lib_LTLIBRARIES = libguilegossip.la

INCLUDES = -I $(top_srcdir) -I. $(GUILE_CFLAGS)

libguilegossip_la_SOURCES = init.cc		\
                            signal-pipe.cc      \
                            signal-pipe.h       \
	                    dynblock.cc		\
                            dynblock.h		\
                            dyncomp.cc		\
                            dyncomp.h		\
                            dynsim.cc		\
                            dynsim.h		\
	                    dynlink.cc          \
                            dynlink.h           \
                            comp.cc		\
                            generics.cc         \
                            prototype.cc

libguilegossip_la_LDFLAGS = -export-dynamic
libguilegossip_la_LIBADD = -lstdc++

XFILES = dynblock.x dyncomp.x dynsim.x signal-pipe.x

BUILT_SOURCES = $(XFILES)

SUFFIXES = .x

.c.x:
	guile-snarf -o $@ $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $<

.cc.x:
	guile-snarf -o $@ $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CXXFLAGS) $<

EXTRA_DIST = $(XFILES)
