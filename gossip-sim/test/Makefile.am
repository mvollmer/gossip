testdir = $(libexecdir)/gossip/sim/test
test_PROGRAMS = source.prim thru.prim dest.prim fork.prim
test_DATA = hier-1.block hier-2.block

INCLUDES = -I $(top_srcdir) $(GUILE_CFLAGS)
LDFLAGS = $(GOSSIP_BLOCK_LDFLAGS)

# We must mention all sources here because automake defaults to .c
# files and not .cc files.

source_prim_SOURCES = source.cc
thru_prim_SOURCES = thru.cc
dest_prim_SOURCES = dest.cc
fork_prim_SOURCES = fork.cc

EXTRA_DIST = hier-1.block hier-2.block
