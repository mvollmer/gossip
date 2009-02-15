SUBDIRS = cdma

exampledir = $(libexecdir)/gossip/sim/example
example_PROGRAMS = source.prim destination.prim adder.prim

INCLUDES = -I $(top_srcdir) $(GUILE_CFLAGS)
LDFLAGS = $(GOSSIP_BLOCK_LDFLAGS)

# We must mention all sources here because automake defaults to .c
# files and not .cc files.

source_prim_SOURCES = source.cc
destination_prim_SOURCES = destination.cc
adder_prim_SOURCES = adder.cc

EXTRA_DIST = example-1.sim example-2.sim example-3.sim example-4.sim
