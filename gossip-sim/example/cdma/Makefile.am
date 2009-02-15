noinst_PROGRAMS = random-bits.prim block-ber.prim invert.prim \
                  spread.prim despread.prim \
                  modulate.prim demodulate.prim awgn.prim \
                  add.prim power-control.prim \
                  rmodulate.prim rdemodulate.prim \
                  rspread.prim rdespread.prim \
                  rawgn.prim radd.prim

INCLUDES = -I $(top_srcdir) $(GUILE_CFLAGS)
LDFLAGS = $(GOSSIP_BLOCK_LDFLAGS)

# We must mention all sources here because automake defaults to .c
# files and not .cc files.

random_bits_prim_SOURCES = random-bits.cc
block_ber_prim_SOURCES = block-ber.cc
invert_prim_SOURCES = invert.cc
spread_prim_SOURCES = spread.cc
despread_prim_SOURCES = despread.cc
modulate_prim_SOURCES = modulate.cc
demodulate_prim_SOURCES = demodulate.cc
awgn_prim_SOURCES = awgn.cc
add_prim_SOURCES = add.cc
power_control_prim_SOURCES = power-control.cc
rmodulate_prim_SOURCES = rmodulate.cc
rdemodulate_prim_SOURCES = rdemodulate.cc
rspread_prim_SOURCES = rspread.cc
rdespread_prim_SOURCES = rdespread.cc
rawgn_prim_SOURCES = rawgn.cc
radd_prim_SOURCES = radd.cc

EXTRA_DIST = cdma.sim rcdma.sim
