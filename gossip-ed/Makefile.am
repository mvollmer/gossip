SUBDIRS = libgossipcanvas doc

gossipdir = $(datadir)/guile/gossip
gossip_DATA = canvas.scm

bin_SCRIPTS = gossip-ed-mockup

EXTRA_DIST = gossip-ed-mockup canvas.scm canonicalize-prefixes

snap:
	$(MAKE) dist distdir=$(PACKAGE)-`date +%Y%m%d`
