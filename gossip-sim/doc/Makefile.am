info_TEXINFOS = gossip-sim.texi gossip-lib-example.texi

GOSSIP_SIM_PICS_FIG = example-1.fig example-2.fig
GOSSIP_SIM_PICS_TXT = example-1.txt example-2.txt

GOSSIP_SIM_PICS_PDF = example-1.pdf example-2.pdf
GOSSIP_SIM_PICS_EPS = example-1.eps example-2.eps
GOSSIP_SIM_PICS_PNG = example-1.png example-2.png

gossip-sim.dvi: $(GOSSIP_SIM_PICS_EPS)
gossip-sim.pdf: $(GOSSIP_SIM_PICS_PDF)
gossip-sim.html: $(GOSSIP_SIM_PICS_PNG)

%.pdf: %.texi
	texi2dvi --pdf $<

%.html: %.texi
	LANG="" makeinfo --no-split --html $< -o - | $(srcdir)/fix-html >$@

%.index: %.html
	$(srcdir)/extract-index $< file:$(htmldir)/$< >$@

%.eps: %.fig
	fig2dev -Lps $< $@

%.pdf: %.eps
	epstopdf $<

%.png: %.fig
	fig2dev -Lpng -m1.5 $< $@

htmldir = $(datadir)/gossip/doc
html_DATA = gossip-lib-example.html
noinst_DATA = gossip-lib-example.index

exampledir = $(libexecdir)/gossip/sim/example

install-data-local: gossip-lib-example.index
	$(mkinstalldirs) $(exampledir)
	$(INSTALL_DATA) gossip-lib-example.index $(exampledir)/html.index

uninstall-local:
	rm -f $(exampledir)/html.index

EXTRA_DIST = $(GOSSIP_SIM_PICS_FIG) $(GOSSIP_SIM_PICS_EPS) \
             $(GOSSIP_SIM_PICS_PDF) $(GOSSIP_SIM_PICS_TXT) \
             fix-html extract-index

CLEANFILES = gossip-lib-example.index gossip-lib-example.html \
             gossip-sim.tmp gossip-lib-example.bls
