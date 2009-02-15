INCLUDES = -I .. -I $(srcdir)/.. $(GTK_PLUS_GUILE_GTK_CFLAGS)

lib_LTLIBRARIES = libgossipcanvas.la
noinst_PROGRAMS = testcanvas

libgossipcanvas_la_SOURCES = gossip-canvas.c \
                             gossip-canvas.h \
                             gossip-art.c \
                             gossip-art.h \
                             gossip-canvas-util.c \
                             gossip-canvas-util.h \
                             gossip-canvas-line.c \
                             gossip-canvas-line.h \
                             gossip-canvas-rect-ellipse.c \
                             gossip-canvas-rect-ellipse.h \
                             gossip-canvas-polygon.c \
                             gossip-canvas-polygon.h \
                             gossip-canvas-text.c \
                             gossip-canvas-text.h \
                             gossip-canvas-grid.c \
                             gossip-canvas-grid.h \
	                     gossip-canvas-glue.c \
	                     gossip-canvas-extra-glue.c

libgossipcanvas_la_LIBADD = $(GUILE_GTK_LIBS)

gossip-canvas-glue.c: canvas.defs
	build-guile-gtk glue $< >tmp && mv tmp $@

testcanvas_SOURCES = testcanvas.c
testcanvas_LDADD = libgossipcanvas.la

BUILT_SOURCES = gossip-canvas-glue.c

EXTRA_DIST = canvas.defs
