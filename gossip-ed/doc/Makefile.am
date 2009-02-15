all: mockup-tutorial.html

PICS_PNG = mockup-0.png mockup-2.png mockup-4.png mockup-6.png \
           mockup-8.png mockup-1.png mockup-3.png mockup-5.png \
           mockup-7.png mockup-9.png

mockup-tutorial.html: mockup-tutorial.texi $(PICS_PNG)
	cd $(srcdir); \
        LANG="" makeinfo --html mockup-tutorial.texi -o - | ./fix-html >$@

EXTRA_DIST = mockup-tutorial.texi mockup-tutorial.html $(PICS_PNG)
