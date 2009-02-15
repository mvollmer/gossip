# Makefile with useful targets for maintaining the GOSSIP web site
#
# update: Copy all changed files to sourceforge.  [default target]
# reset:  Clear memory so that the next update will copy all files.

WEBFILES = index.html \
           install.html \
           history.html \
	   gossip-small-color.png \
           gossip-midi-color.png \
           gossip-sim.html \
           gossip-libs.html \
           gossip-ed.html gossip-ed-shot.png \
           gossip-vhdl.html \
           docs

update:
	test -e update.stamp || touch -t 010100001970 update.stamp
	LANG=""; tar -N "`date -R -r update.stamp`" -cf - $(WEBFILES) | \
	ssh gossip.sourceforge.net tar -C /home/groups/gossip/htdocs -xvvf -
	touch update.stamp

reset:
	rm -f update.stamp
