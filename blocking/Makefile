.PHONY: all
all: touches matches

.PHONY: touches
touches: blocking
	./blocking -t \
	  byu-h-final-2013.touch \
	  byu-h-tam-semi-2013.touch \
	  clarion-11-11.touch \
	  clarion-11-8.touch \
	  concordia-final-2013.touch \
	  wju-semi-2013.touch

.PHONY: matches
matches: blocking
	./blocking \
	  clarion-iup.match \
	  tampa-wju-2014.match \
	  tampa-fortsmith-2014.match \
	  tampa-sw-minn-2014.match \
	  byu_hi_concordia_2013.match \
	  edinboro-clarion-2014.match \
	  cal-clarion-2014.match \
	  hilldale-clarion-2015.match \
	  gannon-indy-2015.match

#penn-stanford-2014.match \
#penn-byu-2014.match \

.PHONY: historic
historic: blocking
	./blocking \
	  hawaii-stanford-1987.match \
	  japan-ussr-1964.match

.PHONY: mens
mens: blocking
	./blocking rus-bra-2012.match

Match.hs: Match.y
	happy Match.y

Touches.hs: Touches.y
	happy Touches.y

blocking: Blocking.hs Match.hs Touches.hs
	ghc --make -W -o blocking Blocking.hs

.PHONY: clean
clean:
	-rm blocking
	-rm *.o *.hi
	-rm Match.hs
	-rm Touches.hs

