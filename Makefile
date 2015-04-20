.PHONY: all clean score_gen

all: score17.pdf score13.pdf
	open $^

%.pdf: %.ly
	${HOME}/Applications/LilyPond.app/Contents/Resources/bin/lilypond $^

score17.ly: score_gen
	./score_gen 17 20 $@

score13.ly: score_gen
	./score_gen 13 20 $@

score_gen:
	ghc --make score_gen.hs

clean:
	-rm *.pdf *.ly
	-rm score_gen
	-find . -iname '*.o' -or -iname '*.hi' | xargs rm
