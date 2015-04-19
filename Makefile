.PHONY: all clean score_gen

all: score.pdf

%.pdf: %.ly
	${HOME}/Applications/LilyPond.app/Contents/Resources/bin/lilypond $^

%.ly: score_gen
	./score_gen 17 > $@

score_gen:
	ghc --make score_gen.hs

clean:
	-rm score.pdf score.ly
	-rm score_gen
	-find . -iname '*.o' -or -iname '*.hi' | xargs rm
