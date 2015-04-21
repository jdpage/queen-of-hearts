.PHONY: all clean scores score_gen

all: scores

scores: score_gen chains.py corpus.txt
	-rm score*.txt
	./score_gen 19 30 | python3 chains.py corpus.txt

score_gen:
	ghc --make score_gen.hs

clean:
	-rm score*.txt
	-rm score_gen
	-find . -iname '*.o' -or -iname '*.hi' | xargs rm
