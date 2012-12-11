
testsri: test.sri RegEx.sri
	../seri/build/seri-bin/seri --io \
		--include ../seri/seri/sri \
		--include . \
		--main-is Main.main \
		-f test.sri

test: test.hs RegEx.hs
	ghc -o test --make $<

main: main.hs Grammar.hs Lexer.hs CFG.hs Hampi.hs
	ghc -o main --make $<

Grammar.hs: Grammar.y
	happy $<

clean:
	- rm *.o *.hi Grammar.hs main


