
test: test.hs SeriGen.hs
	ghc -o test --make $<

testsri: test.sri RegEx.sri
	../seri/build/seri-bin/seri --io \
		--include ../seri/seri/sri \
		--include . \
		--main-is Main.main \
		-f test.sri

main: main.hs Grammar.hs Lexer.hs CFG.hs Hampi.hs
	ghc -o main --make $<

SeriGen.hs: SeriGen.sri RegEx.sri
	../seri/build/seri-bin/seri --haskellf \
		--include ../seri/seri/sri \
		--include . \
		--no-main \
		--mod-name SeriGen \
		-f $< > $@

Grammar.hs: Grammar.y
	happy $<

clean:
	- rm *.o *.hi Grammar.hs main


