
test: test.hs RegEx.hs
	ghc -o test --make $<

main: main.hs Grammar.hs Lexer.hs CFG.hs Hampi.hs
	ghc -o main --make $<

Grammar.hs: Grammar.y
	happy $<

clean:
	- rm *.o *.hi Grammar.hs main


