
test: shampi
	tclsh runtests.tcl ./$< > tests.shampi
	tclsh runtests.tcl ./rhampi > tests.rhampi
	diff tests.rhampi tests.shampi

shampi: hampi.hs SeriGen.hs Hampi.hs Grammar.hs Lexer.hs
	ghc -o shampi --make $<

SeriGen.hs: SeriGen.sri RegEx.sri
	../seri/build/seri-bin/seri --haskellf \
		--include ../seri/seri/sri \
		--include . \
		--no-main \
		--mod-name SeriGen \
		-f $< > $@

Grammar.hs: Grammar.y RegEx.hs Hampi.hs Lexer.hs
	happy $<

clean:
	- rm *.o *.hi Grammar.hs shampi SeriGen.hs hampi.dbg tests.shampi tests.rhampi


