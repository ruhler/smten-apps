
test: shampi
	tclsh runtests.tcl ./$< > tests.shampi
	tclsh runtests.tcl ./rhampi > tests.rhampi
	diff tests.rhampi tests.shampi

bench: shampi
	tclsh runbench.tcl ./shampi > bench.shampi
	#./rhampi_s
	#tclsh runbench.tcl ./rhampi_c > bench.rhampi
	#./rhampi_shutdown

shampi: hampi.hs SeriGen.hs Hampi.hs Grammar.hs Lexer.hs Map.hs
	ghc -o shampi -O2 --make $<

prof: shampi
	ghc -o shampi_prof hampi.hs -prof -fprof-auto-top -rtsopts -osuf o_prof

SeriGen.hs: SeriGen.sri SeriRegEx.sri SeriCFG.sri Fix.sri Map.sri Match.sri
	../seri/build/seri-bin/seri --haskellf \
		--include ../seri/seri/sri \
		--include . \
		--no-main \
		--mod-name SeriGen \
		-f $< > $@

Grammar.hs: Grammar.y Hampi.hs Lexer.hs Elem.hs SeriGen.hs SeriCFG.hs
	happy $<

clean:
	- rm *.o *.o_prof *.hi Grammar.hs shampi SeriGen.hs tests.shampi tests.rhampi tests/*.dbg


