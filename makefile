
shampi: hampi.hs SeriGen.hs Hampi.hs Grammar.hs
	ghc -o shampi --make $<

SeriGen.hs: SeriGen.sri RegEx.sri
	../seri/build/seri-bin/seri --haskellf \
		--include ../seri/seri/sri \
		--include . \
		--no-main \
		--mod-name SeriGen \
		-f $< > $@

Grammar.hs: Grammar.y RegEx.hs Hampi.hs
	happy $<

clean:
	- rm *.o *.hi Grammar.hs shampi SeriGen.hs


