
nqueens: Smten/Compiled/Main.hs
	ghc --make -main-is Smten.Compiled.Main.main -o $@_prof -osuf o_prof -hisuf hi_prof $< -rtsopts -prof
	ghc --make -main-is Smten.Compiled.Main.main -O -o $@ $<

Smten/Compiled/Main.hs: nqueens.hs
	ghc -osuf smten_o -prof -c -fplugin=Smten.Plugin.Plugin nqueens.hs -fwarn-unused-binds -fwarn-unused-imports

clean:
	rm -rf nqueens nqueens.hi nqueens_prof nqueens.smten_o Smten

