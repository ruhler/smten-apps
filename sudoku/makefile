
sudoku: Smten/Compiled/Main.hs Smten/Compiled/BitCell.hs Smten/Compiled/Board.hs Smten/Compiled/Cell.hs Smten/Compiled/EnumCell.hs Smten/Compiled/IntegerCell.hs
	ghc --make -main-is Smten.Compiled.Main.main -o $@_prof -osuf o_prof -hisuf hi_prof $< -rtsopts -prof
	ghc --make -main-is Smten.Compiled.Main.main -O -o $@ $<

Smten/Compiled/Main.hs: sudoku.hs BitCell.hs Board.hs Cell.hs EnumCell.hs IntegerCell.hs
	ghc --make -osuf smten_o -prof -c -fplugin=Smten.Plugin.Plugin sudoku.hs -fwarn-unused-binds -fwarn-unused-imports

