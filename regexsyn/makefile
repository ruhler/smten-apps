
all: build/release/resyn

build/release/Grammar.hs: src/Grammar.y
	happy src/Grammar.y -o build/release/Grammar.hs

build/release/resyn: src/resyn.hs src/RegEx.hs build/release/Grammar.hs src/Lexer.hs
	mkdir -p build/release
	smten --make -o $@ -ibuild/release -isrc -hidir build/release -odir build/release $< -rtsopts -fwarn-unused-imports

clean:
	rm -rf build
