CFLAGS=-ansi -pedantic -O2
GHCFLAGS=-Wall -XNoCPP -fno-warn-name-shadowing -XHaskell98 -O2
HLINTFLAGS=-XHaskell98 -XNoCPP -i 'Use camelCase' -i 'Use String' -i 'Use head' -i 'Use string literal' -i 'Use list comprehension' --utf8
VERSION=0.1

.PHONY: all shell clean doc install

all: dist/build/libHSgit-date-$(VERSION).a dist/git-date-$(VERSION).tar.gz

install: dist/build/libHSgit-date-$(VERSION).a
	cabal install

shell:
	ghci $(GHCFLAGS)

report.html: Data/Time/Git.hs
	-hlint $(HLINTFLAGS) --report $^

doc: dist/doc/html/git-date/index.html README

README: git-date.cabal
	tail -n+$$(( `grep -n ^description: $^ | head -n1 | cut -d: -f1` + 1 )) $^ > .$@
	head -n+$$(( `grep -n ^$$ .$@ | head -n1 | cut -d: -f1` - 1 )) .$@ > $@
	-printf ',s/        //g\n,s/^.$$//g\n,s/\\\\\\//\\//g\nw\nq\n' | ed $@
	$(RM) .$@

dist/doc/html/git-date/index.html: dist/setup-config Data/Time/Git.hs
	cabal haddock --hyperlink-source

dist/setup-config: git-date.cabal
	cabal configure

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)

dist/build/libHSgit-date-$(VERSION).a: Data/Time/Git.hs ext/date.c dist/setup-config
	cabal build --ghc-options="$(GHCFLAGS)" --gcc-options="$(CFLAGS)"

dist/git-date-$(VERSION).tar.gz: Data/Time/Git.hs ext/date.c README dist/setup-config
	cabal check
	cabal sdist
