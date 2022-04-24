hpack:
	hpack .

dev: hpack
	ghcid --command 'cabal new-repl lib:investments-tracking' --allow-eval --warnings

test: hpack
	ghcid --command='cabal v2-repl investments-tracking-test' --test 'Main.main'
