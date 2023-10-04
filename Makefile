hs:
	cd hs && cabal run

a:
	cd hs && cabal run apribot -- -b

b:
	cd web && npm run dev

c:
	fly proxy 5432 -a apripsql
