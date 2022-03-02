ifndef LIGO
LIGO=docker run --rm -v "$(PWD)":"$(PWD)" -w "$(PWD)" ligolang/ligo:next
endif

json=--michelson-format json
tsc=npx tsc

.PHONY: test
test:
	$(LIGO) run test ./test/fa2/single_asset.test.mligo
	$(LIGO) run test ./test/fa2/multi_asset.test.mligo
	$(LIGO) run test ./test/fa2/nft/nft.test.mligo
	$(LIGO) run test ./test/fa2/nft/views.test.mligo --protocol hangzhou

test-mutation: 
	$(LIGO) run test ./test/fa2/nft/e2e_mutation.test.mligo

compile:
	$(LIGO) compile contract lib/fa2/nft/NFT.mligo > compiled/fa2/nft/NFT_mligo.tz
	$(LIGO) compile contract lib/fa2/nft/NFT.mligo $(json) > compiled/fa2/nft/NFT_mligo.json


deploy: 
	cd deploy/fa2/nft && $(tsc) deploy.ts --esModuleInterop --resolveJsonModule && node deploy.js
