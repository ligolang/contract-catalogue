ifndef LIGO
LIGO=docker run --rm -v "$(PWD)":"$(PWD)" -w "$(PWD)" ligolang/ligo:next
endif

test:
	$(LIGO) run test ./test/fa2/single_asset.test.mligo
	$(LIGO) run test ./test/fa2/multi_asset.test.mligo
	$(LIGO) run test ./test/fa2/nft/nft.test.mligo
	$(LIGO) run test ./test/fa2/nft/views.test.mligo --protocol hangzhou

test-mutation: 
	$(LIGO) run test ./test/fa2/nft/e2e_mutation.test.mligo

compile:
	$(LIGO) compile contract lib/fa2/single_asset.mligo
