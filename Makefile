ifndef LIGO
LIGO=docker run --rm -v "$(PWD)":"$(PWD)" -w "$(PWD)" ligolang/ligo:next
endif

test:
	$(LIGO) run test ./tests/FA2_single_asset.test.mligo
	$(LIGO) run test ./tests/FA2_multi_asset.test.mligo
	$(LIGO) run test ./tests/FA2_nft.test.mligo

	$(LIGO) run test contract/test_FA2_single_asset.mligo

compile:
	$(LIGO) compile contract contract/FA2_single_asset.mligo

check:
	$(LIGO) run test contract/foo.mligo
