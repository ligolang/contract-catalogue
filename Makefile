SHELL := /bin/bash

ligo_compiler?=docker run --rm -v "$(PWD)":"$(PWD)" -w "$(PWD)" ligolang/ligo:1.2.0
# ^ Override this variable when you run make command by make <COMMAND> ligo_compiler=<LIGO_EXECUTABLE>
# ^ Otherwise use default one (you'll need docker)
PROTOCOL_OPT?=

project_root=--project-root .
# ^ required when using packages

help:
	@grep -E '^[ a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | \
	awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-15s\033[0m %s\n", $$1, $$2}'

compile = $(ligo_compiler) compile contract $(project_root) ./lib/$(1) -o ./compiled/$(2) $(3) $(PROTOCOL_OPT)
# ^ compile contract to michelson or micheline

test = $(ligo_compiler) run test $(project_root) ./test/$(1) $(PROTOCOL_OPT)
# ^ run given test file

compile: ## compile contracts
	@if [ ! -d ./compiled ]; then mkdir -p ./compiled/fa2/nft && mkdir -p ./compiled/fa2/asset ; fi
	@echo "Compiling contracts..."
	@echo "fa2.1/fa2.1-NFT.jsligo"
	@$(call compile,fa2.1/fa2.1-NFT.jsligo,fa2/asset/fa2.1-single-asset.jsligo.json)
	@$(call compile,fa2.1/fa2.1-NFT.jsligo,fa2/asset/fa2.1-single-asset.jsligo.json,--michelson-format json)
	@echo "fa2.1/fa2.1-single-asset.jsligo"
	@$(call compile,fa2.1/fa2.1-single-asset.jsligo,fa2/asset/fa2.1-single-asset.jsligo.json)
	@$(call compile,fa2.1/fa2.1-single-asset.jsligo,fa2/asset/fa2.1-single-asset.jsligo.json,--michelson-format json)
	@echo "fa2.1/fa2.1-multi-asset.jsligo"
	@$(call compile,fa2.1/fa2.1-multi-asset.jsligo,fa2/asset/fa2.1-multi-asset.jsligo.tz)
	@$(call compile,fa2.1/fa2.1-multi-asset.jsligo,fa2/asset/fa2.1-multi-asset.jsligo.json,--michelson-format json)

	@echo "fa2/nft/nft.impl.mligo"
	@$(call compile,fa2/nft/nft.impl.mligo,fa2/nft/nft.impl.mligo.tz)
	@$(call compile,fa2/nft/nft.impl.mligo,fa2/nft/nft.impl.mligo.json,--michelson-format json)
	@echo "fa2/asset/single_asset.impl.mligo"
	@$(call compile,fa2/asset/single_asset.impl.mligo,fa2/asset/single_asset.impl.mligo.tz)
	@$(call compile,fa2/asset/single_asset.impl.mligo,fa2/asset/single_asset.impl.mligo.json,--michelson-format json)
	@echo "fa2/asset/multi_asset.impl.mligo"
	@$(call compile,fa2/asset/multi_asset.impl.mligo,fa2/asset/multi_asset.impl.mligo.tz)
	@$(call compile,fa2/asset/multi_asset.impl.mligo,fa2/asset/multi_asset.impl.mligo.json,--michelson-format json)
	@echo "Compiled contracts!"
clean: ## clean up
	@rm -rf compiled

deploy: deploy_deps deploy.js

deploy.js:
	@echo "Running deploy script\n"
	@cd deploy && npm i && npm start

deploy_deps:
	@echo "Installing deploy script dependencies"
	@cd deploy && npm install
	@echo ""

install: ## install dependencies
	@$(ligo_compiler) install

.PHONY: test
test: ## run tests (SUITE=permit make test)
ifndef SUITE
	@$(call test,fa2/single_asset.test.mligo)
	@$(call test,fa2/single_asset_jsligo.test.mligo)
	@$(call test,fa2/multi_asset.test.mligo)
#	@$(call test,fa2/nft/nft.test.mligo)
	@$(call test,fa2/multi_asset_jsligo.test.mligo)
#	@$(call test,fa2/nft/nft_jsligo.test.mligo)
	@$(call test,fa2/nft/views.test.mligo)

##  @$(call test,fa2/nft/e2e_mutation.test.mligo)
else
	@$(call test,$(SUITE).test.mligo)
endif

lint: ## lint code
	@npx eslint ./scripts --ext .ts
