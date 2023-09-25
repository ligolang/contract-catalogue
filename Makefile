SHELL := /bin/bash

ligo_compiler?=docker run --rm -v "$(PWD)":"$(PWD)" -w "$(PWD)" ligolang/ligo:0.57.0
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
	@if [ ! -d ./compiled ]; then mkdir ./compiled ; fi
	@echo "Compiling contracts..."
	@$(call compile,fa2/nft/NFT.mligo,fa2/nft/NFT_mligo.tz)
	@$(call compile,fa2/nft/NFT.mligo,fa2/nft/NFT_mligo.json,--michelson-format json)
	@echo "Compiled contracts!"
clean: ## clean up
	@rm -rf compiled

deploy: deploy_deps deploy.js

deploy.js:
	@if [ ! -f ./deploy/metadata.json ]; then cp deploy/metadata.json.dist deploy/metadata.json ; fi
	@echo "Running deploy script\n"
	@cd deploy && npm start

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
	@$(call test,fa2/nft/nft.test.mligo)
	@$(call test,fa2/nft/views.test.mligo)
	@$(call test,fa2/multi_asset_jsligo.test.mligo)
	@$(call test,fa2/nft/nft_jsligo.test.mligo)
	@$(call test,generic-fa2/single_asset.test.mligo)
	@$(call test,generic-fa2/single_asset.extended.test.mligo)
	@$(call test,generic-fa2/multi_asset.test.mligo)
##  @$(call test,fa2/nft/e2e_mutation.test.mligo)
else
	@$(call test,$(SUITE).test.mligo)
endif

lint: ## lint code
	@npx eslint ./scripts --ext .ts

sandbox-start: ## start sandbox
	@./scripts/run-sandbox

sandbox-stop: ## stop sandbox
	@docker stop sandbox