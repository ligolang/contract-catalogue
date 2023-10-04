# FA2 tokens

`ligo/fa` library provides :

- the [interface](./lib/fa2/common/tzip12.interfaces.jsligo) and [types](./lib/fa2/common/tzip12.datatypes.jsligo) defined by [FA2 (TZIP-12)](https://tzip.tezosagora.org/proposal/tzip-12/)
- a [LIGO](https://ligolang.org/) implementation for :
  - unique [NFTs](./lib/fa2/nft/nft.impl.jsligo): This contract implements the FA2 interface for
    NFT(non-fungible-token) where a token can belong to only one address at a time
    (1:1)
  - [Single Assets](./lib/fa2/asset/single_asset.impl.mligo): This is an implementation of
    Single Asset Token where a different amount of single token can belong to multiple
    addresses at a time (1:n)
  - [Multiple Assets](./lib/fa2/asset/multi_asset.impl.mligo): This is an implementation of
    Multi Asset Token where there are many tokens (available in different amounts)
    and they can belong to multiple addresses (m:n)

## Use the implementation directly

The library provides you 3 template implementations ready to deploy

1. To install this package, run `ligo install @ligo/fa`. It will download the files
1. Deploy the NFT contract with Taquito the Ghostnet with `alice` wallet

```bash
make compile
make deploy
```

## Extend the implementation

If you want to build a dapp that is more than just a FA2 contract, like an NFT marketplace, you can extend the base code

Install the library and create a new file

```bash
ligo install @ligo/fa
touch marketplace.jsligo
```

Edit the file

```ligolang
#import "@ligo/fa/lib/fa2/nft/nft.impl.jsligo" "NFT"

type storage = {
  administrators: set<address>,
  ledger: NFT.ledger,
  metadata: NFT.TZIP16.metadata,
  token_metadata: NFT.TZIP12.tokenMetadata,
  operators: NFT.operators
};

```

## Implement the interface differently
