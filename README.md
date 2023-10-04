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

Edit the file to add an additional field `administrators`

```ligolang
#import "./lib/fa2/nft/nft.impl.jsligo" "Contract"

export type storage = {
    administrators: set<address>,
    ledger: Contract.NFT.ledger,
    metadata: Contract.TZIP16.metadata,
    token_metadata: Contract.TZIP12.tokenMetadata,
    operators: Contract.NFT.operators
};

type ret = [list<operation>, storage];
```

Importing the library allows you to add TZIP types to your custom storage

Add the TZIP12 default entrypoints, calling the functions of the library and mapping it to your custom storage

```ligolang
@entry
const transfer = (p: Contract.TZIP12.transfer, s: storage): ret => {
    const ret2: [list<operation>, Contract.NFT.storage] =
        Contract.NFT.transfer(
            p,
            {
                ledger: s.ledger,
                metadata: s.metadata,
                token_metadata: s.token_metadata,
                operators: s.operators,
            }
        );
    return [
        ret2[0],
        {
            ...s,
            ledger: ret2[1].ledger,
            metadata: ret2[1].metadata,
            token_metadata: ret2[1].token_metadata,
            operators: ret2[1].operators,
        }
    ]
};

@entry
const balance_of = (p: Contract.TZIP12.balance_of, s: storage): ret => {
    const ret2: [list<operation>, Contract.NFT.storage] =
        Contract.NFT.balance_of(
            p,
            {
                ledger: s.ledger,
                metadata: s.metadata,
                token_metadata: s.token_metadata,
                operators: s.operators,
            }
        );
    return [
        ret2[0],
        {
            ...s,
            ledger: ret2[1].ledger,
            metadata: ret2[1].metadata,
            token_metadata: ret2[1].token_metadata,
            operators: ret2[1].operators
        }
    ]
};

@entry
const update_operators = (p: Contract.TZIP12.update_operators, s: storage): ret => {
    const ret2: [list<operation>, Contract.NFT.storage] =
        Contract.NFT.update_operators(
            p,
            {
                ledger: s.ledger,
                metadata: s.metadata,
                token_metadata: s.token_metadata,
                operators: s.operators
            }
        );
    return [
        ret2[0],
        {
            ...s,
            ledger: ret2[1].ledger,
            metadata: ret2[1].metadata,
            token_metadata: ret2[1].token_metadata,
            operators: ret2[1].operators
        }
    ]
};
```

Continue to add non-TZIP new entrypoints, etc ...

## Implement the interface differently
