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
    Multi Asset Token where there are several token ids (available in different amounts)
    and they can belong to multiple addresses (m:n)

## Use the implementation directly

The library provides you 3 template implementations ready to deploy

1. To install this package, run `ligo install @ligo/fa`. It will download the files
1. Deploy the NFT contract with Taquito the Ghostnet with `alice` wallet

```bash
make compile
make deploy
```

## Extend an implementation

If you need additional features in your contract, you can use the extendable version. An example is
available in the file `examples/mintable.mligo`. Using the extension mechanism, it adds an admin
address to the storage, as well as a `mint` entrypoint to mint owner-less NFTs. Only the admin can
call this entrypoint.

Install the library and create a new file

```bash
ligo install @ligo/fa
touch mintable.mligo
```

To extend the storage, define the type of the extension and refer to the original storage type as
such:

```ocaml
#import "@ligo/fa/lib/main.mligo" "FA2"

module NFT = FA2.NFTExtendable

type extension = {
  admin: address
}

type storage = extension NFT.storage
type ret = operation list * storage
```

Importing the library allows you to refer to the TZIP12 operations signatures and make it easier to
redefine all the entrypoints and views that are required:

```ocaml
(* Standard FA2 interface, copied from the source *)

[@entry]
let transfer (t: NFT.TZIP12.transfer) (s: storage) : ret =
  NFT.transfer t s

[@entry]
let balance_of (b: NFT.TZIP12.balance_of) (s: storage) : ret =
  NFT.balance_of b s

(* Etc. *)
```

To make it easier to define new entrypoints, some functions are available in the library, and you
can also use the `storage` fields directly:

```ocaml
(* Extension *)

type mint = {
   owner    : address;
   token_id : nat;
}

[@entry]
let mint (mint : mint) (s : storage): ret =
  let sender = Tezos.get_sender () in
  let () = assert (sender = s.extension.admin) in
  let () = NFT.Assertions.assert_token_exist s.token_metadata mint.token_id in
  (* Check that nobody owns the token already *)
  let () = assert (Option.is_none (Big_map.find_opt mint.token_id s.ledger)) in
  let s = NFT.set_balance s mint.owner mint.token_id in
  [], s
```

Note that this version requires the minted NFTs to be already defined in the `token_metadata` big
map. However, you can also change the `mint` entrypoint to create new tokens dynamically.

## Implement the interface differently

If you are not happy with the default NFT implementation, you can define your own

Create a new file

```bash
touch myTzip12NFTImplementation.jsligo
```

Import some code and define implementation of missing types `ledger` and `operators`

```ligolang
#import "@ligo/fa/lib/fa2/common/errors.mligo" "Errors"

#import "@ligo/fa/lib/fa2/common/assertions.jsligo" "Assertions"

#import "@ligo/fa/lib/fa2/common/tzip12.datatypes.jsligo" "TZIP12"

#import "@ligo/fa/lib/fa2/common/tzip12.interfaces.jsligo" "TZIP12Interface"

#import "@ligo/fa/lib/fa2/common/tzip16.datatypes.jsligo" "TZIP16"

export namespace NFT implements TZIP12Interface.FA2{
    export type ledger = big_map<nat, address>;
    type operator = address;
    export type operators = big_map<[address, operator], set<nat>>;
    export type storage = {
        ledger: ledger,
        operators: operators,
        token_metadata: TZIP12.tokenMetadata,
        metadata: TZIP16.metadata
    };
    type ret = [list<operation>, storage];

}
```

Copy the missing entrypoints from the TZIP12 interface and give your own implementation

```ligolang
  @entry
    const transfer = (p: TZIP12.transfer, s: storage): ret => {
        failwith("TODO");
    };
    @entry
    const balance_of = (p: TZIP12.balance_of, s: storage): ret => {
        failwith("TODO");
    };
    @entry
    const update_operators = (p: TZIP12.update_operators, s: storage): ret => {
        failwith("TODO");
    };
```

Compile it (do not forget to add the parameter -m NFT as you have to define a namespace to be able to implement an interface)

```bash
ligo compile contract myTzip12NFTImplementation.jsligo -m NFT
```
