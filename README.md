# LIGO-FA (tokens)

`ligo-fa` is a collection of [FA2 (TZIP-12)](https://tzip.tezosagora.org/proposal/tzip-12/)
implementations written in [LIGO](https://ligolang.org/)

It provides 3 types of asset contracts:
1. [NFT](./lib/fa2/nft/NFT.jsligo): This contract implements the FA2 interface for
   NFT(non-fungible-token) where a token can belong to only one address at a time
   (1:1)
2. [Single Asset]()./lib/fa2/asset/single_asset.mligo): This is an implementation of 
   Single Asset Token where a different amount of single token can belong to multiple
   addresses at a time (1:n)
3. [Multi Asset](./lib/fa2/asset/multi_asset.mligo): This is an implementation of 
   Multi Asset Token where there are many tokens (available in different amounts)
   and they can belong to multiple addresses (m:n)   

## Development
To compile the contracts to [Michelson](https://tezos.gitlab.io/active/michelson.html)
, run
```
make compile
``` 

## Tests
A makefile is provided to launch tests.
```
$ make test
```
The tests are available in [./test/fa2](./test/fa2) directory, there is also an
example of [mutation testing](./test/fa2/nft/e2e_mutation.test.mligo) 

## Deploy
A TypeScript program for deployment is provided to originate the smart contract. 
This deployment script relies on .env file which provides the RPC node url and 
the deployer public and private key. (example [.env](./deploy/fa2/nft/.env.example) 
file)
```
$ make test
```

## Usage
1. To install this package, run `ligo install ligo-fa`.
2. In order originate the FA2 contracts from another contract you can use the 
   `CREATE_CONTRACT` Michelcon instruction like this
   ```ocaml
   ...
   let create_my_contract : lambda_create_contract =
       [%Michelson ( {| { 
               UNPAIR ;
               UNPAIR ;
               CREATE_CONTRACT 
   #include "@ligo/fa/compiled/fa2/nft/NFT_mligo.tz"
               ;
               PAIR } |})] in
   ...
   ```
