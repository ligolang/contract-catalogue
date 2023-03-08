(**
   This file implement the TZIP-12 protocol (a.k.a FA2) for NFT on Tezos
   copyright Wulfman Corporation 2021
*)

#import "errors.mligo" "Errors"
#import "metadata.mligo" "Metadata"
#import "operators.mligo" "Operators"
#import "tokenMetadata.mligo" "TokenMetadata"
#import "ledger.mligo" "Ledger"

type token_id = nat
type 'a t = {
  metadata: Metadata.t;
  ledger : Ledger.t;
  token_metadata : TokenMetadata.t;
  operators : Operators.t;
  extension : 'a;
}

let get_amount_for_owner (type a) (s: a t) (owner : address) =
  Ledger.get_for_user s.ledger owner

let set_ledger (type a) (s: a t) (ledger:Ledger.t) = {s with ledger = ledger}

let get_operators (type a) (s: a t) = s.operators
let set_operators (type a) (s: a t) (operators:Operators.t) = {s with operators = operators}
