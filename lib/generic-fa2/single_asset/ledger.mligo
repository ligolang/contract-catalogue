(** 
   This file implement the TZIP-12 protocol (a.k.a FA2) for NFT on Tezos
   copyright Wulfman Corporation 2021
*)

#import "errors.mligo" "Errors"

type owner  = address
type amount_ = nat
type t = (owner, amount_) big_map

let get_for_user    (ledger:t) (owner: owner) : amount_ =
  match Big_map.find_opt owner ledger with 
     Some (tokens) -> tokens
  |  None          -> 0n

let update_for_user (ledger:t) (owner: owner) (amount_ : amount_) : t = 
  Big_map.update owner (Some amount_) ledger

let decrease_token_amount_for_user (ledger : t) (from_ : owner) (amount_ : amount_) : t = 
  let tokens = get_for_user ledger from_ in
  let () = assert_with_error (tokens >= amount_) Errors.ins_balance in
  let tokens = abs(tokens - amount_) in
  let ledger = update_for_user ledger from_ tokens in
  ledger 

let increase_token_amount_for_user (ledger : t) (to_   : owner) (amount_ : amount_) : t = 
  let tokens = get_for_user ledger to_ in
  let tokens = tokens + amount_ in
  let ledger = update_for_user ledger to_ tokens in
  ledger 
