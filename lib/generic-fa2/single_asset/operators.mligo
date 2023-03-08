(**
This file implement the TZIP-12 protocol (a.k.a FA2) for NFT on Tezos
copyright Wulfman Corporation 2021
*)

#import "errors.mligo" "Errors"

type owner    = address
type operator = address
type token_id = nat
type t = (owner, operator set) big_map

(** if transfer policy is Owner_or_operator_transfer *)
let assert_authorisation (operators : t) (from_ : address) : unit =
  let sender_ = (Tezos.get_sender ()) in
  if (sender_ = from_) then ()
  else
  let authorized = match Big_map.find_opt from_ operators with
     Some (a) -> a | None -> Set.empty
  in if Set.mem sender_ authorized then ()
  else failwith Errors.not_operator
(** if transfer policy is Owner_transfer
let assert_authorisation (operators : t) (from_ : address) : unit =
  let sender_ = Tezos.sender in
  if (sender_ = from_) then ()
  else failwith Errors.not_owner
*)

(** if transfer policy is No_transfer
let assert_authorisation (operators : t) (from_ : address) : unit =
  failwith Errors.no_owner
*)

let assert_update_permission (owner : owner) : unit =
  assert_with_error (owner = (Tezos.get_sender ())) "The sender can only manage operators for his own token"
(** For an administator
  let admin = tz1.... in
  assert_with_error (Tezos.sender = admiin) "Only administrator can manage operators"
*)

let add_operator (operators : t) (owner : owner) (operator : operator) : t =
  if owner = operator then operators (* assert_authorisation always allow the owner so this case is not relevant *)
  else
     let () = assert_update_permission owner in
     let auths = match Big_map.find_opt owner operators with
        Some (os) -> os | None -> Set.empty in
     let auths  = Set.add operator auths in
     Big_map.update owner (Some auths) operators

let remove_operator (operators : t) (owner : owner) (operator : operator) : t =
  if owner = operator then operators (* assert_authorisation always allow the owner so this case is not relevant *)
  else
     let () = assert_update_permission owner in
     let auths = match Big_map.find_opt owner operators with
     None -> None | Some (os) ->
        let os = Set.remove operator os in
        if (Set.size os = 0n) then None else Some (os)
     in
     Big_map.update owner auths operators
