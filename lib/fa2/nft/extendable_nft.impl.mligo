[@public] #import "../common/assertions.jsligo" "Assertions"
[@public] #import "../common/errors.mligo" "Errors"
[@public] #import "../common/tzip12.datatypes.jsligo" "TZIP12"
[@public] #import "../common/tzip12.interfaces.jsligo" "TZIP12Interface"
[@public] #import "../common/tzip16.datatypes.jsligo" "TZIP16"

type ledger = (nat, address) big_map

type operator = address

type operators = ((address * operator), nat set) big_map

type 'a storage =
  {
   extension : 'a;
   ledger : ledger;
   operators : operators;
   token_metadata : TZIP12.tokenMetadata;
   metadata : TZIP16.metadata
  }

type 'a ret = operation list * 'a storage

let make_storage (type a) (extension : a) : a storage =
  {
   ledger = Big_map.empty;
   operators = Big_map.empty;
   token_metadata = Big_map.empty;
   metadata = Big_map.empty;
   extension = extension
  }

// Operators
let assert_authorisation
  (operators : operators)
  (from_ : address)
  (token_id : nat)
: unit =
  let sender_ = (Tezos.get_sender ()) in
  if (sender_ = from_)
  then ()
  else
    let authorized =
      match Big_map.find_opt (from_, sender_) operators with
        Some (a) -> a
      | None -> Set.empty in
    if Set.mem token_id authorized then () else failwith Errors.not_operator

let is_operator
  (operators, owner, operator, token_id : (operators * address * address * nat))
: bool =
  let authorized =
    match Big_map.find_opt (owner, operator) operators with
      Some (a) -> a
    | None -> Set.empty in
  (owner = operator || Set.mem token_id authorized)

let add_operator
  (operators : operators)
  (owner : address)
  (operator : operator)
  (token_id : nat)
: operators =
  if owner = operator
  then operators
  (* assert_authorisation always allow the owner so this case is not relevant *)
  else
    let () = Assertions.assert_update_permission owner in
    let auth_tokens =
      match Big_map.find_opt (owner, operator) operators with
        Some (ts) -> ts
      | None -> Set.empty in
    let auth_tokens = Set.add token_id auth_tokens in
    Big_map.update (owner, operator) (Some auth_tokens) operators

let remove_operator
  (operators : operators)
  (owner : address)
  (operator : operator)
  (token_id : nat)
: operators =
  if owner = operator
  then operators
  (* assert_authorisation always allow the owner so this case is not relevant *)
  else
    let () = Assertions.assert_update_permission owner in
    let auth_tokens =
      match Big_map.find_opt (owner, operator) operators with
        None -> None
      | Some (ts) ->
          let ts = Set.remove token_id ts in
          [@no_mutation]
          let is_empty = Set.size ts = 0n in
          if is_empty then None else Some (ts) in
    Big_map.update (owner, operator) auth_tokens operators

//module Ledger = struct
let is_owner_of (ledger : ledger) (token_id : nat) (owner : address) : bool =
  let current_owner: address = Option.value_with_error "option is None" (Big_map.find_opt token_id ledger) in
  current_owner = owner

let assert_owner_of (ledger : ledger) (token_id : nat) (owner : address) : unit =
  Assert.Error.assert (is_owner_of ledger token_id owner) Errors.ins_balance

let transfer_token_from_user_to_user
  (ledger : ledger)
  (token_id : nat)
  (from_ : address)
  (to_ : address)
: ledger =
  let () = assert_owner_of ledger token_id from_ in
  let ledger = Big_map.update token_id (Some to_) ledger in
  ledger

//module Storage = struct
let is_owner_of (type a) (s : a storage) (owner : address) (token_id : nat)
: bool = is_owner_of s.ledger token_id owner

let set_ledger (type a) (s : a storage) (ledger : ledger) =
  {s with ledger = ledger}

let get_operators (type a) (s : a storage) = s.operators

let set_operators (type a) (s : a storage) (operators : operators) =
  {s with operators = operators}

let get_balance (type a) (s : a storage) (owner : address) (token_id : nat)
: nat =
  let () = Assertions.assert_token_exist s.token_metadata token_id in
  if is_owner_of s owner token_id then 1n else 0n

let set_balance (type a) (s : a storage) (owner : address) (token_id : nat)
: a storage =
  let () = Assertions.assert_token_exist s.token_metadata token_id in
  let new_ledger = Big_map.update token_id (Some owner) s.ledger in
  set_ledger s new_ledger

let transfer (type a) (t : TZIP12.transfer) (s : a storage) : a ret =
  (* This function process the "txs" list. Since all transfer share the same "from_" address, we use a se *)
  let process_atomic_transfer
    (from_ : address)
    (ledger, t : ledger * TZIP12.atomic_trans) =
    let {
     to_;
     token_id;
     amount = _
    } = t in
    let () = Assertions.assert_token_exist s.token_metadata token_id in
    let () = assert_authorisation s.operators from_ token_id in
    let ledger = transfer_token_from_user_to_user ledger token_id from_ to_ in
    ledger in
  let process_single_transfer (ledger, t : ledger * TZIP12.transfer_from) =
    let {
     from_;
     txs
    } = t in
    let ledger = List.fold_left (process_atomic_transfer from_) ledger txs in
    ledger in
  let ledger = List.fold_left process_single_transfer s.ledger t in
  let s = set_ledger s ledger in
  ([] : operation list), s

let balance_of (type a) (b : TZIP12.balance_of) (s : a storage) : a ret =
  let {
   requests;
   callback
  } = b in
  let get_balance_info (request : TZIP12.request) : TZIP12.callback =
    let {
     owner;
     token_id
    } = request in
    let balance_ = get_balance s owner token_id in
    {
     request = request;
     balance = balance_
    } in
  let callback_param = List.map get_balance_info requests in
  let operation = Tezos.Next.Operation.transaction (Main callback_param) 0mutez callback in
  ([operation] : operation list), s

let update_operators (type a)
  (updates : TZIP12.update_operators)
  (s : a storage)
: a ret =
  let update_operator (operators, update : operators * TZIP12.unit_update) =
    match update with
      Add_operator
        {
         owner = owner;
         operator = operator;
         token_id = token_id
        } -> add_operator operators owner operator token_id
    | Remove_operator
        {
         owner = owner;
         operator = operator;
         token_id = token_id
        } -> remove_operator operators owner operator token_id in
  let operators = get_operators s in
  let operators = List.fold_left update_operator operators updates in
  let s = set_operators s operators in
  ([] : operation list), s

let get_balance (type a) (p : address * nat) (s : a storage) =
  let (owner, token_id) = p in
  let balance_ = get_balance s owner token_id in
  balance_

(* FIXME? Dynamic supply *)
let total_supply (type a) (token_id : nat) (s : a storage) =
  let () = Assertions.assert_token_exist s.token_metadata token_id in
  1n

let all_tokens (type a) (() : unit) (_s : a storage) : nat set =
  failwith Errors.not_available

let is_operator (type a) (op : TZIP12.operator) (s : a storage) : bool =
  let authorized =
    match Big_map.find_opt (op.owner, op.operator) s.operators with
      Some (opSet) -> opSet
    | None -> Set.empty in
  Set.size authorized > 0n || op.owner = op.operator

let token_metadata (type a) (p : nat) (s : a storage) : TZIP12.tokenMetadataData =
  match Big_map.find_opt p s.token_metadata with
    Some (data) -> data
  | None () -> failwith Errors.undefined_token
