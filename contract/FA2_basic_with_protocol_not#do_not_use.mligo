(** 
   This file implement the TZIP-12 protocol (a.k.a FA2) on Tezos
   copyright Wulfman Corporation 2021
*)

(* 
   Errors
*)
module Errors = struct
   let undefined_token = "FA2_TOKEN_UNDEFINED"
   let ins_balance     = "FA2_INSUFFICIENT_BALANCE"
   let no_transfer     = "FA2_TX_DENIED"
   let not_owner       = "FA2_NOT_OWNER"
   let not_operator    = "FA2_NOT_OPERATOR"
   let not_supported   = "FA2_OPERATORS_UNSUPPORTED"
   let rec_hook_fail   = "FA2_RECEIVER_HOOK_FAILED"
   let send_hook_fail  = "FA2_SENDER_HOOK_FAILED"
   let rec_hook_undef  = "FA2_RECEIVER_HOOK_UNDEFINED"
   let send_hook_under = "FA2_SENDER_HOOK_UNDEFINED"
end

module Operators = struct
   
(**
Operators
Operator is a Tezos address that originates token transfer operation on behalf
of the owner.
Owner is a Tezos address which can hold tokens.
An operator, other than the owner, MUST be approved to manage specific tokens
held by the owner to transfer them from the owner account.
FA2 interface specifies an entrypoint to update operators. Operators are permitted
per specific token owner and token ID (token type). Once permitted, an operator
can transfer tokens of that type belonging to the owner.
*)
   type owner    = address
   type operator = address
   type token_id = nat
   type t = ((owner * operator), token_id set) big_map
(** 
Default Transfer Permission Policy


Token owner address MUST be able to perform a transfer of its own tokens (e. g.
SENDER equals to from_ parameter in the transfer).


An operator (a Tezos address that performs token transfer operation on behalf
of the owner) MUST be permitted to manage the specified owner's tokens before
it invokes a transfer transaction (see update_operators).


If the address that invokes a transfer operation is neither a token owner nor
one of the permitted operators, the transaction MUST fail with the error mnemonic
"FA2_NOT_OPERATOR". If at least one of the transfers in the batch is not permitted,
the whole transaction MUST fail.
*)
(** if transfer policy is Owner_or_operator_transfer *)
   let assert_authorisation (operators : t) (from_ : address) (token_id : nat) : unit = 
      let sender_ = Tezos.sender in
      if (sender_ = from_) then ()
      else 
      let authorized = match Big_map.find_opt (from_,sender_) operators with
         Some (a) -> a | None -> Set.empty
      in if Set.mem token_id authorized then ()
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

(** 
The standard does not specify who is permitted to update operators on behalf of
the token owner. Depending on the business use case, the particular implementation
of the FA2 contract MAY limit operator updates to a token owner (owner == SENDER)
or be limited to an administrator.
*)
   let assert_update_permission (owner : owner) : unit =
      assert_with_error (owner = Tezos.sender) "The sender can only manage operators for his own token"
   (** For an administator
      let admin = tz1.... in
      assert_with_error (Tezos.sender = admiin) "Only administrator can manage operators"
   *)

   let add_operator (operators : t) (owner : owner) (operator : operator) (token_id : token_id) : t =
      if owner = operator then operators (* assert_authorisation always allow the owner so this case is not relevant *)
      else
         let () = assert_update_permission owner in
         let auth_tokens = match Big_map.find_opt (owner,operator) operators with
            Some (ts) -> ts | None -> Set.empty in
         let auth_tokens  = Set.add token_id auth_tokens in
         Big_map.update (owner,operator) (Some auth_tokens) operators
         
   let remove_operator (operators : t) (owner : owner) (operator : operator) (token_id : token_id) : t =
      if owner = operator then operators (* assert_authorisation always allow the owner so this case is not relevant *)
      else
         let () = assert_update_permission owner in
         let auth_tokens = match Big_map.find_opt (owner,operator) operators with
         None -> None | Some (ts) ->
            let ts = Set.remove token_id ts in
            if (Set.size ts = 0n) then None else Some (ts)
         in
         Big_map.update (owner,operator) auth_tokens operators
end

module Collection = struct
   type token_id = nat
   type amount_  = nat
   type t = (token_id, amount_) map

   let empty = (Map.empty : t)

   let get_amount_for_token (collection : t) (token_id : token_id) : amount_ = 
      match (Map.find_opt token_id collection) with
         Some (amount_) -> amount_
      |  None           -> 0n

   let increase_amount (tokens : t) (token_id : nat) (amount_ : nat) =
      let current_amount = get_amount_for_token tokens token_id in
      let new_amount     = current_amount + amount_ in
      Map.update token_id (Some new_amount) tokens

   let decrease_amount (tokens : t) (token_id : nat) (amount_ : nat) =
      let current_amount = get_amount_for_token tokens token_id in
      let () = assert_with_error (current_amount >= amount_) Errors.ins_balance in
      let new_amount     = abs(current_amount - amount_) in
      if (new_amount = 0n) then Map.remove token_id tokens
         else Map.update token_id (Some new_amount) tokens 
end
module Ledger = struct
   type owner = address
   type t = (owner, Collection.t) big_map
   
   let get_for_user (ledger:t) (owner: owner) : Collection.t =
      match Big_map.find_opt owner ledger with Some (col) -> col | None -> Collection.empty
   

   let update_for_user (ledger:t) (owner: owner) (tokens : Collection.t) : t = 
      Big_map.update owner (Some tokens) ledger

   let decrease_token_amount_for_user (ledger : t) (from_ : owner) (token_id : nat) (amount_ : nat) : t = 
      let tokens = get_for_user ledger from_ in
      let tokens = Collection.decrease_amount tokens token_id amount_ in
      let ledger = update_for_user ledger from_ tokens in
      ledger 

   let increase_token_amount_for_user (ledger : t) (to_   : owner) (token_id : nat) (amount_ : nat) : t = 
      let tokens = get_for_user ledger to_ in
      let tokens = Collection.increase_amount tokens token_id amount_ in
      let ledger = update_for_user ledger to_ tokens in
      ledger 
end

module TokenMetadata = struct
   (**
      This should be initialized at origination, conforming to either 
      TZIP-12 : https://gitlab.com/tezos/tzip/-/blob/master/proposals/tzip-12/tzip-12.md#token-metadata
      or TZIP-16 : https://gitlab.com/tezos/tzip/-/blob/master/proposals/tzip-12/tzip-12.md#contract-metadata-tzip-016 
   *)
   type data = {token_id:nat;token_info:(string,bytes)map}
   type t = (nat, data) big_map 
end

module Storage = struct
   type token_id = nat
   type t = {
      ledger : Ledger.t;
      token_metadata : TokenMetadata.t;
      operators : Operators.t;
   }

   let get_token_for_owner (s:t) (owner : address) = 
      match Big_map.find_opt owner s.ledger with 
         Some (tokens) -> tokens
      |  None          -> Map.empty

   let assert_token_exist (s:t) (token_id : nat) : unit  = 
      let _ = Option.unopt_with_error (Big_map.find_opt token_id s.token_metadata)
         Errors.undefined_token in
      ()

   let set_ledger (s:t) (ledger:Ledger.t) = {s with ledger = ledger}

   let get_operators (s:t) = s.operators
   let set_operators (s:t) (operators:Operators.t) = {s with operators = operators}
end


type storage = Storage.t

(** transfer entrypoint
(list %transfer
  (pair
    (address %from_)
    (list %txs
      (pair
        (address %to_)
        (pair
          (nat %token_id)
          (nat %amount)
        )
      )
    )
  )
)
*)

(*
Each transfer in the batch is specified between one source (from_) address and
a list of destinations. Each transfer_destination specifies token type and the
amount_ to be transferred from the source address to the destination (to_) address.
FA2 does NOT specify an interface for mint and burn operations; however, if an
FA2 token contract implements mint and burn operations, it SHOULD, when possible,
enforce the same logic (core transfer behavior and transfer permission logic)
applied to the token transfer operation. Mint and burn can be considered special
cases of the transfer. Although, it is possible that mint and burn have more or
less restrictive rules than the regular transfer. For instance, mint and burn
operations may be invoked by a special privileged administrative address only.
In this case, regular operator restrictions may not be applicable.
*)


type atomic_trans = [@layout:comb] {
   to_      : address;
   token_id : nat;
   amount   : nat;
}

type transfer_from = {
   from_ : address;
   tx    : atomic_trans list
}
type transfer = transfer_from list

(*
Core Transfer Behavior
FA2 token contracts MUST always implement this behavior.


Every transfer operation MUST happen atomically and in order. If at least one
transfer in the batch cannot be completed, the whole transaction MUST fail, all
token transfers MUST be reverted, and token balances MUST remain unchanged.
-> Modification of storage at the end, common design pattern

Each transfer in the batch MUST decrement token balance of the source (from_)
address by the amount of the transfer and increment token balance of the destination
(to_) address by the amount of the transfer.
-> Expected behavior

If the transfer amount exceeds current token balance of the source address,
the whole transfer operation MUST fail with the error mnemonic "FA2_INSUFFICIENT_BALANCE".
-> adding the error

If the token owner does not hold any tokens of type token_id, the owner's balance
is interpreted as zero. No token owner can have a negative balance.
-> Enforced by data type


The transfer MUST update token balances exactly as the operation
parameters specify it. Transfer operations MUST NOT try to adjust transfer
amounts or try to add/remove additional transfers like transaction fees.

Transfers of zero amount MUST be treated as normal transfers.
-> don't cover corner cases

Transfers with the same address (from_ equals to_) MUST be treated as normal
transfers.

If one of the specified token_ids is not defined within the FA2 contract, the
entrypoint MUST fail with the error mnemonic "FA2_TOKEN_UNDEFINED".
-> need to have a collection of defined token (big_map ?)


Transfer implementations MUST apply transfer permission policy logic (either
default transfer permission policy or
customized one).
If permission logic rejects a transfer, the whole operation MUST fail.
-> permission logic ?


Core transfer behavior MAY be extended. If additional constraints on tokens
transfer are required, FA2 token contract implementation MAY invoke additional
permission policies. If the additional permission fails, the whole transfer
operation MUST fail with a custom error mnemonic.
*)

let transfer : transfer -> storage -> operation list * storage = 
   fun (t:transfer) (s:storage) -> 
   (* This function process the "tx" list. Since all transfer share the same "from_" address, we use a se *)
   let process_atomic_transfer (from_:address) (ledger, t:Ledger.t * atomic_trans) =
      let {to_;token_id;amount=amount_} = t in
      let ()     = Storage.assert_token_exist s token_id in
      let ()     = Operators.assert_authorisation s.operators from_ token_id in
      let ledger = Ledger.decrease_token_amount_for_user ledger from_ token_id amount_ in
      let ledger = Ledger.increase_token_amount_for_user ledger to_   token_id amount_ in
      ledger
   in
   let process_single_transfer (ledger, t:Ledger.t * transfer_from ) =
      let {from_;tx} = t in
      let ledger     = List.fold_left (process_atomic_transfer from_) ledger tx in
      ledger
   in
   let ledger = List.fold_left process_single_transfer s.ledger t in
   let s = Storage.set_ledger s ledger in
   ([]: operation list),s

(** balance_of entrypoint 
(pair %balance_of
  (list %requests
    (pair
      (address %owner)
      (nat %token_id)
    )
  )
  (contract %callback
    (list
      (pair
        (pair %request
          (address %owner)
          (nat %token_id)
        )
        (nat %balance)
      )
    )
  )
)
*)
type request = {
   owner    : address;
   token_id : nat;
}

(*The layout allow to fix the ordering *)
type callback = [@layout:comb] {
   request : request;
   balance : nat;
}

(*Empty batch is a valid input and MUST be processed as a non-empty one.
For example, an empty transfer batch will not affect token balances, but applicable
transfer core behavior and permission policy MUST be applied. *)
type balance_of = [@layout:comb] {
   requests : request list;
   callback : callback list contract;
}

(** 
Gets the balance of multiple account/token pairs. Accepts a list of
balance_of_requests and a callback contract callback which accepts a list of
balance_of_response records.


There may be duplicate balance_of_request's, in which case they should not be
deduplicated nor reordered.


If the account does not hold any tokens, the account
balance is interpreted as zero.


If one of the specified token_ids is not defined within the FA2 contract, the
entrypoint MUST fail with the error mnemonic "FA2_TOKEN_UNDEFINED".


Notice: The balance_of entrypoint implements a continuation-passing style (CPS)
view entrypoint pattern that invokes the other callback contract with the requested
data. This pattern, when not used carefully, could expose the callback contract
to an inconsistent state and/or manipulatable outcome (see
view patterns).
The balance_of entrypoint should be used on the chain with extreme caution.
*)

(* Invocation of the balance_of entrypoint with an empty batch input MUST result in a call to a
callback contract with an empty response batch. *)
let balance_of : balance_of -> storage -> operation list * storage = 
   fun (b: balance_of) (s: storage) -> 
   let {requests;callback} = b in
   let get_balance_info (request : request) : callback =
      let {owner;token_id} = request in
      let ()          = Storage.assert_token_exist  s token_id in 
      let owner_token = Storage.get_token_for_owner s owner    in
      let balance_    = Collection.get_amount_for_token owner_token token_id in
      {request=request;balance=balance_}
   in
   let callback_param = List.map get_balance_info requests in
   let operation = Tezos.transaction callback_param 0tez callback in
   ([operation]: operation list),s

(** update operators entrypoint *)
(**
(list %update_operators
  (or
    (pair %add_operator
      (address %owner)
      (pair
        (address %operator)
        (nat %token_id)
      )
    )
    (pair %remove_operator
      (address %owner)
      (pair
        (address %operator)
        (nat %token_id)
      )
    )
  )
)
*)
type operator = [@layout:comb] {
   owner    : address;
   operator : address;
   token_id : nat; 
}

type unit_update      = Add_operator of operator | Remove_operator of operator
type update_operators = unit_update list
(**
Add or Remove token operators for the specified token owners and token IDs.


The entrypoint accepts a list of update_operator commands. If two different
commands in the list add and remove an operator for the same token owner and
token ID, the last command in the list MUST take effect.


It is possible to update operators for a token owner that does not hold any token
balances yet.


Operator relation is not transitive. If C is an operator of B and if B is an
operator of A, C cannot transfer tokens that are owned by A, on behalf of B.


*)
let update_ops : update_operators -> storage -> operation list * storage = 
   fun (updates: update_operators) (s: storage) -> 
   let update_operator (operators,update : Operators.t * unit_update) = match update with 
      Add_operator    {owner=owner;operator=operator;token_id=token_id} -> Operators.add_operator    operators owner operator token_id
   |  Remove_operator {owner=owner;operator=operator;token_id=token_id} -> Operators.remove_operator operators owner operator token_id
   in
   let operators = Storage.get_operators s in
   let operators = List.fold_left update_operator operators updates in
   let s = Storage.set_operators s operators in
   ([]: operation list),s

(** If transfer_policy is  No_transfer or Owner_transfer
let update_ops : update_operators -> storage -> operation list * storage = 
   fun (updates: update_operators) (s: storage) -> 
   let () = failwith Errors.not_supported in
   ([]: operation list),s
*)


type parameter = [@layout:comb] | Transfer of transfer | Balance_of of balance_of | Update_operators of update_operators
let main ((p,s):(parameter * storage)) = match p with
   Transfer         p -> transfer   p s
|  Balance_of       p -> balance_of p s
|  Update_operators p -> update_ops p s
