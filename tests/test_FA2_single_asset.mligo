#include "../contract/FA2_single_asset.mligo"
#import "./balance_of_callback_contract.mligo" "Callback"

type owner    = address
type operator = address
type token_id = nat
type amount_ = nat

type storage = Storage.t

module Big_map_helper = struct
  
  let find_opt_exn (type a b) (i: a) (b: (a, b) big_map) : b =
    match Big_map.find_opt i b with 
      Some s -> s
    | None -> 
      failwith "Not found in big map"
  
end

module List_helper = struct 

  let nth_exn (type a) (i: int) (a: a list) : a =
    let rec aux (remaining: a list) (cur: int) : a =
      match remaining with 
       [] -> 
        failwith "Not found in list"
      | hd :: tl -> 
          if cur = i then 
            hd 
          else aux tl (cur + 1)
    in
    aux a 0  

end

module Test_helper = struct

  let addresses (no_of_accounts: int) = 
    let rec aux (result: address list) (cur: int) : address list = 
      if cur > 0 then (
        aux (Test.nth_bootstrap_account cur :: result) (cur - 1)
      )
      else 
        result
      in aux ([]: address list) no_of_accounts

  let setup (amount_: tez list) = 
    let () = Test.reset_state (List.length amount_) amount_ in
    let ledger = Big_map.literal ([
        (Test.nth_bootstrap_account 1, 10n)
      ])
    in
    let operators = (Big_map.empty:  (address, address set) big_map) in
    let operators = 
      List.fold_left 
        (fun ((acc, key): (address, address set) big_map * address) -> (
          let value = List.fold_left (fun ((s, i): address set * address) -> Set.add i s) (Set.empty: address set) (addresses (List.length amount_ - 1)) in
          Big_map.add key value acc) 
        )
        operators 
        (addresses (List.length amount_ - 1)) 
    in
    let token_info = (Map.empty: (string, bytes) map) in
    let token_metadata = {
      token_id = 0n;
      token_info = token_info;
    } in
    let initial_storage = ({
      ledger = ledger;
      token_metadata = token_metadata;
      operators = operators;
    }: storage)
    in
    let (t_addr,_,_) = Test.originate main initial_storage 1tez in
    let (b_addr,_,_) = Test.originate Callback.main ([] : nat list) 1tez in
    t_addr, b_addr, addresses (List.length amount_ - 1)

end

(*
  Every transfer operation MUST happen atomically and in order. If at least one 
  transfer in the batch cannot be completed, the whole transaction MUST fail, 
  all token transfers MUST be reverted, and token balances MUST remain 
  unchanged.
*)
let test_atomic () =
  let (t_addr, b_addr, addresses) = Test_helper.setup [8000tez; 5000tez; 5000tez; 5000tez; 5000tez; 1tez] in
  let account1 = List_helper.nth_exn 0 addresses in
  let account2 = List_helper.nth_exn 1 addresses in
  let account3 = List_helper.nth_exn 2 addresses in
  let account4 = List_helper.nth_exn 3 addresses in
  let account5 = List_helper.nth_exn 4 addresses in
  let contr = Test.to_contract t_addr in
  let b_contr = b_addr in
  (* Test order of transactions *)
  let () = Test.transfer_to_contract_exn contr (Transfer [
    {
      from_ = account1;
      tx = ([
        {
          amount = 1n;
          to_ = account2;
        };
        {
          amount = 1n;
          to_ = account3;
        };
        {
          amount = 1n;
          to_ = account4;
        };
      ]: atomic_trans list);
    };
    {
      from_ = account2;
      tx = ([
        {
          amount = 1n;
          to_ = account3;
        }
      ])
    };
    {
      from_ = account3;
      tx = ([
        {
          amount = 2n;
          to_ = account4;
        }
      ])
    }
  ]) 10mutez in
  
  (* Ensure batch fails completely if one transaction in the batch goes wrong *)
  let () = match (Test.transfer_to_contract contr (
    (Transfer [
      {
      from_ = account1;
      tx = ([
        {
          amount = 1n;
          to_ = account2;
        };

      ])
      };
    {
      from_ = account5;
      tx = ([
        {
          amount = 1n;
          to_ = account1
        }]
      )
    }
  ])) 10mutez
  ) with 
  | Success -> failwith "Unexpected success"
  | Fail _ -> 
    (* Tezos.get_contract_with_error *)
    type x = {balance: nat; request: {owner: address; token_id: nat}} in
    let callback = (Test.to_contract b_contr) in
    ( match (Test.transfer_to_contract contr (
      (Balance_of {
        callback = callback;  
        requests = ([]: request list);
      })
    ) 1tez) with 
      Success -> Test.log "okay"
    | Fail _ -> Test.log "fail")
      (* let storage:Storage.t = Test.get_storage(t_addr) in 
      let a1 = Big_map_helper.find_opt_exn account1 storage.ledger in
      let a2 = Big_map_helper.find_opt_exn account2 storage.ledger in
      let () = assert (a1 = 7n) in
      let () = assert (a2 = 0n) in
      () *)
  in
  ()

(*
  Each transfer in the batch MUST decrement token balance of the source (from_) 
  address by the amount of the transfer and increment token balance of the 
  destination (to_) address by the amount of the transfer.
*)
let test_decrement () = 
  let (t_addr, b_add, addresses) = Test_helper.setup [8000tez; 5000tez; 5000tez] in
  let account1 = List_helper.nth_exn 0 addresses in
  let account2 = List_helper.nth_exn 1 addresses in
  let contr = Test.to_contract t_addr in
  let () = Test.transfer_to_contract_exn contr (Transfer [
    {
      from_ = account1;
      tx = ([
        {
          amount = 3n;
          to_ = account2;
        };
      ]: atomic_trans list);
    };
  ]) 10mutez in
  let storage:Storage.t = Test.get_storage(t_addr) in 
  let a1 = Big_map_helper.find_opt_exn account1 storage.ledger in
  let a2 = Big_map_helper.find_opt_exn account2 storage.ledger in
  let () = assert(a1 = 7n) in
  let () = assert(a2 = 3n) in
  ()


(*
  If the transfer amount exceeds current token balance of the source address, 
  the whole transfer operation MUST fail with the error mnemonic 
  "FA2_INSUFFICIENT_BALANCE".
*)
let test_insufficient_balance () = 
  let (t_addr, b_addr, addresses) = Test_helper.setup [8000tez; 100tez; 1tez] in
  let account1 = List_helper.nth_exn 0 addresses in
  let account2 = List_helper.nth_exn 1 addresses in
  let contr = Test.to_contract t_addr in
  let result = Test.transfer_to_contract contr (Transfer [
    {
      from_ = account2;
      tx = ([
        {
          amount = 3n;
          to_ = account1;
        };
      ]: atomic_trans list);
    };
  ]) 10tez in
  match result with 
    Success -> failwith "Unexpected success"
  | Fail (Rejected (err, _)) -> assert (Test.michelson_equal err (Test.eval Errors.ins_balance))
  | Fail (Other _) -> failwith "Unexpected fail other"


(* 
  If the token owner does not hold any tokens of type token_id, the owner's 
  balance is interpreted as zero. No token owner can have a negative balance. 
*)
let test_balance_zero =
()  

(*
(*
  The transfer MUST update token balances exactly as the operation parameters 
  specify it. Transfer operations MUST NOT try to adjust transfer amounts or 
  try to add/remove additional transfers like transaction fees.
*)
let test_update_token_balances = 

(*
  Transfers of zero amount MUST be treated as normal transfers.
*)
let test_transfer_zero_amount =

(*
  Transfers with the same address (from_ equals to_) MUST be treated as normal
  transfers.
*)
let test_transfer_same_address =

(* 
  If one of the specified token_ids is not defined within the FA2 contract, the 
  entrypoint MUST fail with the error mnemonic "FA2_TOKEN_UNDEFINED".
*)
let test_token_undefined =

(*
  Transfer implementations MUST apply transfer permission policy logic (either 
  default transfer permission policy or customized one). If permission logic 
  rejects a transfer, the whole operation MUST fail.
*)
let test_transfer_permission_policy_logic =

(*
  Core transfer behavior MAY be extended. If additional constraints on tokens 
  transfer are required, FA2 token contract implementation MAY invoke additional 
  permission policies. If the additional permission fails, the whole transfer 
  operation MUST fail with a custom error mnemonic.
*)
let test_custom =

(*
  The entrypoint accepts a list of update_operator commands. If two different 
  commands in the list add and remove an operator for the same token owner and 
  token ID, the last command in the list MUST take effect.
*)
let test_update_operator_order =

(* 
  It is possible to update operators for a token owner that does not hold any 
  token balances yet.
*)
let test_no_token_balance =

(*
  Operator relation is not transitive. If C is an operator of B and if B is an 
  operator of A, C cannot transfer tokens that are owned by A, on behalf of B.
*)
let test_no_transitive =

*)

let test = 
  let () = test_atomic () in
  let () = test_decrement () in
  let () = test_insufficient_balance () in
  ()