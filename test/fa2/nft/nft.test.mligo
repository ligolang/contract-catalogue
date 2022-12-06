#import "../../../lib/fa2/nft/NFT.mligo" "FA2_NFT"
#import "../balance_of_callback_contract.mligo" "Callback"
#import "../../helpers/list.mligo" "List_helper"
#import "../../helpers/nft_helpers.mligo" "TestHelpers"

(* Tests for FA2 multi asset contract *)

type return = operation list * FA2_NFT.storage
type main_fn = (FA2_NFT.parameter * FA2_NFT.storage) -> return

(* Transfer *)

(* 1. transfer successful *)
let _test_atomic_tansfer_operator_success (main : main_fn) =
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let transfer_requests = ([
    ({from_=owner1; txs=([({to_=owner2;token_id=1n;amount=1n} : FA2_NFT.atomic_trans);])});
  ] : FA2_NFT.transfer)
  in
  let () = Test.set_source op1 in 
  let (t_addr,_,_) = Test.originate main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let _ = Test.transfer_to_contract_exn contr (Transfer transfer_requests) 0tez in
  let () = TestHelpers.assert_balances t_addr ((owner2, 1n), (owner2, 2n), (owner3, 3n)) in
  ()
let test_atomic_tansfer_operator_success = _test_atomic_tansfer_operator_success FA2_NFT.main

(* 1.1. transfer successful owner *)
let _test_atomic_tansfer_owner_success (main : main_fn) =
  let initial_storage, owners, _ = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let transfer_requests = ([
    ({from_=owner1; txs=([({to_=owner2;token_id=1n;amount=1n} : FA2_NFT.atomic_trans);])});
  ] : FA2_NFT.transfer)
  in
  let () = Test.set_source owner1 in 
  let (t_addr,_,_) = Test.originate main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let _ = Test.transfer_to_contract_exn contr (Transfer transfer_requests) 0tez in
  let () = TestHelpers.assert_balances t_addr ((owner2, 1n), (owner2, 2n), (owner3, 3n)) in
  ()
let test_atomic_tansfer_owner_success = _test_atomic_tansfer_owner_success FA2_NFT.main

(* 2. transfer failure token undefined *)
let _test_transfer_token_undefined (main : main_fn) = 
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let transfer_requests = ([
    ({from_=owner1; txs=([({to_=owner2;token_id=15n;amount=1n} : FA2_NFT.atomic_trans);])});
  ] : FA2_NFT.transfer)
  in
  let () = Test.set_source op1 in 
  let (t_addr,_,_) = Test.originate main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let result = Test.transfer_to_contract contr (Transfer transfer_requests) 0tez in
  TestHelpers.assert_error result FA2_NFT.Errors.undefined_token
let test_transfer_token_undefined = _test_transfer_token_undefined FA2_NFT.main

(* 3. transfer failure incorrect operator *)
let _test_atomic_transfer_failure_not_operator (main : main_fn) = 
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let op2    = List_helper.nth_exn 1 operators in
  let transfer_requests = ([
    ({from_=owner1; txs=([({to_=owner2;token_id=1n;amount=1n} : FA2_NFT.atomic_trans);])});
  ] : FA2_NFT.transfer)
  in
  let () = Test.set_source op2 in 
  let (t_addr,_,_) = Test.originate main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let result = Test.transfer_to_contract contr (Transfer transfer_requests) 0tez in
  TestHelpers.assert_error result FA2_NFT.Errors.not_operator
let test_atomic_transfer_failure_not_operator 
  = _test_atomic_transfer_failure_not_operator FA2_NFT.main

(* 4. self transfer *)
let _test_atomic_tansfer_success_zero_amount_and_self_transfer (main : main_fn) =
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let transfer_requests = ([
    ({from_=owner2; txs=([({to_=owner2;token_id=2n;amount=1n} : FA2_NFT.atomic_trans);])});
  ] : FA2_NFT.transfer)
  in
  let () = Test.set_source op1 in 
  let (t_addr,_,_) = Test.originate main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let _ = Test.transfer_to_contract_exn contr (Transfer transfer_requests) 0tez in
  let () = TestHelpers.assert_balances t_addr ((owner1, 1n), (owner2, 2n), (owner3, 3n)) in
  ()
let test_atomic_tansfer_success_zero_amount_and_self_transfer =
  _test_atomic_tansfer_success_zero_amount_and_self_transfer FA2_NFT.main

(* 5. transfer failure transitive operators *)
let _test_transfer_failure_transitive_operators (main : main_fn) = 
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let op3    = List_helper.nth_exn 2 operators in
  let transfer_requests = ([
    ({from_=owner1; txs=([({to_=owner2;token_id=1n;amount=1n} : FA2_NFT.atomic_trans);])});
  ] : FA2_NFT.transfer)
  in
  let () = Test.set_source op3 in 
  let (t_addr,_,_) = Test.originate main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let result = Test.transfer_to_contract contr (Transfer transfer_requests) 0tez in
  TestHelpers.assert_error result FA2_NFT.Errors.not_operator
let test_transfer_failure_transitive_operators = 
  _test_transfer_failure_transitive_operators FA2_NFT.main

(* Balance of *)

(* 6. empty balance of + callback with empty response *)
let _test_empty_transfer_and_balance_of (main : main_fn) = 
  let initial_storage, _owners, _operators = TestHelpers.get_initial_storage () in
  let (callback_addr,_,_) = Test.originate Callback.main ([] : nat list) 0tez in
  let callback_contract = Test.to_contract callback_addr in

  let balance_of_requests = ({
    requests = ([] : FA2_NFT.request list);
    callback = callback_contract;
  } : FA2_NFT.balance_of) in

  let (t_addr,_,_) = Test.originate main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let _ = Test.transfer_to_contract_exn contr (Balance_of balance_of_requests) 0tez in

  let callback_storage = Test.get_storage callback_addr in
  assert (callback_storage = ([] : nat list))
let test_empty_transfer_and_balance_of = _test_empty_transfer_and_balance_of FA2_NFT.main

(* 7. balance of failure token undefined *)
let _test_balance_of_token_undefines (main : main_fn) = 
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let _op1   = List_helper.nth_exn 0 operators in
  let (callback_addr,_,_) = Test.originate Callback.main ([] : nat list) 0tez in
  let callback_contract = Test.to_contract callback_addr in

  let balance_of_requests = ({
    requests = ([
      {owner=owner1;token_id=0n};
      {owner=owner2;token_id=2n};
      {owner=owner1;token_id=1n};
    ] : FA2_NFT.request list);
    callback = callback_contract;
  } : FA2_NFT.balance_of) in

  let (t_addr,_,_) = Test.originate main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let result = Test.transfer_to_contract contr (Balance_of balance_of_requests) 0tez in
  TestHelpers.assert_error result FA2_NFT.Errors.undefined_token
let test_balance_of_token_undefines = _test_balance_of_token_undefines FA2_NFT.main

(* 8. duplicate balance_of requests *)
let _test_balance_of_requests_with_duplicates (main : main_fn) = 
  let initial_storage, owners, _ = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let (callback_addr,_,_) = Test.originate Callback.main ([] : nat list) 0tez in
  let callback_contract = Test.to_contract callback_addr in

  let balance_of_requests = ({
    requests = ([
      {owner=owner1;token_id=1n};
      {owner=owner2;token_id=2n};
      {owner=owner1;token_id=1n};
      {owner=owner1;token_id=2n};
    ] : FA2_NFT.request list);
    callback = callback_contract;
  } : FA2_NFT.balance_of) in

  let (t_addr,_,_) = Test.originate main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let _ = Test.transfer_to_contract_exn contr (Balance_of balance_of_requests) 0tez in

  let callback_storage = Test.get_storage callback_addr in
  assert (callback_storage = ([1n; 1n; 1n; 0n]))
let test_balance_of_requests_with_duplicates 
  = _test_balance_of_requests_with_duplicates FA2_NFT.main

(* 9. 0 balance if does not hold any tokens (not in ledger) *)
let _test_balance_of_0_balance_if_address_does_not_hold_tokens (main : main_fn) = 
    let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
    let owner1 = List_helper.nth_exn 0 owners in
    let owner2 = List_helper.nth_exn 1 owners in
    let op1    = List_helper.nth_exn 0 operators in
    let (callback_addr,_,_) = Test.originate Callback.main ([] : nat list) 0tez in
    let callback_contract = Test.to_contract callback_addr in

    let balance_of_requests = ({
      requests = ([
        {owner=owner1;token_id=1n};
        {owner=owner2;token_id=2n};
        {owner=op1;token_id=1n};
      ] : FA2_NFT.request list);
      callback = callback_contract;
    } : FA2_NFT.balance_of) in

    let (t_addr,_,_) = Test.originate main initial_storage 0tez in
    let contr = Test.to_contract t_addr in
    let _ = Test.transfer_to_contract_exn contr (Balance_of balance_of_requests) 0tez in

    let callback_storage = Test.get_storage callback_addr in
    assert (callback_storage = ([1n; 1n; 0n]))
let test_balance_of_0_balance_if_address_does_not_hold_tokens = 
  _test_balance_of_0_balance_if_address_does_not_hold_tokens FA2_NFT.main

(* Update operators *)

(* 10. Remove operator & do transfer - failure *)
let _test_update_operator_remove_operator_and_transfer (main : main_fn) = 
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let (t_addr,_,_) = Test.originate main initial_storage 0tez in
  let contr = Test.to_contract t_addr in

  let () = Test.set_source owner1 in 
  let _ = Test.transfer_to_contract_exn contr 
    (Update_operators ([
      (Remove_operator ({
        owner    = owner1;
        operator = op1;
        token_id = 1n;
      } : FA2_NFT.operator) : FA2_NFT.unit_update)
    ] : FA2_NFT.update_operators)) 0tez in

  let () = Test.set_source op1 in
  let transfer_requests = ([
    ({from_=owner1; txs=([({to_=owner2;token_id=1n;amount=1n} : FA2_NFT.atomic_trans);])});
  ] : FA2_NFT.transfer)
  in
  let result = Test.transfer_to_contract contr (Transfer transfer_requests) 0tez in
  TestHelpers.assert_error result FA2_NFT.Errors.not_operator
let test_update_operator_remove_operator_and_transfer = 
  _test_update_operator_remove_operator_and_transfer FA2_NFT.main

(* 10.1. Remove operator & do transfer - failure *)
let _test_update_operator_remove_operator_and_transfer1 (main : main_fn) = 
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner4 = List_helper.nth_exn 3 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let (t_addr,_,_) = Test.originate main initial_storage 0tez in
  let contr = Test.to_contract t_addr in

  let () = Test.set_source owner4 in 
  let _ = Test.transfer_to_contract_exn contr 
    (Update_operators ([
      (Remove_operator ({
        owner    = owner4;
        operator = op1;
        token_id = 4n;
      } : FA2_NFT.operator) : FA2_NFT.unit_update)
    ] : FA2_NFT.update_operators)) 0tez in

  let storage = Test.get_storage t_addr in
  let operator_tokens = Big_map.find_opt (owner4,op1) storage.operators in
  let operator_tokens = Option.unopt operator_tokens in
  assert (operator_tokens = Set.literal [5n])
let test_update_operator_remove_operator_and_transfer1 = 
  _test_update_operator_remove_operator_and_transfer1 FA2_NFT.main


(* 11. Add operator & do transfer - success *)
let _test_update_operator_add_operator_and_transfer (main : main_fn) = 
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let op3    = List_helper.nth_exn 2 operators in
  let (t_addr,_,_) = Test.originate main initial_storage 0tez in
  let contr = Test.to_contract t_addr in

  let () = Test.set_source owner1 in 
  let _ = Test.transfer_to_contract_exn contr 
    (Update_operators ([
      (Add_operator ({
        owner    = owner1;
        operator = op3;
        token_id = 1n;
      } : FA2_NFT.operator) : FA2_NFT.unit_update);
    ] : FA2_NFT.update_operators)) 0tez in

  let () = Test.set_source op3 in
  let transfer_requests = ([
    ({from_=owner1; txs=([({to_=owner2;token_id=1n;amount=1n} : FA2_NFT.atomic_trans);])});
  ] : FA2_NFT.transfer)
  in
  let _ = Test.transfer_to_contract_exn contr (Transfer transfer_requests) 0tez in
  ()
let test_update_operator_add_operator_and_transfer = 
  _test_update_operator_add_operator_and_transfer FA2_NFT.main

(* 11.1. Add operator & do transfer - success *)
let _test_update_operator_add_operator_and_transfer1 (main : main_fn) = 
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner4 = List_helper.nth_exn 3 owners in
  let op3    = List_helper.nth_exn 2 operators in
  let (t_addr,_,_) = Test.originate main initial_storage 0tez in
  let contr = Test.to_contract t_addr in

  let () = Test.set_source owner4 in 
  let _ = Test.transfer_to_contract_exn contr 
    (Update_operators ([
      (Add_operator ({
        owner    = owner4;
        operator = op3;
        token_id = 4n;
      } : FA2_NFT.operator) : FA2_NFT.unit_update);
    ] : FA2_NFT.update_operators)) 0tez in

  let () = Test.set_source op3 in
  let transfer_requests = ([
    ({from_=owner4; txs=([({to_=owner2;token_id=4n;amount=1n} : FA2_NFT.atomic_trans);])});
  ] : FA2_NFT.transfer)
  in
  let _ = Test.transfer_to_contract_exn contr (Transfer transfer_requests) 0tez in
  ()
let test_update_operator_add_operator_and_transfer1 = 
  _test_update_operator_add_operator_and_transfer1 FA2_NFT.main

let _test_only_sender_manage_operators (main : main_fn) = 
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let op3    = List_helper.nth_exn 2 operators in
  let (t_addr,_,_) = Test.originate main initial_storage 0tez in
  let contr = Test.to_contract t_addr in

  let () = Test.set_source owner2 in 
  let result = Test.transfer_to_contract contr 
    (Update_operators ([
      (Add_operator ({
        owner    = owner1;
        operator = op3;
        token_id = 1n;
      } : FA2_NFT.operator) : FA2_NFT.unit_update);
    ] : FA2_NFT.update_operators)) 0tez in

  TestHelpers.assert_error result FA2_NFT.Errors.only_sender_manage_operators

let test_only_sender_manage_operators = _test_only_sender_manage_operators FA2_NFT.main
