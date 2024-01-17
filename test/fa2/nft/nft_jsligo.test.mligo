#import "../../../lib/fa2/nft/nft.impl.jsligo" "FA2_NFT"
#import "../balance_of_callback_contract.mligo" "Callback"
#import "../../helpers/list.mligo" "List_helper"
#import "../../helpers/nft_helpers.mligo" "TestHelpers"
(* Tests for FA2 multi asset contract *)
type return = operation list * FA2_NFT.storage
(* Transfer *)
(* 1. transfer successful *)
let _test_atomic_transfer_operator_success () =
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let transfer_requests = ([
    ({from_=owner1; txs=([({to_=owner2;token_id=1n;amount=1n} : FA2_NFT.TZIP12.atomic_trans);])});
  ] : FA2_NFT.TZIP12.transfer)
  in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of FA2_NFT) initial_storage 0tez in

  let _ = Test.transfer orig.addr (Transfer transfer_requests) 0tez in
  let () = TestHelpers.assert_balances orig.addr ((owner2, 1n), (owner2, 2n), (owner3, 3n)) in
  ()
let test_atomic_transfer_operator_success = _test_atomic_transfer_operator_success ()
(* 1.1. transfer successful owner *)
let _test_atomic_transfer_owner_success () =
  let initial_storage, owners, _ = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let transfer_requests = ([
    ({from_=owner1; txs=([({to_=owner2;token_id=1n;amount=1n} : FA2_NFT.TZIP12.atomic_trans);])});
  ] : FA2_NFT.TZIP12.transfer)
  in
  let () = Test.set_source owner1 in
  let orig = Test.originate (contract_of FA2_NFT) initial_storage 0tez in

  let _ = Test.transfer orig.addr (Transfer transfer_requests) 0tez in
  let () = TestHelpers.assert_balances orig.addr ((owner2, 1n), (owner2, 2n), (owner3, 3n)) in
  ()
let test_atomic_transfer_owner_success = _test_atomic_transfer_owner_success ()
(* 2. transfer failure token undefined *)
let _test_transfer_token_undefined () =
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let transfer_requests = ([
    ({from_=owner1; txs=([({to_=owner2;token_id=15n;amount=1n} : FA2_NFT.TZIP12.atomic_trans);])});
  ] : FA2_NFT.TZIP12.transfer)
  in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of FA2_NFT) initial_storage 0tez in

  let result = Test.transfer orig.addr (Transfer transfer_requests) 0tez in
  TestHelpers.assert_error result FA2_NFT.Errors.undefined_token
let test_transfer_token_undefined = _test_transfer_token_undefined ()
(* 3. transfer failure incorrect operator *)
let _test_atomic_transfer_failure_not_operator () =
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let op2    = List_helper.nth_exn 1 operators in
  let transfer_requests = ([
    ({from_=owner1; txs=([({to_=owner2;token_id=1n;amount=1n} : FA2_NFT.TZIP12.atomic_trans);])});
  ] : FA2_NFT.TZIP12.transfer)
  in
  let () = Test.set_source op2 in
  let orig = Test.originate (contract_of FA2_NFT) initial_storage 0tez in

  let result = Test.transfer orig.addr (Transfer transfer_requests) 0tez in
  TestHelpers.assert_error result FA2_NFT.Errors.not_operator
let test_atomic_transfer_failure_not_operator
  = _test_atomic_transfer_failure_not_operator ()
(* 4. self transfer *)
let _test_atomic_transfer_success_zero_amount_and_self_transfer () =
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let transfer_requests = ([
    ({from_=owner2; txs=([({to_=owner2;token_id=2n;amount=1n} : FA2_NFT.TZIP12.atomic_trans);])});
  ] : FA2_NFT.TZIP12.transfer)
  in
  let () = Test.set_source op1 in
  let orig = Test.originate (contract_of FA2_NFT) initial_storage 0tez in

  let _ = Test.transfer orig.addr (Transfer transfer_requests) 0tez in
  let () = TestHelpers.assert_balances orig.addr ((owner1, 1n), (owner2, 2n), (owner3, 3n)) in
  ()
let test_atomic_transfer_success_zero_amount_and_self_transfer =
  _test_atomic_transfer_success_zero_amount_and_self_transfer ()
(* 5. transfer failure transitive operators *)
let _test_transfer_failure_transitive_operators () =
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let op3    = List_helper.nth_exn 2 operators in
  let transfer_requests = ([
    ({from_=owner1; txs=([({to_=owner2;token_id=1n;amount=1n} : FA2_NFT.TZIP12.atomic_trans);])});
  ] : FA2_NFT.TZIP12.transfer)
  in
  let () = Test.set_source op3 in
  let orig = Test.originate (contract_of FA2_NFT) initial_storage 0tez in

  let result = Test.transfer orig.addr (Transfer transfer_requests) 0tez in
  TestHelpers.assert_error result FA2_NFT.Errors.not_operator
let test_transfer_failure_transitive_operators =
  _test_transfer_failure_transitive_operators ()
(* Balance of *)
(* 6. empty balance of + callback with empty response *)
let _test_empty_transfer_and_balance_of () =
  let initial_storage, _owners, _operators = TestHelpers.get_initial_storage () in
  let orig_callback = Test.originate (contract_of Callback) ([] : nat list) 0tez in
  let callback_contract = Test.to_contract orig_callback.addr in
  let balance_of_requests = ({
    requests = ([] : FA2_NFT.TZIP12.request list);
    callback = callback_contract;
  } : FA2_NFT.TZIP12.balance_of) in
  let orig = Test.originate (contract_of FA2_NFT) initial_storage 0tez in

  let _ = Test.transfer orig.addr (Balance_of balance_of_requests) 0tez in
  let callback_storage = Test.get_storage orig_callback.addr in
  Test.assert (List.size(callback_storage) = 0n)
let test_empty_transfer_and_balance_of = _test_empty_transfer_and_balance_of ()
(* 7. balance of failure token undefined *)
let _test_balance_of_token_undefines () =
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let _op1   = List_helper.nth_exn 0 operators in
  let orig_callback = Test.originate (contract_of Callback) ([] : nat list) 0tez in
  let callback_contract = Test.to_contract orig_callback.addr in
  let balance_of_requests = ({
    requests = ([
      {owner=owner1;token_id=0n};
      {owner=owner2;token_id=2n};
      {owner=owner1;token_id=1n};
    ] : FA2_NFT.TZIP12.request list);
    callback = callback_contract;
  } : FA2_NFT.TZIP12.balance_of) in
  let orig = Test.originate (contract_of FA2_NFT) initial_storage 0tez in

  let result = Test.transfer orig.addr (Balance_of balance_of_requests) 0tez in
  TestHelpers.assert_error result FA2_NFT.Errors.undefined_token
let test_balance_of_token_undefines = _test_balance_of_token_undefines ()
(* 8. duplicate balance_of requests *)
let _test_balance_of_requests_with_duplicates () =
  let initial_storage, owners, _ = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let orig_callback = Test.originate (contract_of Callback) ([] : nat list) 0tez in
  let callback_contract = Test.to_contract orig_callback.addr in
  let balance_of_requests = ({
    requests = ([
      {owner=owner1;token_id=1n};
      {owner=owner2;token_id=2n};
      {owner=owner1;token_id=1n};
      {owner=owner1;token_id=2n};
    ] : FA2_NFT.TZIP12.request list);
    callback = callback_contract;
  } : FA2_NFT.TZIP12.balance_of) in
  let orig = Test.originate (contract_of FA2_NFT) initial_storage 0tez in

  let _ = Test.transfer orig.addr (Balance_of balance_of_requests) 0tez in
  let callback_storage = Test.get_storage orig_callback.addr in
  Test.assert (callback_storage = ([1n; 1n; 1n; 0n]))
let test_balance_of_requests_with_duplicates
  = _test_balance_of_requests_with_duplicates ()
(* 9. 0 balance if does not hold any tokens (not in ledger) *)
let _test_balance_of_0_balance_if_address_does_not_hold_tokens () =
    let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
    let owner1 = List_helper.nth_exn 0 owners in
    let owner2 = List_helper.nth_exn 1 owners in
    let op1    = List_helper.nth_exn 0 operators in
    let orig_callback = Test.originate (contract_of Callback) ([] : nat list) 0tez in
    let callback_contract = Test.to_contract orig_callback.addr in
    let balance_of_requests = ({
      requests = ([
        {owner=owner1;token_id=1n};
        {owner=owner2;token_id=2n};
        {owner=op1;token_id=1n};
      ] : FA2_NFT.TZIP12.request list);
      callback = callback_contract;
    } : FA2_NFT.TZIP12.balance_of) in
    let orig = Test.originate (contract_of FA2_NFT) initial_storage 0tez in

    let _ = Test.transfer orig.addr (Balance_of balance_of_requests) 0tez in
    let callback_storage = Test.get_storage orig_callback.addr in
    Test.assert (callback_storage = ([1n; 1n; 0n]))
let test_balance_of_0_balance_if_address_does_not_hold_tokens =
  _test_balance_of_0_balance_if_address_does_not_hold_tokens ()
(* Update operators *)
(* 10. Remove operator & do transfer - failure *)
let _test_update_operator_remove_operator_and_transfer () =
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let orig = Test.originate (contract_of FA2_NFT) initial_storage 0tez in

  let () = Test.set_source owner1 in
  let _ = Test.transfer orig.addr
    (Update_operators ([
      (Remove_operator ({
        owner    = owner1;
        operator = op1;
        token_id = 1n;
      } : FA2_NFT.TZIP12.operator) : FA2_NFT.TZIP12.unit_update)
    ] : FA2_NFT.TZIP12.update_operators)) 0tez in
  let () = Test.set_source op1 in
  let transfer_requests = ([
    ({from_=owner1; txs=([({to_=owner2;token_id=1n;amount=1n} : FA2_NFT.TZIP12.atomic_trans);])});
  ] : FA2_NFT.TZIP12.transfer)
  in
  let result = Test.transfer orig.addr (Transfer transfer_requests) 0tez in
  TestHelpers.assert_error result FA2_NFT.Errors.not_operator
let test_update_operator_remove_operator_and_transfer =
  _test_update_operator_remove_operator_and_transfer ()
(* 10.1. Remove operator & do transfer - failure *)
let _test_update_operator_remove_operator_and_transfer1 () =
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner4 = List_helper.nth_exn 3 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let orig = Test.originate (contract_of FA2_NFT) initial_storage 0tez in

  let () = Test.set_source owner4 in
  let _ = Test.transfer orig.addr
    (Update_operators ([
      (Remove_operator ({
        owner    = owner4;
        operator = op1;
        token_id = 4n;
      } : FA2_NFT.TZIP12.operator) : FA2_NFT.TZIP12.unit_update)
    ] : FA2_NFT.TZIP12.update_operators)) 0tez in
  let storage = Test.get_storage orig.addr in
  let operator_tokens = Big_map.find_opt (owner4,op1) storage.operators in
  let operator_tokens = Option.unopt operator_tokens in
  Test.assert (operator_tokens = Set.literal [5n])
let test_update_operator_remove_operator_and_transfer1 =
  _test_update_operator_remove_operator_and_transfer1 ()
(* 11. Add operator & do transfer - success *)
let _test_update_operator_add_operator_and_transfer () =
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let op3    = List_helper.nth_exn 2 operators in
  let orig = Test.originate (contract_of FA2_NFT) initial_storage 0tez in

  let () = Test.set_source owner1 in
  let _ = Test.transfer orig.addr
    (Update_operators ([
      (Add_operator ({
        owner    = owner1;
        operator = op3;
        token_id = 1n;
      } : FA2_NFT.TZIP12.operator) : FA2_NFT.TZIP12.unit_update);
    ] : FA2_NFT.TZIP12.update_operators)) 0tez in
  let () = Test.set_source op3 in
  let transfer_requests = ([
    ({from_=owner1; txs=([({to_=owner2;token_id=1n;amount=1n} : FA2_NFT.TZIP12.atomic_trans);])});
  ] : FA2_NFT.TZIP12.transfer)
  in
  let _ = Test.transfer orig.addr (Transfer transfer_requests) 0tez in
  ()
let test_update_operator_add_operator_and_transfer =
  _test_update_operator_add_operator_and_transfer ()
(* 11.1. Add operator & do transfer - success *)
let _test_update_operator_add_operator_and_transfer1 () =
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner4 = List_helper.nth_exn 3 owners in
  let op3    = List_helper.nth_exn 2 operators in
  let orig = Test.originate (contract_of FA2_NFT) initial_storage 0tez in

  let () = Test.set_source owner4 in
  let _ = Test.transfer orig.addr
    (Update_operators ([
      (Add_operator ({
        owner    = owner4;
        operator = op3;
        token_id = 4n;
      } : FA2_NFT.TZIP12.operator) : FA2_NFT.TZIP12.unit_update);
    ] : FA2_NFT.TZIP12.update_operators)) 0tez in
  let () = Test.set_source op3 in
  let transfer_requests = ([
    ({from_=owner4; txs=([({to_=owner2;token_id=4n;amount=1n} : FA2_NFT.TZIP12.atomic_trans);])});
  ] : FA2_NFT.TZIP12.transfer)
  in
  let _ = Test.transfer orig.addr (Transfer transfer_requests) 0tez in
  ()
let test_update_operator_add_operator_and_transfer1 =
  _test_update_operator_add_operator_and_transfer1 ()
let _test_only_sender_manage_operators () =
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let op3    = List_helper.nth_exn 2 operators in
  let orig = Test.originate (contract_of FA2_NFT) initial_storage 0tez in

  let () = Test.set_source owner2 in
  let result = Test.transfer orig.addr
    (Update_operators ([
      (Add_operator ({
        owner    = owner1;
        operator = op3;
        token_id = 1n;
      } : FA2_NFT.TZIP12.operator) : FA2_NFT.TZIP12.unit_update);
    ] : FA2_NFT.TZIP12.update_operators)) 0tez in
  TestHelpers.assert_error result FA2_NFT.Errors.only_sender_manage_operators
let test_only_sender_manage_operators = _test_only_sender_manage_operators ()
