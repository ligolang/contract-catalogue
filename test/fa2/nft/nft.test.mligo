#import "../../../lib/fa2/nft/NFT.mligo" "FA2_NFT"
#import "../balance_of_callback_contract.mligo" "Callback"
#import "../../helpers/list.mligo" "List_helper"
#import "../../helpers/nft_helpers.mligo" "TestHelpers"

(* Tests for FA2 multi asset contract *)

(* Transfer *)

(* 1. transfer successful *)
let test_atomic_tansfer_success =
  let initial_storage, owners, operators = TestHelpers.get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let transfer_requests = ([
    ({from_=owner1; tx=([{to_=owner2;token_id=1n};] : FA2_NFT.atomic_trans list)});
  ] : FA2_NFT.transfer)
  in
  let () = Test.set_source op1 in 
  let (t_addr,_,_) = Test.originate FA2_NFT.main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let () = Test.transfer_to_contract_exn contr (Transfer transfer_requests) 0tez in
  let () = TestHelpers.assert_balances t_addr ((owner2, 2n), (owner2, 2n), (owner3, 3n)) in
  ()

(* 2. transfer failure token undefined *)
let test_transfer_token_undefined = 
  let initial_storage, owners, operators = TestHelpers.get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let transfer_requests = ([
    ({from_=owner1; tx=([{to_=owner2;token_id=5n};] : FA2_NFT.atomic_trans list)});
  ] : FA2_NFT.transfer)
  in
  let () = Test.set_source op1 in 
  let (t_addr,_,_) = Test.originate FA2_NFT.main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let result = Test.transfer_to_contract contr (Transfer transfer_requests) 0tez in
  TestHelpers.assert_error result FA2_NFT.Errors.undefined_token

(* 3. transfer failure incorrect operator *)
let test_atomic_transfer_failure_not_operator = 
  let initial_storage, owners, operators = TestHelpers.get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op2    = List_helper.nth_exn 1 operators in
  let transfer_requests = ([
    ({from_=owner1; tx=([{to_=owner2;token_id=1n};] : FA2_NFT.atomic_trans list)});
  ] : FA2_NFT.transfer)
  in
  let () = Test.set_source op2 in 
  let (t_addr,_,_) = Test.originate FA2_NFT.main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let result = Test.transfer_to_contract contr (Transfer transfer_requests) 0tez in
  TestHelpers.assert_error result FA2_NFT.Errors.not_operator

(* 4. self transfer *)
let test_atomic_tansfer_success_zero_amount_and_self_transfer =
  let initial_storage, owners, operators = TestHelpers.get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let transfer_requests = ([
    ({from_=owner2; tx=([{to_=owner2;token_id=2n};] : FA2_NFT.atomic_trans list)});
  ] : FA2_NFT.transfer)
  in
  let () = Test.set_source op1 in 
  let (t_addr,_,_) = Test.originate FA2_NFT.main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let () = Test.transfer_to_contract_exn contr (Transfer transfer_requests) 0tez in
  let () = TestHelpers.assert_balances t_addr ((owner1, 1n), (owner2, 2n), (owner3, 3n)) in
  ()

(* 5. transfer failure transitive operators *)
let test_transfer_failure_transitive_operators = 
  let initial_storage, owners, operators = TestHelpers.get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op3    = List_helper.nth_exn 2 operators in
  let transfer_requests = ([
    ({from_=owner1; tx=([{to_=owner2;token_id=1n};] : FA2_NFT.atomic_trans list)});
  ] : FA2_NFT.transfer)
  in
  let () = Test.set_source op3 in 
  let (t_addr,_,_) = Test.originate FA2_NFT.main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let result = Test.transfer_to_contract contr (Transfer transfer_requests) 0tez in
  TestHelpers.assert_error result FA2_NFT.Errors.not_operator

(* Balance of *)

(* 6. empty balance of + callback with empty response *)
let test_empty_transfer_and_balance_of = 
  let initial_storage, owners, operators = TestHelpers.get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let (callback_addr,_,_) = Test.originate Callback.main ([] : nat list) 0tez in
  let callback_contract = Test.to_contract callback_addr in

  let balance_of_requests = ({
    requests = ([] : FA2_NFT.request list);
    callback = callback_contract;
  } : FA2_NFT.balance_of) in

  let (t_addr,_,_) = Test.originate FA2_NFT.main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let () = Test.transfer_to_contract_exn contr (Balance_of balance_of_requests) 0tez in

  let callback_storage = Test.get_storage callback_addr in
  assert (callback_storage = ([] : nat list))

(* 7. balance of failure token undefined *)
let test_balance_of_token_undefines = 
  let initial_storage, owners, operators = TestHelpers.get_initial_storage (10n, 5n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
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

  let (t_addr,_,_) = Test.originate FA2_NFT.main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let result = Test.transfer_to_contract contr (Balance_of balance_of_requests) 0tez in
  TestHelpers.assert_error result FA2_NFT.Errors.undefined_token

(* 8. duplicate balance_of requests *)
let test_balance_of_requests_with_duplicates = 
  let initial_storage, owners, operators = TestHelpers.get_initial_storage (10n, 5n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
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

  let (t_addr,_,_) = Test.originate FA2_NFT.main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let () = Test.transfer_to_contract_exn contr (Balance_of balance_of_requests) 0tez in

  let callback_storage = Test.get_storage callback_addr in
  assert (callback_storage = ([1n; 1n; 1n; 0n]))

(* 9. 0 balance if does not hold any tokens (not in ledger) *)
let test_balance_of_0_balance_if_address_does_not_hold_tokens = 
    let initial_storage, owners, operators = TestHelpers.get_initial_storage (10n, 5n, 10n) in
    let owner1 = List_helper.nth_exn 0 owners in
    let owner2 = List_helper.nth_exn 1 owners in
    let owner3 = List_helper.nth_exn 2 owners in
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

    let (t_addr,_,_) = Test.originate FA2_NFT.main initial_storage 0tez in
    let contr = Test.to_contract t_addr in
    let () = Test.transfer_to_contract_exn contr (Balance_of balance_of_requests) 0tez in

    let callback_storage = Test.get_storage callback_addr in
    assert (callback_storage = ([1n; 1n; 0n]))

(* Update operators *)

(* 10. Remove operator & do transfer - failure *)
let test_update_operator_remove_operator_and_transfer = 
  let initial_storage, owners, operators = TestHelpers.get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let (t_addr,_,_) = Test.originate FA2_NFT.main initial_storage 0tez in
  let contr = Test.to_contract t_addr in

  let () = Test.set_source owner1 in 
  let () = Test.transfer_to_contract_exn contr 
    (Update_operators ([
      (Remove_operator ({
        owner    = owner1;
        operator = op1;
        token_id = 1n;
      } : FA2_NFT.operator) : FA2_NFT.unit_update)
    ] : FA2_NFT.update_operators)) 0tez in

  let () = Test.set_source op1 in
  let transfer_requests = ([
    ({from_=owner1; tx=([{to_=owner2;token_id=1n};] : FA2_NFT.atomic_trans list)});
  ] : FA2_NFT.transfer)
  in
  let result = Test.transfer_to_contract contr (Transfer transfer_requests) 0tez in
  TestHelpers.assert_error result FA2_NFT.Errors.not_operator

(* 11. Add operator & do transfer - success *)
let test_update_operator_add_operator_and_transfer = 
  let initial_storage, owners, operators = TestHelpers.get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op3    = List_helper.nth_exn 2 operators in
  let (t_addr,_,_) = Test.originate FA2_NFT.main initial_storage 0tez in
  let contr = Test.to_contract t_addr in

  let () = Test.set_source owner1 in 
  let () = Test.transfer_to_contract_exn contr 
    (Update_operators ([
      (Add_operator ({
        owner    = owner1;
        operator = op3;
        token_id = 1n;
      } : FA2_NFT.operator) : FA2_NFT.unit_update);
    ] : FA2_NFT.update_operators)) 0tez in

  let () = Test.set_source op3 in
  let transfer_requests = ([
    ({from_=owner1; tx=([{to_=owner2;token_id=1n};] : FA2_NFT.atomic_trans list)});
  ] : FA2_NFT.transfer)
  in
  let () = Test.transfer_to_contract_exn contr (Transfer transfer_requests) 0tez in
  ()
