#import "../../helpers/nft_helpers.mligo" "TestHelpers"
#import "../../helpers/list.mligo" "List_helper"
#import "../../../lib/fa2/nft/nft.impl.mligo" "FA2_NFT"
#import "./views_test_contract.mligo" "ViewsTestContract"

(* Tests for views *)

type orig_nft = (FA2_NFT parameter_of, FA2_NFT.storage) origination_result

(* Test get_balance view *)
let test_get_balance_view =
  let initial_storage, owners, _ = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in

  let orig = Test.Next.Originate.contract (contract_of FA2_NFT) initial_storage 0tez in


  let initial_storage : ViewsTestContract.storage = {
    main_contract = Test.to_address orig.taddr;
    get_balance   = (None : nat option);
    total_supply  = (None : nat option);
    is_operator   = (None : bool option);
    all_tokens    = (None : nat set option);
  } in

  let orig_v = Test.Next.Originate.contract (contract_of ViewsTestContract) initial_storage 0tez in

  let _ = Test.Next.Typed_address.transfer_exn orig_v.addr
    (Get_balance (owner1,1n) : ViewsTestContract parameter_of) 0tez
  in
  let storage = Test.Next.Typed_address.get_storage orig_v.addr in
  let get_balance = storage.get_balance in
  Assert.assert (get_balance = Some 1n)

(* Test total_supply view *)
let test_total_supply_view =
  let initial_storage, _, _ = TestHelpers.get_initial_storage () in

  let orig = Test.Next.Originate.contract (contract_of FA2_NFT) initial_storage 0tez in


  let initial_storage : ViewsTestContract.storage = {
    main_contract = Test.to_address orig.taddr;
    get_balance   = (None : nat option);
    total_supply  = (None : nat option);
    is_operator   = (None : bool option);
    all_tokens    = (None : nat set option);
  } in

  let orig_v = Test.Next.Originate.contract (contract_of ViewsTestContract) initial_storage 0tez in

  let _ = Test.Next.Typed_address.transfer_exn orig_v.addr
    (Total_supply 2n : ViewsTestContract parameter_of) 0tez
  in
  let storage = Test.Next.Typed_address.get_storage orig_v.addr in
  let total_supply = storage.total_supply in
  Assert.assert (total_supply = Some 1n)


(* Test total_supply view - undefined token *)
let test_total_supply_undefined_token_view =
  let initial_storage, _, _ = TestHelpers.get_initial_storage () in

  let orig = Test.Next.Originate.contract (contract_of FA2_NFT) initial_storage 0tez in


  let initial_storage : ViewsTestContract.storage = {
    main_contract = Test.to_address orig.taddr;
    get_balance   = (None : nat option);
    total_supply  = (None : nat option);
    is_operator   = (None : bool option);
    all_tokens    = (None : nat set option);
  } in

  let orig_v = Test.Next.Originate.contract (contract_of ViewsTestContract) initial_storage 0tez in

  let result = Test.Next.Typed_address.transfer orig_v.taddr
    (Total_supply 15n : ViewsTestContract parameter_of) 0tez
  in
  TestHelpers.assert_error result FA2_NFT.Errors.undefined_token

(* Test is_operator view *)
let test_is_operator_view =
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let op1    = List_helper.nth_exn 0 operators in

  let orig = Test.Next.Originate.contract (contract_of FA2_NFT) initial_storage 0tez in


  let initial_storage : ViewsTestContract.storage = {
    main_contract = Test.to_address orig.taddr;
    get_balance   = (None : nat option);
    total_supply  = (None : nat option);
    is_operator   = (None : bool option);
    all_tokens    = (None : nat set option);
  } in

  let orig_v = Test.Next.Originate.contract (contract_of ViewsTestContract) initial_storage 0tez in

  let _ = Test.Next.Typed_address.transfer_exn orig_v.addr
    (Is_operator {
      owner    = owner1;
      operator = op1;
      token_id = 1n;
    } : ViewsTestContract parameter_of) 0tez
  in
  let storage = Test.Next.Typed_address.get_storage orig_v.addr in
  let is_operator = storage.is_operator in
  Assert.assert (is_operator = Some true)


