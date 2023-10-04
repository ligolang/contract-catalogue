#import "../../helpers/nft_helpers.mligo" "TestHelpers"
#import "../../helpers/list.mligo" "List_helper"
#import "../../../lib/fa2/nft/nft.impl.mligo" "FA2_NFT"
#import "./views_test_contract.mligo" "ViewsTestContract"

(* Tests for views *)

type orig_nft = (FA2_NFT.NFT parameter_of, FA2_NFT.NFT.storage) origination_result

(* Test get_balance view *)
let test_get_balance_view =
  let initial_storage, owners, _ = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in

  let orig : orig_nft = Test.originate_from_file
    "../../../lib/fa2/nft/nft.impl.mligo"
    initial_storage 0tez in

  let initial_storage : ViewsTestContract.storage = {
    main_contract = Test.to_address orig.addr;
    get_balance   = (None : nat option);
    total_supply  = (None : nat option);
    is_operator   = (None : bool option);
    all_tokens    = (None : nat set option);
  } in

  let orig_v = Test.originate (contract_of ViewsTestContract) initial_storage 0tez in
  
  let _ = Test.transfer_exn orig_v.addr
    (Get_balance (owner1,1n) : ViewsTestContract parameter_of) 0tez
  in
  let storage = Test.get_storage orig_v.addr in
  let get_balance = storage.get_balance in
  assert (get_balance = Some 1n)

(* Test total_supply view *)
let test_total_supply_view =
  let initial_storage, _, _ = TestHelpers.get_initial_storage () in

  let orig : orig_nft = Test.originate_from_file
    "../../../lib/fa2/nft/nft.impl.mligo"
    initial_storage 0tez in

  let initial_storage : ViewsTestContract.storage = {
    main_contract = Test.to_address orig.addr;
    get_balance   = (None : nat option);
    total_supply  = (None : nat option);
    is_operator   = (None : bool option);
    all_tokens    = (None : nat set option);
  } in

  let orig_v = Test.originate (contract_of ViewsTestContract) initial_storage 0tez in
  
  let _ = Test.transfer_exn orig_v.addr
    (Total_supply 2n : ViewsTestContract parameter_of) 0tez
  in
  let storage = Test.get_storage orig_v.addr in
  let total_supply = storage.total_supply in
  assert (total_supply = Some 1n)


(* Test total_supply view - undefined token *)
let test_total_supply_undefined_token_view =
  let initial_storage, _, _ = TestHelpers.get_initial_storage () in

  let orig : orig_nft = Test.originate_from_file
    "../../../lib/fa2/nft/nft.impl.mligo"
    initial_storage 0tez in

  let initial_storage : ViewsTestContract.storage = {
    main_contract = Test.to_address orig.addr;
    get_balance   = (None : nat option);
    total_supply  = (None : nat option);
    is_operator   = (None : bool option);
    all_tokens    = (None : nat set option);
  } in

  let orig_v = Test.originate (contract_of ViewsTestContract) initial_storage 0tez in
  
  let result = Test.transfer orig_v.addr
    (Total_supply 15n : ViewsTestContract parameter_of) 0tez
  in
  TestHelpers.assert_error result FA2_NFT.Errors.undefined_token

(* Test is_operator view *)
let test_is_operator_view =
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let op1    = List_helper.nth_exn 0 operators in

  let orig : orig_nft = Test.originate_from_file
    "../../../lib/fa2/nft/nft.impl.mligo"
    initial_storage 0tez in

  let initial_storage : ViewsTestContract.storage = {
    main_contract = Test.to_address orig.addr;
    get_balance   = (None : nat option);
    total_supply  = (None : nat option);
    is_operator   = (None : bool option);
    all_tokens    = (None : nat set option);
  } in

  let orig_v = Test.originate (contract_of ViewsTestContract) initial_storage 0tez in
  
  let _ = Test.transfer_exn orig_v.addr
    (Is_operator {
      owner    = owner1;
      operator = op1;
      token_id = 1n;
    } : ViewsTestContract parameter_of) 0tez
  in
  let storage = Test.get_storage orig_v.addr in
  let is_operator = storage.is_operator in
  assert (is_operator = Some true)

(* Test all_tokens view *)
let test_all_tokens_view =
  let initial_storage, _, _ = TestHelpers.get_initial_storage () in

  let orig : orig_nft = Test.originate_from_file
    "../../../lib/fa2/nft/nft.impl.mligo"
    initial_storage 0tez in

  let initial_storage : ViewsTestContract.storage = {
    main_contract = Test.to_address orig.addr;
    get_balance   = (None : nat option);
    total_supply  = (None : nat option);
    is_operator   = (None : bool option);
    all_tokens    = (None : nat set option);
  } in

  let orig_v = Test.originate (contract_of ViewsTestContract) initial_storage 0tez in
  
  let _ = Test.transfer_exn orig_v.addr
    (All_tokens: ViewsTestContract parameter_of) 0tez
  in
  let storage = Test.get_storage orig_v.addr in
  let all_tokens = storage.all_tokens in
  let expected_tokens = Set.literal [1n; 2n; 3n] in
  assert (all_tokens = Some expected_tokens)
