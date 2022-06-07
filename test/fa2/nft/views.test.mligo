#import "../../helpers/nft_helpers.mligo" "TestHelpers"
#import "../../helpers/list.mligo" "List_helper"
#import "../../../lib/fa2/nft/NFT.mligo" "FA2_NFT"
#import "./views_test_contract.mligo" "ViewsTestContract"

(* Tests for views *)

(* Test get_balance view *)
let test_get_balance_view = 
  let initial_storage, owners, _ = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  
  let (c_addr,_,_) = Test.originate_from_file 
    "./lib/fa2/nft/NFT.mligo" 
    "main"
    (["get_balance"; "total_supply"; "is_operator"; "all_tokens"; "token_metadata"] : string list)
    (Test.eval initial_storage) 0tez in

  let initial_storage : ViewsTestContract.storage = {
    main_contract = c_addr;
    get_balance   = (None : nat option);
    total_supply  = (None : nat option);
    is_operator   = (None : bool option);
    all_tokens    = (None : nat list option);
  } in

  let (t_addr,_,_) = Test.originate ViewsTestContract.main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let _ = Test.transfer_to_contract_exn contr 
    (Get_balance (owner1,1n) : ViewsTestContract.parameter) 0tez
  in
  let storage = Test.get_storage t_addr in
  let get_balance = storage.get_balance in
  assert (get_balance = Some 1n)

(* Test total_supply view *)
let test_total_supply_view = 
  let initial_storage, _, _ = TestHelpers.get_initial_storage () in
  
  let (c_addr,_,_) = Test.originate_from_file 
    "./lib/fa2/nft/NFT.mligo" 
    "main"
    (["get_balance"; "total_supply"; "is_operator"; "all_tokens"; "token_metadata"] : string list)
    (Test.eval initial_storage) 0tez in

  let initial_storage : ViewsTestContract.storage = {
    main_contract = c_addr;
    get_balance   = (None : nat option);
    total_supply  = (None : nat option);
    is_operator   = (None : bool option);
    all_tokens    = (None : nat list option);
  } in

  let (t_addr,_,_) = Test.originate ViewsTestContract.main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let _ = Test.transfer_to_contract_exn contr 
    (Total_supply 2n : ViewsTestContract.parameter) 0tez
  in
  let storage = Test.get_storage t_addr in
  let total_supply = storage.total_supply in
  assert (total_supply = Some 1n)


(* Test total_supply view - undefined token *)
let test_total_supply_undefined_token_view = 
  let initial_storage, _, _ = TestHelpers.get_initial_storage () in
  
  let (c_addr,_,_) = Test.originate_from_file 
    "./lib/fa2/nft/NFT.mligo" 
    "main"
    (["get_balance"; "total_supply"; "is_operator"; "all_tokens"; "token_metadata"] : string list)
    (Test.eval initial_storage) 0tez in

  let initial_storage : ViewsTestContract.storage = {
    main_contract = c_addr;
    get_balance   = (None : nat option);
    total_supply  = (None : nat option);
    is_operator   = (None : bool option);
    all_tokens    = (None : nat list option);
  } in

  let (t_addr,_,_) = Test.originate ViewsTestContract.main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let result = Test.transfer_to_contract contr 
    (Total_supply 15n : ViewsTestContract.parameter) 0tez
  in
  TestHelpers.assert_error result FA2_NFT.Errors.undefined_token

(* Test is_operator view *)
let test_is_operator_view = 
  let initial_storage, owners, operators = TestHelpers.get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let op1    = List_helper.nth_exn 0 operators in
  
  let (c_addr,_,_) = Test.originate_from_file 
    "./lib/fa2/nft/NFT.mligo" 
    "main"
    (["get_balance"; "total_supply"; "is_operator"; "all_tokens"; "token_metadata"] : string list)
    (Test.eval initial_storage) 0tez in

  let initial_storage : ViewsTestContract.storage = {
    main_contract = c_addr;
    get_balance   = (None : nat option);
    total_supply  = (None : nat option);
    is_operator   = (None : bool option);
    all_tokens    = (None : nat list option);
  } in

  let (t_addr,_,_) = Test.originate ViewsTestContract.main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let _ = Test.transfer_to_contract_exn contr 
    (Is_operator {
      owner    = owner1;
      operator = op1;
      token_id = 1n;
    } : ViewsTestContract.parameter) 0tez
  in
  let storage = Test.get_storage t_addr in
  let is_operator = storage.is_operator in
  assert (is_operator = Some true)

(* Test all_tokens view *)
let test_all_tokens_view = 
  let initial_storage, _, _ = TestHelpers.get_initial_storage () in
  
  let (c_addr,_,_) = Test.originate_from_file 
    "./lib/fa2/nft/NFT.mligo" 
    "main"
    (["get_balance"; "total_supply"; "is_operator"; "all_tokens"; "token_metadata"] : string list)
    (Test.eval initial_storage) 0tez in

  let initial_storage : ViewsTestContract.storage = {
    main_contract = c_addr;
    get_balance   = (None : nat option);
    total_supply  = (None : nat option);
    is_operator   = (None : bool option);
    all_tokens    = (None : nat list option);
  } in

  let (t_addr,_,_) = Test.originate ViewsTestContract.main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let _ = Test.transfer_to_contract_exn contr 
    (All_tokens: ViewsTestContract.parameter) 0tez
  in
  let storage = Test.get_storage t_addr in
  let all_tokens = storage.all_tokens in
  assert (all_tokens = Some [1n; 2n; 3n])
