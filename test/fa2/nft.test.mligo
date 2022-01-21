#import "../../lib/fa2/nft/NFT.mligo" "FA2_NFT"
#import "./balance_of_callback_contract.mligo" "Callback"
#import "./views_test_contract.mligo" "ViewsTest"

(* Tests for FA2 multi asset contract *)

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

let get_initial_storage (a, b, c : nat * nat * nat) = 
  let () = Test.reset_state 6n ([] : tez list) in

  let owner1 = Test.nth_bootstrap_account 0 in 
  let owner2 = Test.nth_bootstrap_account 1 in 
  let owner3 = Test.nth_bootstrap_account 2 in 

  let owners = [owner1; owner2; owner3] in

  let op1 = Test.nth_bootstrap_account 3 in
  let op2 = Test.nth_bootstrap_account 4 in
  let op3 = Test.nth_bootstrap_account 5 in

  let ops = [op1; op2; op3] in

    let ledger = Big_map.literal ([
    (1n, owner1);
    (2n, owner2);
    (3n, owner3);
  ])
  in

  let operators  = Big_map.literal ([
    ((owner1, op1), Set.literal [1n]);
    ((owner2, op1), Set.literal [2n]);
    ((owner3, op1), Set.literal [3n]);
    ((op1   , op3), Set.literal [1n]);
  ])
  in
  
  let token_info = (Map.empty: (string, bytes) map) in
  let token_metadata = (Big_map.literal [
    (1n, ({token_id=1n;token_info=(Map.empty : (string, bytes) map);} : FA2_NFT.TokenMetadata.data));
    (2n, ({token_id=2n;token_info=(Map.empty : (string, bytes) map);} : FA2_NFT.TokenMetadata.data));
    (3n, ({token_id=3n;token_info=(Map.empty : (string, bytes) map);} : FA2_NFT.TokenMetadata.data));
  ] : FA2_NFT.TokenMetadata.t) in

  let initial_storage = {
    ledger         = ledger;
    token_metadata = token_metadata;
    operators      = operators;
    token_ids      = [1n; 2n; 3n];
  } in

  initial_storage, owners, ops


let assert_balances 
  (contract_address : (FA2_NFT.parameter, FA2_NFT.storage) typed_address ) 
  (a, b, c : (address * nat) * (address * nat) * (address * nat)) = 
  let (owner1, token_id_1) = a in
  let (owner2, token_id_2) = b in
  let (owner3, token_id_3) = c in
  let storage = Test.get_storage contract_address in
  let ledger = storage.ledger in
  let () = match (Big_map.find_opt token_id_1 ledger) with
    Some amt -> assert (amt = owner1)
  | None -> failwith "incorret address" 
  in
  let () = match (Big_map.find_opt token_id_2 ledger) with
    Some amt ->  assert (amt = owner2)
  | None -> failwith "incorret address" 
  in
  let () = match (Big_map.find_opt token_id_3 ledger) with
    Some amt -> assert (amt = owner3)
  | None -> failwith "incorret address" 
  in
  ()

let assert_error (result : test_exec_result) (error : FA2_NFT.Errors.t) =
  match result with
    Success -> failwith "This test should fail"
  | Fail (Rejected (err, _))  -> assert (Test.michelson_equal err (Test.eval error))
  | Fail _ -> failwith "invalid test failure"

(* Transfer *)

(* 1. transfer successful *)
let test_atomic_tansfer_success =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
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
  let () = assert_balances t_addr ((owner2, 2n), (owner2, 2n), (owner3, 3n)) in
  ()

(* 2. transfer failure token undefined *)
let test_transfer_token_undefined = 
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
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
  assert_error result FA2_NFT.Errors.undefined_token

(* 3. transfer failure incorrect operator *)
let test_atomic_transfer_failure_not_operator = 
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
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
  assert_error result FA2_NFT.Errors.not_operator

(* 4. self transfer *)
let test_atomic_tansfer_success_zero_amount_and_self_transfer =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
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
  let () = assert_balances t_addr ((owner1, 1n), (owner2, 2n), (owner3, 3n)) in
  ()

(* 5. transfer failure transitive operators *)
let test_transfer_failure_transitive_operators = 
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
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
  assert_error result FA2_NFT.Errors.not_operator

(* Balance of *)

(* 6. empty balance of + callback with empty response *)
let test_empty_transfer_and_balance_of = 
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
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
  let initial_storage, owners, operators = get_initial_storage (10n, 5n, 10n) in
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
  assert_error result FA2_NFT.Errors.undefined_token

(* 8. duplicate balance_of requests *)
let test_balance_of_requests_with_duplicates = 
  let initial_storage, owners, operators = get_initial_storage (10n, 5n, 10n) in
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
    let initial_storage, owners, operators = get_initial_storage (10n, 5n, 10n) in
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
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
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
  assert_error result FA2_NFT.Errors.not_operator

(* 11. Add operator & do transfer - success *)
let test_update_operator_add_operator_and_transfer = 
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
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

(* Tests for views *)

(* Test get_balance view *)
let test_get_balance_view = 
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  
  let (c_addr,_,_) = Test.originate_from_file 
    "./lib/fa2/nft/NFT.mligo" 
    "main"
    (["get_balance"; "total_supply"; "is_operator"; "all_tokens"] : string list)
    (Test.eval initial_storage) 0tez in

  let initial_storage : ViewsTest.storage = {
    main_contract = c_addr;
    get_balance   = (None : nat option);
    total_supply  = (None : nat option);
    is_operator   = (None : bool option);
    all_tokens    = (None : nat list option);
  } in

  let (t_addr,_,_) = Test.originate ViewsTest.main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let () = Test.transfer_to_contract_exn contr 
    (Get_balance (owner1,1n) : ViewsTest.parameter) 0tez
  in
  let storage = Test.get_storage t_addr in
  let get_balance = storage.get_balance in
  assert (get_balance = Some 1n)

(* Test total_supply view *)
let test_total_supply_view = 
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  
  let (c_addr,_,_) = Test.originate_from_file 
    "./lib/fa2/nft/NFT.mligo" 
    "main"
    (["get_balance"; "total_supply"; "is_operator"; "all_tokens"] : string list)
    (Test.eval initial_storage) 0tez in

  let initial_storage : ViewsTest.storage = {
    main_contract = c_addr;
    get_balance   = (None : nat option);
    total_supply  = (None : nat option);
    is_operator   = (None : bool option);
    all_tokens    = (None : nat list option);
  } in

  let (t_addr,_,_) = Test.originate ViewsTest.main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let () = Test.transfer_to_contract_exn contr 
    (Total_supply 2n : ViewsTest.parameter) 0tez
  in
  let storage = Test.get_storage t_addr in
  let total_supply = storage.total_supply in
  assert (total_supply = Some 1n)

(* Test total_supply view - undefined token *)
let test_total_supply_undefined_token_view = 
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  
  let (c_addr,_,_) = Test.originate_from_file 
    "./lib/fa2/nft/NFT.mligo" 
    "main"
    (["get_balance"; "total_supply"; "is_operator"; "all_tokens"] : string list)
    (Test.eval initial_storage) 0tez in

  let initial_storage : ViewsTest.storage = {
    main_contract = c_addr;
    get_balance   = (None : nat option);
    total_supply  = (None : nat option);
    is_operator   = (None : bool option);
    all_tokens    = (None : nat list option);
  } in

  let (t_addr,_,_) = Test.originate ViewsTest.main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let result = Test.transfer_to_contract contr 
    (Total_supply 15n : ViewsTest.parameter) 0tez
  in
  assert_error result FA2_NFT.Errors.undefined_token

(* Test is_operator view *)
let test_is_operator_view = 
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let op1    = List_helper.nth_exn 0 operators in
  
  let (c_addr,_,_) = Test.originate_from_file 
    "./lib/fa2/nft/NFT.mligo" 
    "main"
    (["get_balance"; "total_supply"; "is_operator"; "all_tokens"] : string list)
    (Test.eval initial_storage) 0tez in

  let initial_storage : ViewsTest.storage = {
    main_contract = c_addr;
    get_balance   = (None : nat option);
    total_supply  = (None : nat option);
    is_operator   = (None : bool option);
    all_tokens    = (None : nat list option);
  } in

  let (t_addr,_,_) = Test.originate ViewsTest.main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let () = Test.transfer_to_contract_exn contr 
    (Is_operator {
      owner    = owner1;
      operator = op1;
      token_id = 1n;
    } : ViewsTest.parameter) 0tez
  in
  let storage = Test.get_storage t_addr in
  let is_operator = storage.is_operator in
  assert (is_operator = Some true)

(* Test all_tokens view *)
let test_all_tokens_view = 
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let op1    = List_helper.nth_exn 0 operators in
  
  let (c_addr,_,_) = Test.originate_from_file 
    "./lib/fa2/nft/NFT.mligo" 
    "main"
    (["get_balance"; "total_supply"; "is_operator"; "all_tokens"] : string list)
    (Test.eval initial_storage) 0tez in

  let initial_storage : ViewsTest.storage = {
    main_contract = c_addr;
    get_balance   = (None : nat option);
    total_supply  = (None : nat option);
    is_operator   = (None : bool option);
    all_tokens    = (None : nat list option);
  } in

  let (t_addr,_,_) = Test.originate ViewsTest.main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let () = Test.transfer_to_contract_exn contr 
    (All_tokens: ViewsTest.parameter) 0tez
  in
  let storage = Test.get_storage t_addr in
  let all_tokens = storage.all_tokens in
  assert (all_tokens = Some [1n; 2n; 3n])
