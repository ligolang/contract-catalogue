#import "../../lib/fa2/nft/NFT.mligo" "FA2_NFT"

let get_initial_storage (a, b, c : nat * nat * nat) = 
  let () = Test.reset_state 7n ([] : tez list) in

  let owner1 = Test.nth_bootstrap_account 0 in 
  let owner2 = Test.nth_bootstrap_account 1 in 
  let owner3 = Test.nth_bootstrap_account 2 in 
  let owner4 = Test.nth_bootstrap_account 6 in 

  let owners = [owner1; owner2; owner3; owner4] in

  let op1 = Test.nth_bootstrap_account 3 in
  let op2 = Test.nth_bootstrap_account 4 in
  let op3 = Test.nth_bootstrap_account 5 in

  let ops = [op1; op2; op3] in

    let ledger = Big_map.literal ([
    (1n, owner1);
    (2n, owner2);
    (3n, owner3);
    (4n, owner4);
    (5n, owner4);
  ])
  in

  let operators  = Big_map.literal ([
    ((owner1, op1), Set.literal [1n]);
    ((owner2, op1), Set.literal [2n]);
    ((owner3, op1), Set.literal [3n]);
    ((op1   , op3), Set.literal [1n]);
    ((owner4, op1), Set.literal [4n; 5n]);
  ])
  in
  
  let token_info = (Map.empty: (string, bytes) map) in
  let token_metadata = (Big_map.literal [
    (1n, ({token_id=1n;token_info=(Map.empty : (string, bytes) map);} : FA2_NFT.TokenMetadata.data));
    (2n, ({token_id=2n;token_info=(Map.empty : (string, bytes) map);} : FA2_NFT.TokenMetadata.data));
    (3n, ({token_id=3n;token_info=(Map.empty : (string, bytes) map);} : FA2_NFT.TokenMetadata.data));
    (4n, ({token_id=3n;token_info=(Map.empty : (string, bytes) map);} : FA2_NFT.TokenMetadata.data));
    (5n, ({token_id=3n;token_info=(Map.empty : (string, bytes) map);} : FA2_NFT.TokenMetadata.data));
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
    Success _ -> failwith "This test should fail"
  | Fail (Rejected (err, _))  -> assert (Test.michelson_equal err (Test.eval error))
  | Fail _ -> failwith "invalid test failure"
