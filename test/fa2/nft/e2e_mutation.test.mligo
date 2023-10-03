#import "../../../lib/fa2/nft/nft.impl.mligo" "FA2_NFT"
#import "./nft.test.mligo" "Test_FA2_NFT"
 
let originate_and_test_e2e (main : Test_FA2_NFT.main_fn) =
  let () = Test_FA2_NFT._test_atomic_tansfer_operator_success main in
  let () = Test_FA2_NFT._test_atomic_tansfer_owner_success main in
  let () = Test_FA2_NFT._test_transfer_token_undefined main in
  let () = Test_FA2_NFT._test_atomic_transfer_failure_not_operator main in
  let () = Test_FA2_NFT._test_atomic_tansfer_success_zero_amount_and_self_transfer main in
  let () = Test_FA2_NFT._test_transfer_failure_transitive_operators main in
  let () = Test_FA2_NFT._test_empty_transfer_and_balance_of main in
  let () = Test_FA2_NFT._test_balance_of_token_undefines main in
  let () = Test_FA2_NFT._test_balance_of_requests_with_duplicates main in
  let () = Test_FA2_NFT._test_balance_of_0_balance_if_address_does_not_hold_tokens main in
  let () = Test_FA2_NFT._test_update_operator_remove_operator_and_transfer main in
  let () = Test_FA2_NFT._test_update_operator_add_operator_and_transfer main in
  let () = Test_FA2_NFT._test_only_sender_manage_operators main in
  let () = Test_FA2_NFT._test_update_operator_remove_operator_and_transfer1 main in
  let () = Test_FA2_NFT._test_update_operator_add_operator_and_transfer1 main in
  ()

let test_mutation =
  match Test.mutation_test_all FA2_NFT.main originate_and_test_e2e with
    [] -> ()
  | ms ->
    let () = List.iter 
      (fun ((_, mutation) : unit * mutation) ->
        let () = Test.log mutation in 
        ()
      ) ms in
    Test.failwith "Some mutation also passes the tests! ^^"