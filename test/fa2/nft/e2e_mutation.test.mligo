#import "../../../lib/fa2/nft/nft.impl.mligo" "FA2_NFT"
#import "./nft.test.mligo" "Test_FA2_NFT"

let originate_and_test_e2e contract =
  let () = Test_FA2_NFT._test_atomic_transfer_operator_success contract in
  let () = Test_FA2_NFT._test_atomic_transfer_owner_success contract in
  let () = Test_FA2_NFT._test_transfer_token_undefined contract in
  let () = Test_FA2_NFT._test_atomic_transfer_failure_not_operator contract in
  let () =
    Test_FA2_NFT._test_atomic_transfer_success_zero_amount_and_self_transfer
      contract in
  let () = Test_FA2_NFT._test_transfer_failure_transitive_operators contract in
  let () = Test_FA2_NFT._test_empty_transfer_and_balance_of contract in
  let () = Test_FA2_NFT._test_balance_of_token_undefines contract in
  let () = Test_FA2_NFT._test_balance_of_requests_with_duplicates contract in
  let () =
    Test_FA2_NFT._test_balance_of_0_balance_if_address_does_not_hold_tokens
      contract in
  let () =
    Test_FA2_NFT._test_update_operator_remove_operator_and_transfer contract in
  let () = Test_FA2_NFT._test_update_operator_add_operator_and_transfer contract in
  let () = Test_FA2_NFT._test_only_sender_manage_operators contract in
  let () =
    Test_FA2_NFT._test_update_operator_remove_operator_and_transfer1 contract in
  let () =
    Test_FA2_NFT._test_update_operator_add_operator_and_transfer1 contract in
  ()

let test_mutation =
  match Test.mutation_test_all (contract_of  FA2_NFT.NFT) originate_and_test_e2e
  with
    [] -> ()
  | ms ->
      let () =
        List.iter
          (fun ((_, mutation) : unit * mutation) -> let () = Test.log mutation in
             ())
          ms in
      Test.failwith "Some mutation also passes the tests! ^^"
