#import "../lib/main.mligo" "FA2"

module NFT = FA2.NFTExtendable

type extension = {
  admin: address
}

type storage = extension NFT.storage
type ret = operation list * storage

(* Extension *)

type mint = {
   owner    : address;
   token_id : nat;
}

[@entry]
let mint (mint : mint) (s : storage): ret =
  let sender = Tezos.get_sender () in
  let () = assert (sender = s.extension.admin) in
  let () = NFT.Assertions.assert_token_exist s.token_metadata mint.token_id in
  (* Check that nobody owns the token already *)
  let () = assert (Option.is_none (Big_map.find_opt mint.token_id s.ledger)) in
  let s = NFT.set_balance s mint.owner mint.token_id in
  [], s

(* Standard FA2 interface, copied from the source *)

[@entry]
let transfer (t: NFT.TZIP12.transfer) (s: storage) : ret =
  NFT.transfer t s

[@entry]
let balance_of (b: NFT.TZIP12.balance_of) (s: storage) : ret =
  NFT.balance_of b s

[@entry]
let update_operators (u: NFT.TZIP12.update_operators) (s: storage) : ret =
  NFT.update_operators  u s

[@view]
let get_balance (p : (address * nat)) (s : storage) : nat =
  NFT.get_balance p s

[@view]
let total_supply (token_id : nat) (s : storage) : nat =
  NFT.total_supply token_id s

[@view]
let all_tokens (_ : unit) (s : storage) : nat set =
  NFT.all_tokens () s

[@view]
let is_operator (op : NFT.TZIP12.operator) (s : storage) : bool =
  NFT.is_operator op s

[@view]
let token_metadata (p : nat) (s : storage) : NFT.TZIP12.tokenMetadataData =
  NFT.token_metadata p s

