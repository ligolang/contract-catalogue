[@public] #import "../common/assertions.jsligo" "Assertions"
[@public] #import "../common/errors.mligo" "Errors"
[@public] #import "../common/tzip12.datatypes.jsligo" "TZIP12"
[@public] #import "../common/tzip12.interfaces.jsligo" "TZIP12Interface"
[@public] #import "../common/tzip16.datatypes.jsligo" "TZIP16"
#import "./extendable_nft.impl.mligo" "NFTExtendable"

type ledger = NFTExtendable.ledger

type operator = NFTExtendable.operator

type operators = NFTExtendable.operators

type storage =
  {
   ledger : ledger;
   operators : operators;
   token_metadata : TZIP12.tokenMetadata;
   metadata : TZIP16.metadata
  }

type ret = operation list * storage

let empty_storage : storage =
  {
   ledger = Big_map.empty;
   operators = Big_map.empty;
   token_metadata = Big_map.empty;
   metadata = Big_map.empty
  }

[@inline]
let lift (s : storage) : unit NFTExtendable.storage =
  {
   ledger = s.ledger;
   operators = s.operators;
   token_metadata = s.token_metadata;
   metadata = s.metadata;
   extension = ()
  }

[@inline]
let unlift (ret : operation list * unit NFTExtendable.storage) : ret =
  let ops, s = ret in
  ops,
  {
   ledger = s.ledger;
   operators = s.operators;
   token_metadata = s.token_metadata;
   metadata = s.metadata
  }

[@entry]
let transfer (t : TZIP12.transfer) (s : storage) : ret =
  unlift (NFTExtendable.transfer t (lift s))

[@entry]
let balance_of (b : TZIP12.balance_of) (s : storage) : ret =
  unlift (NFTExtendable.balance_of b (lift s))

[@entry]
let update_operators (updates : TZIP12.update_operators) (s : storage) : ret =
  unlift (NFTExtendable.update_operators updates (lift s))

[@view]
let get_balance (p : (address * nat)) (s : storage) : nat =
  NFTExtendable.get_balance p (lift s)

[@view]
let total_supply (token_id : nat) (s : storage) : nat =
  NFTExtendable.total_supply token_id (lift s)

[@view]
let all_tokens (_ : unit) (s : storage) : nat set =
  NFTExtendable.all_tokens () (lift s)

[@view]
let is_operator (op : TZIP12.operator) (s : storage) : bool =
  NFTExtendable.is_operator op (lift s)

[@view]
let token_metadata (p : nat) (s : storage) : TZIP12.tokenMetadataData =
  NFTExtendable.token_metadata p (lift s)
