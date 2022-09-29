(* An implementaion of FA2 Interface (TZIP-12) for NFT *)
#import "./nft/NFT.mligo" "NFT"

(* An implementaion of FA2 Interface (TZIP-12) for a Single Asset Token *)
#import "./asset/single_asset.mligo" "SingleAsset"

(* An implementaion of FA2 Interface (TZIP-12) for a Multi Asset Token *)
#import "./asset/multi_asset.mligo" "MultiAsset"

let main (_,_ : unit * (address option)) : operation list * (address option) =
    let (op, addr) = Tezos.create_contract NFT.main (None) 1tez ({
    ledger         = Big_map.empty;
    token_metadata = Big_map.empty;
    operators      = Big_map.empty;
    token_ids      = [1n; 2n; 3n];
    } : NFT.storage) in
    [op], Some addr 
