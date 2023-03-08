(* An implementaion of FA2 Interface (TZIP-12) for NFT *)
#import "./nft/NFT.mligo" "NFT"

(* An implementaion of FA2 Interface (TZIP-12) for a Single Asset Token *)
#import "./asset/single_asset.mligo" "SingleAsset"

(* An implementaion of FA2 Interface (TZIP-12) for a Multi Asset Token *)
#import "./asset/multi_asset.mligo" "MultiAsset"

(* An implementaion of Generic FA2 single asset *)
#import "single_asset/fa2.mligo" "SingleAsset"

(* An implementaion of Generic FA2 multi asset *)
#import "multi_asset/fa2.mligo" "MultiAsset"
