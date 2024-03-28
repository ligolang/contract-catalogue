(* An implementaion of FA2 Interface (TZIP-12) for NFT *)
[@public] #import "./fa2/nft/nft.impl.mligo" "NFT"
[@public] #import "./fa2/nft/extendable_nft.impl.mligo" "NFTExtendable"

(* An implementaion of FA2 Interface (TZIP-12) for a Single Asset Token *)
[@public] #import "./fa2/asset/single_asset.impl.mligo" "SingleAsset"
[@public] #import "./fa2/asset/extendable_single_asset.impl.mligo" "SingleAssetExtendable"

(* An implementaion of FA2 Interface (TZIP-12) for a Multi Asset Token *)
[@public] #import "./fa2/asset/multi_asset.impl.mligo" "MultiAsset"
[@public] #import "./fa2/asset/extendable_multi_asset.impl.mligo" "MultiAssetExtendable"
