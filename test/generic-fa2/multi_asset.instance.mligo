#import "../../lib/generic-fa2/multi_asset/fa2.mligo" "FA2"

type storage = FA2.storage
type extension = string
type extended_storage = extension storage

type parameter = [@layout:comb]
    | Transfer of FA2.transfer
    | Balance_of of FA2.balance_of
    | Update_operators of FA2.update_operators

let main (p : parameter) (s : extended_storage) : operation list * extended_storage 
= match p with
     Transfer         p -> FA2.transfer   p s
  |  Balance_of       p -> FA2.balance_of p s
  |  Update_operators p -> FA2.update_ops p s
