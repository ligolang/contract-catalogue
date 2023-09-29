#import "../../lib/generic-fa2/single_asset/fa2.mligo" "FA2"

type storage = FA2.storage
type extension = string
type extended_storage = extension storage
type 'p ret = 'p -> extended_storage -> operation list * extended_storage 

[@entry] let transfer : FA2.transfer ret = FA2.transfer
[@entry] let balance_of : FA2.balance_of ret = FA2.balance_of
[@entry] let update_operators : FA2.update_operators ret = FA2.update_operators