type t = string

[@no_mutation] let undefined_token = "FA2_TOKEN_UNDEFINED"
[@no_mutation] let ins_balance     = "FA2_INSUFFICIENT_BALANCE"
[@no_mutation] let no_transfer     = "FA2_TX_DENIED"
[@no_mutation] let not_owner       = "FA2_NOT_OWNER"
[@no_mutation] let not_operator    = "FA2_NOT_OPERATOR"
[@no_mutation] let not_supported   = "FA2_OPERATORS_UNSUPPORTED"
[@no_mutation] let rec_hook_fail   = "FA2_RECEIVER_HOOK_FAILED"
[@no_mutation] let send_hook_fail  = "FA2_SENDER_HOOK_FAILED"
[@no_mutation] let rec_hook_undef  = "FA2_RECEIVER_HOOK_UNDEFINED"
[@no_mutation] let send_hook_under = "FA2_SENDER_HOOK_UNDEFINED"
[@no_mutation] let wrong_amount    = "WRONG_AMOUNT"


[@no_mutation] let only_sender_manage_operators = "The sender can only manage operators for his own token"
