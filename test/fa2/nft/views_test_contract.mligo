type storage = {
    main_contract : address;
    get_balance   : nat option;
    total_supply  : nat option;
    is_operator   : bool option;
    all_tokens    : nat set option;
}

type operator_request = [@layout:comb] {
    owner    : address;
    operator : address;
    token_id : nat; 
}

[@entry] let get_balance (p : address * nat) (s: storage) : operation list * storage = 
    let get_balance : nat option = Tezos.call_view "get_balance" p s.main_contract in
    [],{s with get_balance = get_balance}

[@entry] let total_supply (p : nat) (s : storage) : operation list * storage = 
    let total_supply : nat option = Tezos.call_view "total_supply" p s.main_contract in
    [],{s with total_supply = total_supply}

[@entry] let is_operator (p : operator_request) (s : storage) : operation list * storage =
    let is_operator : bool option = Tezos.call_view "is_operator" p s.main_contract in
    [],{s with is_operator = is_operator}

[@entry] let all_tokens () (s : storage) : operation list * storage =
    let all_tokens : nat set option = Tezos.call_view "all_tokens" () s.main_contract in
    [],{s with all_tokens = all_tokens}
