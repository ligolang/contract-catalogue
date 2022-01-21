type storage = {
    main_contract : address;
    get_balance   : nat option;
    total_supply  : nat option;
    is_operator   : bool option;
}

type operator_request = [@layout:comb] {
    owner    : address;
    operator : address;
    token_id : nat; 
}

type parameter = [@layout:comb] 
    | Get_balance of (address * nat) 
    | Total_supply of nat 
    | Is_operator of operator_request

let get_balance ((p, s) : ((address * nat) * storage)) : storage = 
    let get_balance : nat option = Tezos.call_view "get_balance" p s.main_contract in
    {s with get_balance = get_balance}

let total_supply ((p, s) : (nat * storage)) : storage = 
    let total_supply : nat option = Tezos.call_view "total_supply" p s.main_contract in
    {s with total_supply = total_supply}

let is_operator ((p, s) : (operator_request * storage)) =
    let is_operator : bool option = Tezos.call_view "is_operator" p s.main_contract in
    {s with is_operator = is_operator}

let main ((p,s):(parameter * storage)) =
    let s = match p with
       Get_balance  p -> get_balance (p, s)
    |  Total_supply p -> total_supply (p, s)
    |  Is_operator  p -> is_operator (p, s)
    in
    ([]: operation list), s 