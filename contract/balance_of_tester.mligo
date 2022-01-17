type storage = int

type parameter = Response of int

let main ((p,s):(parameter * storage)) =
  match p with
   Response i -> ([]: operation list), i