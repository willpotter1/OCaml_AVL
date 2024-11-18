type key = int

type ’a btree =
	| Empty
	| Node of ’a btree * ( key * ’a ) * ’a btree

type lookup_error =
  | BoundExceeded
  | NoSuchKey of key

exception LookupError of lookup_error

let rec findEx'' t k n = 
  match t with
  | _ when n < 0 -> lookup_error BoundExceeded
  | Empty -> lookup_error (NoSuchKey k)
  | Node (l, (k', _), _) when k < k' -> findEx' l k (n - 1)
  | Node (_, (k', _), r) when k > k' -> findEx' r k (n - 1)
  | Node (_, (_,  x), _) -> x

  

