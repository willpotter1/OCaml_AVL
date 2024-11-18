type key = int

type ’a btree =
	| Empty
	| Node of ’a btree * ( key * ’a ) * ’a btree

type lookup_error =
  | NoSuchKey of key

exception LookupError of lookup_error

let rec find t k = match t with 
  | Empty -> raise lookup_error (NoSuchKey k) (* raise instead of using None *)
  | Node (l, (k', d), r) when k < k' -> find l k
  | Node (_, (k', _), r) when k > k' -> find r k
  | Node (_, (_,  x), _) -> x (* no need to use Some now *)


