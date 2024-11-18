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


___


type key = int

type 'a btree =
  | Empty
  | Node of {
      left : 'a btree;
      kv : key * 'a;
      right : 'a btree;
      height : int;
    }

type lookup_error =
  | NoSuchKey of key

exception LookupError of lookup_error

(* Helper function to get the height of a tree *)
let height t =
  match t with
  | Empty -> 0
  | Node { height; _ } -> height

(* Helper function to calculate the balance factor of a tree *)
let balance_factor t =
  match t with
  | Empty -> 0
  | Node { left; right; _ } -> height left - height right

(* Create a new node with proper height *)
let make_node left kv right =
  let h = 1 + max (height left) (height right) in
  Node { left; kv; right; height = h }

(* Perform a right rotation *)
let rotate_right = function
  | Node { left = Node { left = a; kv = x; right = b; _ }; kv = y; right = c; _ } ->
      make_node a x (make_node b y c)
  | t -> t (* No rotation needed *)

(* Perform a left rotation *)
let rotate_left = function
  | Node { left = a; kv = x; right = Node { left = b; kv = y; right = c; _ }; _ } ->
      make_node (make_node a x b) y c
  | t -> t (* No rotation needed *)

(* Balance the tree *)
let balance t =
  match t with
  | Empty -> Empty
  | Node { left; kv; right; _ } as node ->
      let bf = balance_factor node in
      if bf > 1 then
        if balance_factor left >= 0 then rotate_right node (* Left-left case *)
        else rotate_right (make_node (rotate_left left) kv right) (* Left-right case *)
      else if bf < -1 then
        if balance_factor right <= 0 then rotate_left node (* Right-right case *)
        else rotate_left (make_node left kv (rotate_right right)) (* Right-left case *)
      else node (* Already balanced *)

(* Insert a key-value pair into the AVL tree *)
let rec insert t k v =
  match t with
  | Empty -> make_node Empty (k, v) Empty
  | Node { left; kv = (k', v'); right; _ } ->
      if k < k' then balance (make_node (insert left k v) (k', v') right)
      else if k > k' then balance (make_node left (k', v') (insert right k v))
      else make_node left (k, v) right (* Update value for the same key *)

(* Find a key in the AVL tree *)
let rec find t k =
  match t with
  | Empty -> raise (LookupError (NoSuchKey k))
  | Node { left; kv = (k', v); right; _ } ->
      if k < k' then find left k
      else if k > k' then find right k
      else v

(* Delete a key from the AVL tree *)
let rec delete t k =
  match t with
  | Empty -> Empty (* Key not found *)
  | Node { left; kv = (k', v'); right; _ } ->
      if k < k' then balance (make_node (delete left k) (k', v') right)
      else if k > k' then balance (make_node left (k', v') (delete right k))
      else (* Key found *)
        match left, right with
        | Empty, _ -> right (* Replace with right subtree *)
        | _, Empty -> left (* Replace with left subtree *)
        | _ ->
            (* Replace with the in-order successor *)
            let rec find_and_remove_min t =
              match t with
              | Empty -> failwith "Unexpected empty tree"
              | Node { left = Empty; kv; right; _ } -> kv, right
              | Node { left; kv; right; _ } ->
                  let min_kv, new_left = find_and_remove_min left in
                  min_kv, balance (make_node new_left kv right)
            in
            let successor, new_right = find_and_remove_min right in
            balance (make_node left successor new_right)

(* Print the tree for debugging *)
let rec print_tree t indent =
  match t with
  | Empty -> Printf.printf "%sEmpty\n" indent
  | Node { left; kv = (k, v); right; height } ->
      Printf.printf "%sNode(key: %d, val: %s, height: %d)\n" indent k v height;
      print_tree left (indent ^ "  ");
      print_tree right (indent ^ "  ")

(* Test the AVL tree *)
let () =
  let tree = Empty in
  let tree = insert tree 10 "ten" in
  let tree = insert tree 20 "twenty" in
  let tree = insert tree 5 "five" in
  let tree = insert tree 15 "fifteen" in
  let tree = insert tree 30 "thirty" in
  let tree = insert tree 25 "twenty-five" in
  Printf.printf "AVL Tree after insertion:\n";
  print_tree tree "";

  (* Test find *)
  Printf.printf "\nFind key 15: %s\n" (find tree 15);
  (try
     Printf.printf "Find key 100: %s\n" (find tree 100)
   with
   | LookupError (NoSuchKey k) -> Printf.printf "Key %d not found\n" k);

  (* Test deletion *)
  let tree = delete tree 20 in
  Printf.printf "\nAVL Tree after deleting key 20:\n";
  print_tree tree "";
