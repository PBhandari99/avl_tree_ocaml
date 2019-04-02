type ('k, 'v) avlnode =
  | Leaf
  | Node of int * 'k * 'v * ('k, 'v) avlnode * ('k, 'v) avlnode

(* Given a key return a value for that key *)
let rec get (n : ('k, 'v) avlnode) (key : 'k) : 'v option =
  match n with
  | Leaf -> None
  | Node (_, k, v, left, right) ->
      if k = key then Some v
      else if key < k then get left key
      else get right key

let height (n : ('k, 'v) avlnode) : int =
  match n with Leaf -> 0 | Node (h, _, _, _, _) -> h

(* Given a avlnode return it key *)
let get_key (n : ('k, 'v) avlnode) : 'k =
  match n with Node (_, k, _, _, _) -> k | _ -> failwith "cannot happen"

(* Balancing function start here
   *
   * balance_ll : Rotate the given node to the right.
   * example:
   *               *
   *              /                   *
   *             *      ----->       / \
   *            /                   *   *
   *           *
   *
   * *)
let balance_ll (n : ('k, 'v) avlnode) : ('k, 'v) avlnode =
  match n with
  | Node (h, k, v, Node (hl, kl, vl, ll, lr), right) ->
      let hr = max (height right) (height lr) + 1 in
      let hn = max hr (height ll) + 1 in
      Node (hn, kl, vl, ll, Node (hr, k, v, lr, right))
  | _ -> failwith "cannot happen"

(*  balance_rr : Rotate the given node to the left.
    *    example:
    *             *
    *              \                    *
    *               *     ----->       / \
    *                \                *   *
    *                 *
    *  *)
let balance_rr (n : ('k, 'v) avlnode) : ('k, 'v) avlnode =
  match n with
  | Node (h, k, v, left, Node (hr, rk, rv, rl, rr)) ->
      let hl = max (height left) (height rl) + 1 in
      let hn = max hl (height rr) + 1 in
      Node (hn, rk, rv, Node (hl, k, v, left, rl), rr)
  | _ -> failwith "cannot a happen"

(*
   * balance_rl : Rotate first to the right and to the left.
   *
   *      *                 *
   *       \                 \               *
   *        *      -->        *     -->     / \
   *       /                   \           *   *
   *      *                     *
   *)
let balance_rl (n : ('k, 'v) avlnode) : ('k, 'v) avlnode =
  match n with
  | Node (h, k, v, left, Node (hr, kr, vr, Node (hrl, krl, vrl, rll, rlr), rr))
    ->
      let hl = max (height left) (height rll) + 1 in
      let hr = max (height rr) (height rlr) + 1 in
      let hn = max hl hr + 1 in
      Node
        (hn, krl, vrl, Node (hl, k, v, left, rll), Node (hr, kr, vr, rlr, rr))
  | _ -> failwith "cannot happen"

(*
   * balance_lr : Rotate first to the left and to the right.
   *  example:
   *            *             *
   *           /             /             *
   *          *     -->     *     -->     / \
   *           \           /             *   *
   *            *         *
   *
   *)
let balance_lr (n : ('k, 'v) avlnode) : ('k, 'v) avlnode =
  match n with
  | Node (h, k, v, Node (hl, kl, vl, ll, Node (hlr, klr, vlr, lrl, lrr)), right)
    ->
      let hl = max (height ll) (height lrl) + 1 in
      let hr = max (height right) (height lrr) + 1 in
      let hn = max hr hl + 1 in
      Node
        (hn, klr, vlr, Node (hl, kl, vl, ll, lrl), Node (hr, k, v, lrr, right))
  | _ -> failwith "cannot happen"

(* Balancing functions end *)

(* Produce a new avltree that contains the given key. If the key already
   exists, update the value to the new value *)
let rec set (n : ('k, 'v) avlnode) (key : 'k) (value : 'v) : ('k, 'v) avlnode =
  match n with
  | Leaf -> Node (1, key, value, Leaf, Leaf)
  | Node (h, k, v, left, right) ->
      if k = key then Node (h, k, value, left, right)
      else if key < k then
        let left_node = set left key value in
        let h_l = height left_node in
        let h_r = height right in
        let bal = h_l - h_r in
        if bal <> 2 then Node (max h_l h_r + 1, k, v, left_node, right)
        else if key < get_key left_node then
          balance_ll (Node (h_l + 1, k, v, left_node, right))
        else balance_lr (Node (h_l + 1, k, v, left_node, right))
      else
        let right_node = set right key value in
        let h_r = height right_node in
        let h_l = height left in
        let bal = h_l - h_r in
        if bal <> -2 then Node (max h_l h_r + 1, k, v, left, right_node)
        else if key > get_key right_node then
          balance_rr (Node (h_r + 1, k, v, left, right_node))
        else balance_rl (Node (h_r + 1, k, v, left, right_node))

(* Return a list of tuples containing the elements of the tree *)
let rec inorder (n : ('k, 'v) avlnode) : ('k * 'v) list =
  match n with
  | Leaf -> []
  | Node (_, k, v, left, right) -> inorder left @ [(k, v)] @ inorder right

(* Return true if the given key exists in the tree *)
let contains (n : ('k, 'v) avlnode) (key : 'k) : bool =
  List.exists (fun elm -> fst elm = key) (inorder n)

(* Given an AVL tree and a list of key/value tuples, set all the given keys
(first pair components) to the corresponding value (second pair components) *)
let add_all (n : ('k, 'v) avlnode) (keys : ('k * 'v) list) : ('k, 'v) avlnode =
  List.fold_left (fun acc key -> set acc (fst key) (snd key)) n keys

(* Return the total value of all the integers in a tree that has int values *)
let sum (n : ('k, int) avlnode) : int =
  let ls = List.map (fun a -> match a with k, v -> v) (inorder n) in
  List.fold_left (fun acc x -> acc + x) 0 ls
