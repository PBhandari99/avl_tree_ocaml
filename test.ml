open OUnit2
open ExtLib
open Avl
open Printf

(* A helper for testing primitive values (won't print datatypes well) *)
let t_any name value expected =
  name >:: fun _ -> assert_equal expected value ~printer:dump

let tuple_to_string (ls : ('k * 'v) list) : string =
  let str =
    List.fold_left
      (fun acc x -> acc ^ "(" ^ fst x ^ ", " ^ string_of_int (snd x) ^ "); ")
      "[" ls
  in
  str ^ "]"

let t_inorder name value expected =
  name >:: fun _ -> assert_equal expected value ~printer:tuple_to_string

let a_tree = Node (1, "a", 5, Leaf, Leaf)

(* this tree has structure:
 *                          4
 *                        /   \
 *                       2     6
 *                     /  \   /  \
 *                    1    3 5    7
 *)
let b_tree =
  Node
    ( 3
    , "d"
    , 4
    , Node
        (2, "b", 2, Node (1, "a", 1, Leaf, Leaf), Node (1, "c", 3, Leaf, Leaf))
    , Node
        (2, "f", 6, Node (1, "e", 5, Leaf, Leaf), Node (1, "g", 7, Leaf, Leaf))
    )

let key_value_list = [("c", 3); ("h", 8); ("l", 12)]

let b_tree_key_value_list =
  [("a", 1); ("b", 2); ("c", 3); ("d", 4); ("e", 5); ("f", 6); ("g", 7)]

let s_tree =
  Node (2, "b", 2, Node (1, "a", 1, Leaf, Leaf), Node (1, "c", 3, Leaf, Leaf))

let s_tree_key_value_list = [("a", 1); ("b", 2); ("c", 3)]

(*
 *                     4
 *                    /
 *                   2
 *                  /
 *                 1
 *  *)
let ll_tree =
  Node (3, "d", 4, Node (2, "b", 2, Node (1, "a", 1, Leaf, Leaf), Leaf), Leaf)

let ll_tree_balanced =
  Node (2, "b", 2, Node (1, "a", 1, Leaf, Leaf), Node (1, "d", 4, Leaf, Leaf))

(*
 *                     4
 *                    /
 *                   2
 *                    \
 *                     3
 *
 *  *)
let lr_tree =
  Node (3, "d", 4, Node (2, "b", 2, Leaf, Node (1, "c", 3, Leaf, Leaf)), Leaf)

let lr_tree_balanced =
  Node (2, "c", 3, Node (1, "b", 2, Leaf, Leaf), Node (1, "d", 4, Leaf, Leaf))

(*
 *               4
 *                \
 *                 6
 *                  \
 *                   7
 *  *)
let rr_tree =
  Node (3, "d", 4, Leaf, Node (2, "f", 6, Leaf, Node (1, "g", 7, Leaf, Leaf)))

let rr_tree_balanced =
  Node (2, "f", 6, Node (1, "d", 4, Leaf, Leaf), Node (1, "g", 7, Leaf, Leaf))

(*
 *               4
 *                \
 *                 6
 *                /
 *               5
 *  *)
let rl_tree =
  Node (3, "d", 4, Leaf, Node (2, "f", 6, Node (1, "e", 5, Leaf, Leaf), Leaf))

let rl_tree_balanced =
  Node (2, "e", 5, Node (1, "d", 4, Leaf, Leaf), Node (1, "f", 6, Leaf, Leaf))

(* It can be useful to aggregate tests into lists if they test separate
functions, and put them together at the end *)

let add_all_test =
  [ t_any "add_all1" (add_all Leaf b_tree_key_value_list) b_tree
  ; t_any "add_all2" (add_all Leaf s_tree_key_value_list) s_tree ]

let get_tests =
  [ t_any "get1" (get a_tree "a") (Some 5)
  ; t_any "get2" (get (Node (0, "b", 15, a_tree, Leaf)) "a") (Some 5)
  ; t_any "get3" (get (Node (0, "b", 15, a_tree, Leaf)) "c") None ]

let height_tests =
  [ t_any "height1" (height Leaf) 0
  ; t_any "height2" (height a_tree) 1
  ; t_any "height3" (height b_tree) 3 ]

let get_key_test =
  [t_any "get_key1" (get_key a_tree) "a"; t_any "get_key2" (get_key b_tree) "d"]

let balance_ll_test =
  [t_any "balance_ll1" (balance_ll ll_tree) ll_tree_balanced]

let balance_lr_test =
  [t_any "balance_lr1" (balance_lr lr_tree) lr_tree_balanced]

let balance_rl_test =
  [t_any "balance_rl1" (balance_rl rl_tree) rl_tree_balanced]

let balance_rr_test =
  [t_any "balance_rr1" (balance_rr rr_tree) rr_tree_balanced]

let inorder_tests =
  [ t_inorder "inorder1" (inorder a_tree) [("a", 5)]
  ; t_inorder "inorder2" (inorder b_tree)
      [("a", 1); ("b", 2); ("c", 3); ("d", 4); ("e", 5); ("f", 6); ("g", 7)] ]

let contains_tests =
  [ t_any "contains1" (contains a_tree "c") false
  ; t_any "constains2" (contains b_tree "e") true
  ; t_any "constains3" (contains b_tree "b") true
  ; t_any "constains4" (contains b_tree "n") false ]

let sum_tests =
  [ t_any "sum1" (sum Leaf) 0
  ; t_any "sum2" (sum a_tree) 5
  ; t_any "sum3" (sum b_tree) 28 ]

let all_tests =
  get_tests @ contains_tests @ sum_tests @ inorder_tests @ height_tests
  @ get_key_test @ balance_ll_test @ balance_rr_test @ balance_lr_test
  @ balance_rl_test @ add_all_test

let suite = "suite" >::: all_tests

;;
run_test_tt_main suite
