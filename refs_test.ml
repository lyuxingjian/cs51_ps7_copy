(*
                         CS 51 Problem Set 7
                       Refs, Streams, and Music
                             Refs Testing
 *)

open CS51Utils ;;
open Absbook ;;

(* Make your refs solution available for testing *)
open Refs ;;

(* Establish some mutable lists for testing. *)
let list1a = Cons (2, ref Nil) ;;
let list1b = Cons (2, ref list1a) ;;
let list1 = Cons (1, ref list1b) ;;

let reflist = ref (Cons (2, ref Nil)) ;;
let list2 = Cons (1, ref (Cons (2, reflist))) ;;
let _ = reflist := list2 ;;

(* Some example tests. You'll want more. *)
let tests () =
  unit_test (not (has_cycle list1a)) "list1a no cycle";
  unit_test (has_cycle !reflist) "reflist has cycle";

  (* More tests go here. . . *) 

  () ;;


let _ = tests () ;;
