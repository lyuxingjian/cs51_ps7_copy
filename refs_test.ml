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
let list1a' = list1a;; 
let list1b' = list1b;; 
let list1'  = list1;;

let reflist = ref (Cons (2, ref Nil)) ;;
let list2 = Cons (1, ref (Cons (2, reflist))) ;;

let _ = reflist := list2 ;;
let reflist' = (!reflist);; 

let sample_end = ref Nil ;;
let cyclic = Cons (1, ref (Cons (2, sample_end))) ;;
sample_end := cyclic ;;

let acyclic = Cons (3, ref (Cons(4, ref Nil))) ;;

let sample_end2 = ref Nil;; 
let cyclic2 = Cons(3, ref (Cons (4, ref (Cons (5, sample_end2)))));; 
sample_end2 := cyclic ;; 

let sample_end3 = ref Nil;; 
let cyclic3 = Cons(3, ref (Cons (4, ref (Cons (5, sample_end3)))));; 
sample_end3 := cyclic3 ;; 



let has_cycle_test () =
  unit_test (not (has_cycle list1a)) "has_cycle list1a no cycle";
  unit_test (not (has_cycle list1b)) "has_cycle list1b no cycle"; 
  unit_test (not (has_cycle list1 )) "has_cycle list1 no cycle"; 
  unit_test (not (has_cycle acyclic )) "has_cycle acyclic no cycle"; 
  unit_test (has_cycle !reflist) "has_cycle reflist has cycle";
  unit_test (has_cycle cyclic)   "has_cycle cyclic has cycle";
  unit_test (has_cycle cyclic2) "has_cycle cyclic2 has cycle";;

let mlength_test () = 
  unit_test (mlength list1a = 0) "mlength list1a no cycle";
  unit_test (mlength list1b = 0) "mlength list1b no cycle";
  unit_test (mlength list1  = 0) "mlength list1  no cycle";
  unit_test (mlength acyclic  = 0) "mlength acyclic no cycle";
  unit_test (mlength !reflist = 2) "mlength reflist cycle";
  unit_test (mlength cyclic = 2)   "mlength cyclic  cycle";
  unit_test (mlength cyclic2  = 2) "mlength cyclic2 cycle";
  unit_test (mlength cyclic3  = 3) "mlength cyclic3 cycle";;

let flatten () = 
  unit_test ((flatten list1a); list1a  = list1a') "flatten list1a no cycle";
  unit_test ((flatten list1b); list1b  = list1b') "flatten list1b no cycle";
  unit_test ((flatten list1); list1 = list1') "flatten list1  no cycle";
  unit_test ((flatten cyclic); cyclic = Cons(1, ref (Cons (2, ref Nil)))) "flatten cyclic cycle";
  unit_test ((flatten cyclic2); 
    cyclic2 = Cons(3, ref (Cons(4, ref (Cons(5, ref(Cons(1,
    ref (Cons(2, ref Nil)))))))))) "flatten cyclic2 cycle";
  unit_test ((flatten cyclic3); 
    (cyclic3 =  Cons(3, ref
    (Cons(4, ref (Cons(5, ref Nil))))))) "flatten cyclic3 cycle";;

 


 


(* let flatten_test () = *) 
(*   unit_test (flatten reflist'; reflist' = *) 
 
(*   (1* More tests go here. . . *1) *) 


let test_all () =
  has_cycle_test (); 
  mlength_test (); 
  flatten ();; 

let _ = test_all () ;;

