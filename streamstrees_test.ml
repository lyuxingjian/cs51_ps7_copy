(*
                         CS 51 Problem Set 7
                       Refs, Streams, and Music
                             Refs Testing
 *)

open CS51Utils ;;
open Absbook ;;

(* Make your refs solution available for testing *)
module SS = SampleStreams;;
module ST = Streamstrees ;;
module NLS = NativeLazyStreams ;;
module R = Refs ;;

let test_average () = 
  let float_nats = ST.to_float (SS.nats) in 
    unit_test ((NLS.first 3 (ST.average float_nats)) = [0.5; 1.5; 2.5]) "avg test 1" ;
    unit_test ((NLS.first 3 (SS.ones)) = [1; 1; 1]) "avg test 2" ;
    () ;;

let test_trees () = 
  (* treemap test *)
  let one_tree = ST.onest in 
  let two_tree = ST.tmap ((+) 1) one_tree in 
  let three_tree = ST.tmap ((+) 1) two_tree in 
  unit_test ((ST.node two_tree) = 2) "tmap 1";
  unit_test ((ST.node three_tree) = 3) "tmap 2";
  let six_tree = ST.tmap2 ( * ) two_tree three_tree in 
  unit_test ((ST.node six_tree) = 6) "tmap2 1";
  let six_tree = ST.tmap2 ( - ) two_tree three_tree in 
  unit_test ((ST.node six_tree) = (0 - 1)) "tmap2 2";
  let six_tree = ST.tmap2 ( - ) two_tree three_tree in 
  unit_test ((ST.node six_tree) = (0 - 1)) "tmap2 2";
  let nats = ST.tree_nats in 
  let nats_stream = ST.bfenumerate [nats] in 
  unit_test (NLS.first 3 (ST.bfenumerate [one_tree]) = [1; 1; 1]) "bfenumerate 1"; 
  unit_test ((NLS.first 3 nats_stream) = [0; 1; 2]) "bfenumerate 2";
  let even_tree = ST.tmap2 ( ( * ) ) nats two_tree in 
  unit_test (NLS.first 5 (ST.bfenumerate [even_tree]) = [0; 2; 4; 6; 8]) "bfenumerate 3";  
  () ;;

let streamstree_tests () =
  test_average () ;
  test_trees () ;
  () ;;

let test_all () = 
  streamstree_tests () ; 
  ()
;;

let _ = test_all () ;;
