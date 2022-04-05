(*
                         CS 51 Problem Set 7
                       Refs, Streams, and Music
                   Part 1: Mutable Lists and Cycles
 *)

(* The type of mutable lists. *)
type 'a mlist = Nil | Cons of 'a * 'a mlist ref ;;

 (*......................................................................
Problem 1: Write a function `has_cycle` that returns `true` if a
mutable list has a cycle, `false` otherwise. You may want a recursive
auxiliary function. Don't worry about space usage.

For instance, we can establish a cyclic and an acyclic mutable list
like this:

    # let sample_end = ref Nil ;;
    # let cyclic = Cons (1, ref (Cons (2, sample_end))) ;;
    # sample_end := cyclic ;;
    # let acyclic = Cons (3, ref (Cons(4, ref Nil))) ;;

     let sample_end = ref Nil ;;
     let cyclic = Cons (1, ref (Cons (2, sample_end))) ;;
     sample_end := cyclic ;;
     let acyclic = Cons (3, ref (Cons(4, ref Nil))) ;;
let sample_end2 = ref Nil;; 
let cyclic2 = Cons(3, ref (Cons (4, ref (Cons (5,
sample_end2)))));; 
sample_end2 := cyclic ;; 


and test for cycles using `has_cycle`:

    # has_cycle cyclic ;;
    - : bool = true
    # has_cycle acyclic ;;
    - : bool = false
......................................................................*)
(* let rec cycle (lst : 'a mlist) (searched : ('a mlist ref) list) = *) 
(*   match lst with *) 
(*   | Nil -> false, searched *)  
(*   | Cons (hd, tl) -> let current = ref (Cons(hd, tl)) in *) 
(*   if not (List.memq (current) searched) *)
(*   then cycle !tl (current :: searched) else true, (current :: searched);; *)  

let rec cycle (lst : 'a mlist) (searched : ('a mlist ref) list) = 
  match lst with 
  | Nil -> false, searched  
  | Cons (_hd, tl) -> if not (List.memq tl searched)
  then cycle !tl (tl :: searched) else true, (tl :: searched);;  


(* let rec cycle (lst : 'a mlist) (searched : ('a mlist) list) = *) 
(*   match lst with *) 
(*   | Nil -> false, searched *)  
(*   | Cons (_hd, tl) -> if not (List.memq lst searched) *)
(*   then cycle !tl (lst :: searched) else true, (lst :: searched);; *)  

(* if List.for_all ((!=) lst) seen then *) 
(*         flatten' !ref_tl (lst :: seen) ref_tl else *) 


(* let rec cycle (lst : 'a mlist) (searched : ('a mlist ref) list) = *) 
(*   match lst with *) 
(*   | Nil -> false, searched *)  
(*   | Cons (_hd, tl) -> if not (List.memq tl searched) *)
(*   then cycle !tl (searched @ [tl]) else true, (searched @ [tl]);; *)  


let has_cycle (lst : 'a mlist) : bool  = 
  fst (cycle lst []);; 

(*......................................................................
Problem 2: Write a function `flatten` that flattens a list (removes
its cycles if it has any) destructively. Again, you may want a
recursive auxiliary function, and you shouldn't worry about space.
......................................................................*)
(* let rec lst_to_mlst (lst : (('a mlist ref) list)) : 'a mlist = *) 
(*   match lst with *) 
(*   |[] -> Nil *) 
(*   | hd :: tl -> *) 
(*       match !hd with *) 
(*       |Nil -> failwith "impossible mlist" *)  
(*       |Cons(a, _b) -> Cons(a, ref (lst_to_mlst tl) );; *) 

(* let flatten (lst : 'a mlist) : unit = *)
(*   let found, searched = cycle lst [] in *)
(*   if found = false then () *)
(*   else List.hd searched := lst_to_mlst searched ;; *) 

(* let flatten (lst : 'a mlist) : unit = *)
(*   let found, searched = cycle lst [] in *)
(*   if found = false then () *)
(*   else List.hd searched := Nil ;; *) 


let flatten (lst : 'a mlist) : unit =
  let rec flatten' (lst : 'a mlist) (seen : ('a mlist) list) (last_ref : 'a mlist ref) = 
    match lst with 
    | Nil -> ()
    | Cons (hd, ref_tl) ->
      (* Same as "has_cycle": detect whether the lst itself has been seen before; 
      checks physical inequalty *) 
      if List.for_all ((!=) lst) seen then 
        flatten' !ref_tl (lst :: seen) ref_tl else 
        (* If so, replace the *last* reference (replacing the current tail 
        will simultaneously overwrite a previous tail ref, which we don't want) *)
        last_ref := Nil; 
      in 
  flatten' lst [] (ref lst);;



(*......................................................................
Problem 3: Write a function `mlength`, which nondestructively finds
the number of nodes in a mutable list that may have cycles.
......................................................................*)

(* chops off nodes not in cycle *) 
let rec chop (lst : ('a mlist ref) list) (hd: 'a mlist ref)
(final : ('a mlist ref) list) : ('a mlist ref) list = 
  match lst with 
  |[] -> []
  |h :: t -> if h == hd then final else chop t hd (h ::
    final) ;; 

(* let rec chop (lst : ('a mlist) list) (hd: 'a mlist) *)
(* (final : ('a mlist) list) : ('a mlist) list = *) 
(*   match lst with *) 
(*   |[] -> [] *)
(*   |h :: t -> if h == hd then final else chop t hd (h :: *)
(*     final) ;; *) 


let mlength (lst : 'a mlist) : int =
  let found, searched = cycle lst [] in 
  if found = false then 0
  else 1 + List.length(chop (List.tl searched) (List.hd searched) []);; 
  


(*   if (has_cycle lst) = false then 0 else *) 
(*   match lst with *) 
(*   | Nil -> 0 *)
(*   | Cons(a, b) -> let found, searched = cycle2 b () in *) 
(*   if found then let revlst = List.rev !searched in *) 
(*   List.length (chop (List.tl revlst) (List.hd revlst)) *)
(*   else mlength !b;; *) 



(*======================================================================
Reflection on the problem set

After each problem set, we'll ask you to reflect on your experience.
We care about your responses and will use them to help guide us in
creating and improving future assignments.

........................................................................
Please give us an honest (if approximate) estimate of how long (in
minutes) this problem set (in total, not just this file) took you to
complete. (If you worked with a partner, we're asking for how much time
each of you (on average) spent on the problem set, not in total.)
......................................................................*)

let minutes_spent_on_pset () : int =
  failwith "time estimate not provided" ;;

(*......................................................................
It's worth reflecting on the work you did on this problem set, where
you ran into problems and how you ended up resolving them. What might
you have done in retrospect that would have allowed you to generate as
good a submission in less time? Please provide us your thoughts in the
string below.
......................................................................*)

let reflection () : string =
  "...your reflections here..." ;;
