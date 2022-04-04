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

and test for cycles using `has_cycle`:

    # has_cycle cyclic ;;
    - : bool = true
    # has_cycle acyclic ;;
    - : bool = false
......................................................................*)
 
let cycle (lst : 'a mlist ref) =
    let searched = ref [] in  
    fun () -> 
      let found = List.memq lst !searched in 
      searched := lst :: !searched; 
      found;; 

(* let cycle2 (lst : 'a mlist ref) = *)
(*     let searched = ref [] in *)  
(*     fun () -> *) 
(*       let found = List.mem lst !searched in *) 
(*       searched := lst :: !searched; *) 
(*       found, searched;; *) 


let rec has_cycle (lst : 'a mlist) : bool  = 
  match lst with 
  | Nil -> false 
  | Cons (a, b) -> cycle b () || has_cycle !b;;  


(*......................................................................
Problem 2: Write a function `flatten` that flattens a list (removes
its cycles if it has any) destructively. Again, you may want a
recursive auxiliary function, and you shouldn't worry about space.
......................................................................*)

let cycle2 (lst : 'a mlist ref) =
    let searched = ref [] in  
    fun () -> 
      let found = List.memq lst !searched in 
      searched := lst :: !searched; 
      found, searched;; 

(* let rec mlist_of_list (lst : 'a list) : 'a mlist = *)
(*   match lst with *) 
(*   | [] -> Nil *) 
(*   | hd :: tl -> Cons (hd, ref (mlist_of_list tl));; *) 

let rec lst_to_mlst (lst : (('a mlist ref) list)) : 'a mlist = 
  match lst with 
  |[] -> Nil 
  | hd :: tl -> 
      match !hd with 
      |Nil -> failwith "impossible mlist"  
      |Cons(a, b) -> Cons(a, ref (lst_to_mlst tl) );; 

let rec flatten (lst : 'a mlist ref) : unit =
  if (has_cycle !lst) = false then () else 
  match !lst with 
  | Nil -> ()
  | Cons(a, b) -> let found, searched = cycle2 b () in 
  if found then lst := (lst_to_mlst !searched) else flatten b;; 




(*......................................................................
Problem 3: Write a function `mlength`, which nondestructively finds
the number of nodes in a mutable list that may have cycles.
......................................................................*)

(* let cycle2 (lst : 'a mlist ref) = *)
(*     let searched = ref [] in *)  
(*     fun () -> *) 
(*       let found = List.memq lst !searched in *) 
(*       searched := lst :: !searched; *) 
(*       found, searched;; *) 

let rec chop (revtl : ('a mlist ref) list) (hd: 'a mlist ref) : ('a mlist ref) list = 
  match revtl with 
  | [] -> []
  | h :: t -> if h == hd then chop t hd else h :: chop t hd;;  

let rec mlength (lst : 'a mlist) : int =
  if (has_cycle lst) = false then 0 else 
  match lst with 
  | Nil -> 0
  | Cons(a, b) -> let found, searched = cycle2 b () in 
  if found then let revlst = List.rev !searched in 
  List.length (chop (List.tl revlst) (List.hd revlst))
  else mlength !b;; 



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
