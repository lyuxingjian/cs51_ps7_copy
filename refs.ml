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

let sample_end = ref Nil ;;
let cyclic = Cons (1, ref (Cons (2, sample_end))) ;;
sample_end := cyclic ;;
    # let acyclic = Cons (3, ref (Cons(4, ref Nil))) ;;

and test for cycles using `has_cycle`:

    # has_cycle cyclic ;;
    - : bool = true
    # has_cycle acyclic ;;
    - : bool = false
......................................................................*)
                                      
let has_cycle (lst : 'a mlist) : bool =
  let rec has_cycle' (lst : 'a mlist) (seen : ('a mlist ref) list) : bool = 
    match lst with 
    | Nil -> false 
    | Cons (_, ref_tl) -> 
      (* If this has not been seen before: move on 
      i.e. move to next element, and append the current reference to "seen" *)
      if List.for_all ((!=) ref_tl) seen then 
        has_cycle' !ref_tl (ref_tl :: seen) else 
      true in 
  has_cycle' lst [] ;;

(*......................................................................
Problem 2: Write a function `flatten` that flattens a list (removes
its cycles if it has any) destructively. Again, you may want a
recursive auxiliary function, and you shouldn't worry about space.
......................................................................*)

let flatten (lst : 'a mlist) : unit =
  let rec flatten' (lst : 'a mlist) (seen : ('a mlist ref) list) = 
    match lst with 
    | Nil -> ()
    | Cons (hd, ref_tl) ->
      (* Same as "has_cycle": detect whether ref_tl has been seen before *) 
      if List.for_all ((!=) ref_tl) seen then 
        flatten' !ref_tl (ref_tl :: seen) else 
        (* If so, replace the *last* reference (replacing the current tail 
        will simultaneously overwrite a previous tail ref, which we don't want) *)
        List.hd seen := Cons (hd, ref Nil)
      in 
  flatten' lst [] ;;

(*......................................................................
Problem 3: Write a function `mlength`, which nondestructively finds
the number of nodes in a mutable list that may have cycles.
......................................................................*)

(* First note that there can be at most one cycle *)
let mlength (lst : 'a mlist) : int =
  let rec mlength' (lst : 'a mlist) (seen : ('a mlist ref) list) : int = 
    match lst with 
    | Nil -> List.length seen 
    | Cons (hd, ref_tl) ->
      (* Same as "has_cycle": detect whether ref_tl has been seen before *) 
      if List.for_all ((!=) ref_tl) seen then 
        mlength' !ref_tl (ref_tl :: seen) else 
        (* If so, simply return the number of tails seen *)
        List.length seen 
      in 
  mlength' lst [] ;;

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
