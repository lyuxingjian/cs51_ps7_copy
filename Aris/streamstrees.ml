(*
                         CS 51 Problem Set 7
                       Refs, Streams, and Music
                       Part 2: Lazy Evaluation
 *)

(*======================================================================
Section 2.1: Series acceleration with infinite streams

In the file `nativeLazyStreams.ml`, we provide the definitions of
lazy streams using OCaml's native `Lazy` module as presented in
Section 17.3 in the textbook, and in the file `sampleStreams.ml`, we
provide some sample streams from that chapter, up to and including
code for approximating pi through partial sums of the terms in a
Taylor series (Section 17.4). In lab 15, you used streams to find
faster approximations for pi by averaging adjacent elements in the
stream. In this section, you'll use Aitken's method to generate even
faster convergence to pi. *)
   
open NativeLazyStreams ;;

(* Recall from Section 17.4 the use of streams to generate
approximations of pi of whatever accuracy. Try it. You should be able
to reproduce the following:

   # within 0.01 pi_sums ;;
   - : int * float = (199, 3.13659268483881615)
   # within 0.001 pi_sums ;;
   - : int * float = (1999, 3.14109265362104129)
   # within 0.0001 pi_sums ;;
   - : int * float = (19999, 3.14154265358982476)

Notice that it takes about 2000 terms in the Taylor series to get
within 0.001 of the value of pi. This method converges quite
slowly. 

let rec within epsilon s =
 let hd,tl = head s, tail s in
 if abs_float (hd -. (head tl)) < epsilon then hd
    else within epsilon tl ;;


But we can increase the speed dramatically using a technique called
"series acceleration". In lab 15, you began experimenting with series
acceleration, speeding up the convergence of the `pi_sums` stream by
averaging its elements. The `average` function takes a `float stream`
and returns a stream of `float`s each of which is the average of
adjacent values in the input stream. For example:

    # first 5 (average (to_float nats)) ;;
    - : float list = [0.5; 1.5; 2.5; 3.5; 4.5]
 *)

let average (s : float stream) : float stream =
  smap2 (fun x y -> (x +. y) /. 2.0) s (tail s) ;;

(* Now instead of using the stream of approximations in `pi_sums`, we
can instead use the stream of averaged `pi_sums`, which converges much
more quickly:

    # within 0.01 (average pi_sums) ;;
    - : int * float = (9, 3.13707771416749859)
    # within 0.001 (average pi_sums) ;;     
    - : int * float = (30, 3.14209630544471885)
    # within 0.0001 (average pi_sums) ;;
    - : int * float = (99, 3.14154315231477277)
    # within 0.00001 (average pi_sums) ;;
    - : int * float = (315, 3.14158766221239905)
    # within 0.000001 (average pi_sums) ;;
    - : int * float = (999, 3.14159215408966919)
    # within 0.0000001 (average pi_sums) ;;
    - : int * float = (3161, 3.1415926035968269)


     within 0.01 (aitken pi_sums) ;;
     within 0.001 (aitken pi_sums) ;;     
     within 0.0001 (aitken pi_sums) ;;
     within 0.00001 (aitken pi_sums) ;;
     within 0.000001 (aitken pi_sums) ;;
     within 0.0000001 (aitken pi_sums) ;;



We've placed a table below of the number of steps required for the
`pi_sums` and `average pi_sums` methods.

An even better accelerator of convergence for series of this sort
is Aitken's method. The formula is given in the problem set writeup in
Section 17.8.3. *)
  
(*......................................................................
Problem 4: Implementing Aitken's method

Write a function `aitken` to apply this accelerator to a stream, and
use it to generate approximations of pi.
......................................................................*)
   
let aitken (s: float stream) : float stream =
    let s1 = smap2 (fun x y -> x -. 2. *. y) s (tail s) in 
    let s2 = smap2 (fun x y -> x +. y) s1 (tail (tail s)) in 
    let s3 = smap2 (fun x y -> (x -. y) ** 2.) s (tail s) in 
    let s4 = smap2 (fun x y -> x /. y) s3 s2 in 
    smap2 (fun x y -> x -. y) s s4;; 


(*......................................................................
Problem 5: Testing the acceleration

Fill out the following table, recording how many steps are needed to
get within different epsilons of pi using Aitken's method.
 *)
(*
    -------------------------------------------------------------
    epsilon  |  pi_sums  |  averaged method  |  Aitken's method
    -------------------------------------------------------------
    0.1      |        19 |                 2 |  2 
    -------------------------------------------------------------
    0.01     |       199 |                 9 |  6 
    -------------------------------------------------------------
    0.001    |      1999 |                30 |  15 
    -------------------------------------------------------------
    0.0001   |     19999 |                99 |  35
    -------------------------------------------------------------
    0.00001  |    199999 |               315 |  77 
    -------------------------------------------------------------
    0.000001 |  too many |               999 |  169
    -------------------------------------------------------------
 *)
(*....................................................................*)

(*======================================================================
Section 2.2 : Infinite trees

Just as streams are a lazy form of list, we can have a lazy form of
trees. In the definition below, each node in a lazy tree of type `'a
tree` holds a value of some type `'a`, and a (conventional, finite)
list of one or more (lazy) child trees. Complete the implementation by
writing `print_depth`, `tmap`, `tmap2`, and `bfenumerate`. We
recommend implementing them in that order.
......................................................................*)
   
type 'a tree_internal = Node of 'a * 'a tree list
 and 'a tree = 'a tree_internal Lazy.t ;;

(* Infinite trees shouldn't have zero children. This exception is
available to raise in case that eventuality comes up. *)

exception Finite_tree ;;

(*......................................................................
Problem 6: Implement the following functions for manipulating
infinite trees. *)

(*......................................................................
node t -- Returns the element of type `'a` stored at the root node of
tree `t` of type `'a tree`.
......................................................................*)
  
let node (t : 'a tree) : 'a =
    let Node(a, b) = Lazy.force t in 
    if b = [] then raise Finite_tree 
    else a ;; 

(*......................................................................
children t -- Returns the list of children of the root node of tree `t`.
......................................................................*)
   
let children (t : 'a tree) : 'a tree list =
    let Node(_, b) = Lazy.force t in 
    if b = [] then raise Finite_tree 
    else b ;; 


(*......................................................................
print_depth n indent t -- Prints a representation of the first `n`
levels of the tree `t` indented `indent` spaces. You can see some
examples of the intended output of `print_depth` below.
......................................................................*)

(* let rec print_hds (lst : 'a tree list) : unit = *) 
(*     match lst with *) 
(*     | [] -> () *)
(*     | hd :: tl -> print_endline (string_of_int (node hd)); print_hds tl;; *) 

let rec print_depth (n : int) (indent : int) (t : int tree) : unit =
    if n = 0 then () else
        print_endline (string_of_int (node t));  
    let rec print_depth' (lst : 'a tree list) (i : int) : unit = 
        match lst  with 
        |[] -> () 
        |hd :: tl -> Printf.printf "%*s" indent ""; 
        print_depth (n - 1) indent hd; 
        print_depth' tl (i + indent) in 
    print_depth' (children t) (indent);; 
        

(*......................................................................
tmap f t -- Returns a tree obtained by mapping the function `f` over
each node in `t`.
......................................................................*)
   
let rec tmap (f : 'a -> 'b) (t : 'a tree) : 'b tree =
    lazy (Node(f (node t), List.map (fun x -> tmap f x) (children t)));; 

(*......................................................................
tmap2 f t1 t2 -- Returns the tree obtained by applying the function
`f` to corresponding nodes in `t1` and `t2`, which must have the same
"shape". If they don't, an `Invalid_argument` exception is raised.
......................................................................*)
   
let rec tmap2 (f : 'a -> 'b -> 'c)
          (t1 : 'a tree) (t2 : 'b tree)
        : 'c tree =
    if List.length (children t1) <> List.length (children t2) 
      then raise (Invalid_argument "trees not same length")  
    else lazy(Node(f (node t1) (node t2), List.map2 (fun x y -> tmap2 f x y) 
      (children t1) (children t2)));; 
            

(*......................................................................
bfenumerate tree_list -- Returns a `stream` of the nodes in the list
of trees `tree_list` enumerated in breadth-first order, that is, the
root nodes of each of the trees, then the level one nodes, and so
forth. There is an example of `bfenumerate` being applied below. If
there isn't an infinite set of nodes in the list of trees (think about
how that could come about), raise a `Finite_tree` exception.
......................................................................*)


let bfenumerate (tree_list : 'a tree list) : 'a stream =
    if tree_list = [] then raise Finite_tree else 
    let rec split (super: 'a tree list) (sub : 'a tree list)
    : 'a stream = 
        match super with 
        | [] -> Nil 
        | hd :: tl -> Cons(node hd, split tl (sub @
        [children hd]))  
    in 




    match Lazy.force (Lazy.hd tree_list) with 
    |Node (a, b) -> 
    Cons(List.hd tree_list, bfenumerate List.tl tree_list);;




(* Now you'll use your implementation to generate some interesting
infinite trees.  Hint: Drawing a tree considering how the values
change along each branch will yield helpful intuition for the next
problems. *)

(*......................................................................
onest -- An infinite binary tree all of whose nodes hold the integer 1.
......................................................................*)
 let rec onest : int tree =
     lazy (Node (1, [onest; onest]));; 


(*......................................................................
levels n -- Returns an infinite binary tree where the value of each
node in the tree is its level or depth in the tree, starting with the
argument `n`. For example:

    # print_depth 2 0 (levels 0) ;;
    0
     1
      2...
      2...
     1
      2...
      2...
    - : unit = ()
......................................................................*)
   
let rec levels (n : int) : int tree =
    lazy (Node (n, [levels (n + 1); levels (n + 1)]));; 

(*......................................................................
tree_nats -- An infinite binary tree where the value of each
node in the tree is consecutively numbered in breadth-first order
starting with 0. For example:

    # print_depth 2 0 tree_nats ;;
    0
     1
      3...
      4...
     2
      5...
      6...
    - : unit = ()

    # first 10 (bfenumerate [tree_nats]) ;;
    - : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]
......................................................................*)
   
let rec tree_nats : int tree =
    lazy (Node 0, [levels 1, 
  lazy (failwith "tree_nats not implemented") ;;

(*======================================================================
Reflection on the problem set

     Please fill out the information about time spent and your
     reflection thereon in the file refs.ml.
 *)
