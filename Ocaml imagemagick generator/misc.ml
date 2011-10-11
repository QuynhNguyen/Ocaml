(* CSE 130: Programming Assignment 2
 * misc.ml
 *)

(* assoc : int * string * (string * int) list -> int
 * or more generally, assoc : 'a * 'b * ('b * 'a) list -> 'a
 * assoc (d,k,[(k1,v1);(k2,v2);(k3,v3);...]) searches the list for the first i such
 *   that ki = k.  If such a ki is found, then vi is returned.  Otherwise, if no such
 *   ki exists in the list, d is returned.
 * e.g. (assoc (-1,"william",[("ranjit",85);("william",23);("moose",44)]))
 *        returns 23
 *      (assoc (-1,"bob",[("ranjit",85);("william",23);("moose",44)]))
 *        returns -1
 *
 *  ** your function should be tail recursive **
 *)

let rec assoc (d,k,l) = 
	match l with 
	|[] -> (-1)
	|((h,x)::t) -> if h = k then
						x
					else
						assoc (d,k,t)

(* removeDuplicates : int list -> int list 
 * or more generally, removeDuplicates : 'a list -> 'a list
 * (removeDuplicates l) is the list of elements of l with duplicates (second,
 * third ... occurrences) removed, and where the remaining elements 
 * appear in the same order as in l.
 * e.g. (removeDuplicates [1,6,2,4,12,2,13,6,9]) is [1,6,2,4,12,13,9]
 *
 *  ** your function "helper" should be tail recursive **
 * for this problem only, you may use the library function List.mem and List.rev
 *)

(* fill in the code wherever it says : failwith "to be written" *)
let removeDuplicates l = 
  let rec helper (seen,rest) = 
      match rest with 
        [] -> seen
      | h::t -> 
        let seen' = if List.mem h seen then
						[]@seen
					else
						[h]@seen in
        let rest' = t in 
	  helper (seen',rest') 
  in
      List.rev (helper ([],l))


(* wwhile : (int -> int * bool) * int -> int
 * or more generally, ('a -> 'a * bool) * 'a -> 'a
 * wwhile (f,b) should call the function f on input b, to get a pair (b',c').
 *   wwhile should continue calling f on b' to update the pair as long as c' is true
 *   once f returns a c' that is false, wwhile should return b'
 * e.g. let f x = let xx = x*x*x in (xx,xx<100);;
 *   wwhile (f,2) should return 512
 *
 *  ** your function should be tail recursive **
 *)
let rec wwhile (f,b) = 
	let result = f b in
		match result with
		|(xInt, xBool) -> if xBool then
							wwhile (f, xInt) 
						else
							xInt

(* fixpoint : (int -> int) * int -> int
 * or more generally, fixpoint : ('a -> 'a) * 'a -> 'a
 * fixpoint (f,b) repeatedly replaces b with f(b) until b=f(b) and then returns b
 * e.g. let g x = truncate (1e6 *. cos (1e-6 *. float x));;
 *   fixpoint (g,0) should return 739085    (this is because cos 0.739085 is approximately 0.739085)
 *)

(* fill in the code wherever it says : failwith "to be written" *)
(*Note: While expected 1 function and 1 integer input
 * 1 function from while: Should return a (int, boolean) tuple
 * the f from fixpoint input is a function which take an int and return an int 
*)
let fixpoint (f,b) = wwhile ((
	let xx b = 
		let cal = f b in
			if b == cal then
				(cal, false) 
			else
				(cal, true)		
	in xx

),b)

(************** Add Testing Code Here ***************)
