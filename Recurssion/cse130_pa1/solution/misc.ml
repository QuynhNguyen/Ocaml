(* CSE 130: Programming Assignment 1
 * misc.ml
 * Quynh Nguyen
 * A09283252
 * qun003@ucsd.edu
 *)

(* sumList : int list -> int 
	(sumList int list ) is a function to add up all the digits in the list
	together and display the result.
	e.g. int list [1;2;3;4] will display 10
		 int list [1;-2,-4] will display -5
		 empty list will yield 0 b
*) 

let rec sumList l =
	match l with 
	|[] -> 0 (*Return 0 if the list is empty -- iterated through out whole list*)
	|(h::t) -> h + sumList t (*Add all the head together*);;



(* digitsOfInt : int -> int list 
 * (digitsOfInt n) is the list of digits of n in order of which they appear.
 * This will yield an empty list if n is a negative number
 * e.g. (digitsOfInt 1234) is [1;2;3;4]
 *		(digittsOfInt -123123) is []
*)

let rec digitsOfInt n = 	
	if n <= 0 then
		[] (*we want to return empty list if n is a negative number*)
	else
		digitsOfInt (n / 10) @ (n mod 10) :: [];; (*turn digit into list then append them all together*)


(* digits : int -> int list
 * (digits n) is the list of digits of n in the order in which they appear
 * in n
 * e.g. (digits 31243) is [3,1,2,4,3]
 *      (digits (-23422) is [2,3,4,2,2]
 *)
 
let digits n = digitsOfInt (abs n)


(* From http://mathworld.wolfram.com/AdditivePersistence.html
 * Consider the process of taking a number, adding its digits, 
 * then adding the digits of the number derived from it, etc., 
 * until the remaining number has only one digit. 
 * The number of additions required to obtain a single digit from a number n 
 * is called the additive persistence of n, and the digit obtained is called 
 * the digital root of n.
 * For example, the sequence obtained from the starting number 9876 is (9876, 30, 3), so 
 * 9876 has an additive persistence of 2 and a digital root of 3.
 *)


(* ***** PROVIDE COMMENT BLOCKS FOR THE FOLLOWING FUNCTIONS ***** *)

(* additivePersistence: int -> int
 * (additivePersistence n ) is a function that takes in a positive n number and return
 * the total number of time required to obtain a single digit from a number n
 * e.g. (additivePersistence 92) is 2
 *     (additivePErsistence 99993) is 3
 *)
let additivePersistence n =
	if n < 10 then
		0
	else begin
		let rec persistenceHelper n = begin
			let rec addDigits n =
				if n <= 0 then
					0
				else
					((n mod 10) + addDigits (n / 10)) in
				let x = addDigits n in
					if x > 9 then
						1 + persistenceHelper x
					else
						1 
		end in persistenceHelper n;
	end;;
	
(* digitalRoot: int -> int
 * (digitalRoot n ) is a function that takes in a positive n number and keep
 * adding all the digits of n until n is a single digit.
 * e.g. (digitalRoot 92) is 2
 *     (additivePErsistence 99993) is 3
 *)
let digitalRoot n = 
	let rec digitalRootHelper n = begin
		if n <= 0 then
			0
		else
			let x = ((n mod 10) + digitalRootHelper (n / 10)) in
				if (x / 10) >= 1 then
					digitalRootHelper x
				else
					x 
	end in digitalRootHelper n;;
	


(* listReverse: 'a list -> 'a list
 * (listReverse 'a list) is a function that reverse all the elements within an input list
 * e.g. (listReverse [1;2;3]) is [3;2;1]
 *       (listReverse ["a","b"]) is ["b", "a"]
*)
let listReverse l = 
	let rec reverseHelper xs = 
		match xs with 
		|[] -> []
		|(h::t) -> (reverseHelper t) @ (h :: []) in reverseHelper l;;

(* explode : string -> char list 
 * (explode s) is the list of characters in the string s in the order in 
 *   which they appear
 * e.g.  (explode "Hello") is ['H';'e';'l';'l';'o']
 *)
let explode s = 
  let rec _exp i = 
    if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
  _exp 0


(* palindrome: string -> bool
 * (palindrome s) is a function that check whether a given string is a palindrome.
 * The function will return true if the string is indeed a palindrome; otherwise, return false.
 * e.g. (palindrome "racecar") is true
 *      (palindrome "something") is false	
 *)
let palindrome w = 
	let rec palHelper l1 l2 = begin
		match l1, l2 with 
		(h1::t1), (h2::t2) ->
			let myBoo = begin
				if h1 == h2 then
					true
				else
					false
			end	in myBoo && (palHelper t1 t2)
		|_ -> true
	end in 
	let stringList = explode w in
	let reverseList = listReverse stringList in
	palHelper stringList reverseList;;

(************** 
 ***************)
