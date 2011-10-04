(* CSE 130: Programming Assignment 1
 * misc.ml
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
let yourTests = 
  [ 
  (fun () -> mkTest 
    sumList 
    [1;2;-3;4;5;-6;-3] 
    0
    "sumList 1: Regular Case"
  ); 
  (fun () -> mkTest 
    sumList 
    [10;11;12]
    33
    "sumList 2: Double Digits"
  ); 
  (fun () -> mkTest 
    sumList 
    [9] 
    9
    "sumList 3: 1 item"
  ); 
  (fun () -> mkTest 
    sumList 
    [0;0;0;0]
    0
    "sumList 4: Nothing but 0"
  ); 
  (fun () -> mkTest 
    sumList 
    [] 
    0
    "sumList 5: Nothing but air - Empty List"
  );

  (fun () -> mkTest 
    digitsOfInt 
    (-9999)
    [] 
    "digitsOfInt 1: negative number"
  ); 
  (fun () -> mkTest 
    digitsOfInt 
    666
    [6;6;6]
    "digitsOfInt 2: Regular case - the devil"
  ); 
  (fun () -> mkTest 
    digitsOfInt 
    0000000
    [] 
    "digitsOfInt 3: Lots of 0 = 0 -> []"
  ); 
  (fun () -> mkTest 
    digitsOfInt 
    1 
    [1]
    "digitsOfInt 4: Single Digit"
  ); 
  (fun () -> mkTest 
    digitsOfInt 
    90009000
    [9;0;0;0;9;0;0;0] 
    "digitsOfInt 5: Over 9000 with lots of 0s"
  ); 

  (fun () -> mkTest 
    digits
    123
    [1;2;3] 
    "digits 1 - regular case"
  ); 
  (fun () -> mkTest 
    digits
    (-666)
    [6;6;6]
    "digits 2 - negative devil"
  ); 
  (fun () -> mkTest 
    digits
    0
    []
    "digits 3 - input = 0"
  ); 
  (fun () -> mkTest 
    digits
    9000 
    [9;0;0;0]
    "digits 4 - exactly 9000"
  ); 
  (fun () -> mkTest 
    digits
    (-0)
    []
    "digits 5 - Minus 0??!"
  ); 
 
  (fun () -> mkTest 
    additivePersistence 
    1
    0
    "additivePersistence 1 - 0 additive persistence"
  ); 
  (fun () -> mkTest 
    additivePersistence 
    12 
    1
    "additivePersistence 2 - 1 additive persistence"
  ); 
  (fun () -> mkTest 
    additivePersistence 
    666
    2
    "additivePersistence 3 - 2 additive persistence"
  ); 
  (fun () -> mkTest 
    additivePersistence 
    999993
    3
    "additivePersistence 4 - 3 additive persistence"
  ); 
  (fun () -> mkTest 
    additivePersistence 
    (-10) 
    0
    "additivePersistence 5 - how robust is your code? Negative input"
  ); 

  (fun () -> mkTest 
     digitalRoot 
     12
     3 
     "digitalRoot 1 - regular case"
  );
  (fun () -> mkTest 
     digitalRoot 
     1 
     1 
     "digitalRoot 2 - single digit"
  );
  (fun () -> mkTest 
     digitalRoot 
     (-123) 
     0
     "digitalRoot 3 - negative number --> robustness test"
  );
  (fun () -> mkTest 
     digitalRoot 
     99993
     3
     "digitalRoot 4 - 3 level persistence"
  );
  (fun () -> mkTest 
     digitalRoot 
     90000000
     9
     "digitalRoot 5 - over 9000"
  );
 
  (fun () -> mkTest 
     palindrome
     "racecar"
     true 
     "palindrome 1 - true palindrome -> racecar"
  );
  (fun () -> mkTest 
     palindrome
     "raceme"
     false 
     "palindrome 2 - false palindrome"
  );
  (fun () -> mkTest 
     palindrome
     "122221" 
     true
     "palindrome 3 - true palindrome number"
  );
  (fun () -> mkTest 
     palindrome
     "10000001000"
     false
     "palindrome 4 - fail palindrome"
  );
  (fun () -> mkTest 
     palindrome
     "a"
     true
     "palindrome 5 - single palindrome"
  );

 (fun () -> mkTest 
     listReverse
     ["q";"u";"y";"n";"h"]
     ["h";"n";"y";"u";"q"]
     "listReverse 1 - regular case"
  );
  (fun () -> mkTest 
     listReverse
     [0;1;0;0;1]
     [1;0;0;1;0] 
     "listReverse 2 - number reversing"
  );
  (fun () -> mkTest 
     listReverse
     []
     []
     "listReverse 3 - Empty reversing"
  );
  (fun () -> mkTest 
     listReverse
     [0]
     [0]
     "listReverse 4 - single element on the list"
  );
  (fun () -> mkTest 
     listReverse
     [true; false]
     [false; true]
     "listReverse 5 - boolean palindrome"
  );
	
  ]
 ***************)
