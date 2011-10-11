type attrib = 
	| Name of string
	| Age of int
	| DOB of int * int * int
	| Address of string
	| Height of float
	| Alive of bool
	| Phone of int*int
	| Email of string;;
	
let matching xs =
	match xs with 
	|Name s -> 2
	| _ -> 5;;
	
let rec length x =
	match x with 
	| Nil -> 0
	| Cons(h,t) -> 1 + length t;;
	
let rec head l =
	match l with
	|Cons (h, _) -> h
	| _ -> failwith "Empty";;
	
let rec tail l =
	match l with 
	| Cons(_, t) -> t
	| _ -> failwith "hey";;
	
let rec append (l1, l2) =
	match l1 with
	|Nil -> l2
	|Cons (h,t) -> Cons (h, append (t, l2));;
	
type expr = 
	Float of float
	|Add of expr * expr
	|Sub of expr * expr
	|Mul of expr * expr
	
let rec eval e =
	match e with 
	| Add (e1,e2) -> eval e1 +. eval e2
	| Sub (e1,e2) -> eval e1 -. eval e2
	| Mul (e1,e2) -> eval e1 *. eval e2
	| Float n -> n;;