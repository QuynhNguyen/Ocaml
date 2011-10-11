let rec exprToString e =
	match e with 
	| Thresh (e1,e2,e3,e4) -> exprToString e1
	| VarX -> e
	

# let sampleExpr1 = Thresh(VarX,VarY,VarX,(Times(Sine(VarX),Cosine(Average(VarX,VarY)))));;
- : expr =  ...

# exprToString sampleExpr1 
- : string = "(x<y?x:sin(pi*x)*cos(pi*((x+y)/2)))"

type expr = VarX 
          | VarY 
          | Sine of expr 
          | Cosine of expr 
          | Average of expr * expr 
          | Times of expr * expr 
          | Thresh of expr * expr * expr * expr 

e ::= x 
    | y 
    | sin (pi*e) 
    | cos (pi*e) 
    | ((e + e)/2) 
    | e * e 
    | (e<e ? e : e)


let rec exprToString e =
	match e with 
	|VarX -> "x"
	|VarY -> "y"
	|Cosine e -> "cos(pi*" ^ (exprToString e) ^ ")"
	|Sine e -> "sin(pi*" ^ (exprToString e) ^ ")"
	|Average (e1,e2) -> "((" ^ (exprToString e1) ^ "+" ^ (exprToString e2) ^ ")/2)"
	|Times (e1,e2) -> (exprToString e1) ^ "*" ^ (exprToString e2)
	|Thresh (e1,e2,e3,e4) -> "("  ^ (exprToString e1) ^ "<" ^ (exprToString e2) ^ "?" ^ (exprToString e3) ^ ":" ^ (exprToString e4) ^ ")"
	
	
	
let rec eval (e, f1, f2) =
	let pi = 4.0 *. atan 1.0 (*Finding PI*) in
		match e with 
		|VarX -> f1
		|VarY -> f2
		|Cosine e -> cos(pi*.(eval (e, f1, f2)))
		|Sine e -> sin(pi*.(eval (e,f1,f2)))
		|Average (e1,e2) -> (((eval (e1,f1,f2))+.(eval (e2,f1,f2))) /. (2.0))
		|Times (e1,e2) -> (eval (e1,f1,f2))*.(eval (e2,f1,f2))
		|Thresh (e1,e2,e3,e4) -> (if (eval (e1,f1,f2)) < (eval (e2,f1,f2)) then (eval (e3,f1,f2)) else (eval (e4,f1,f2)))