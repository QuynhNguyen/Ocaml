(* CSE 130 PA 1. Autotester *)

#use "misc.ml"
#use "tester.ml" 

let sampleTests =
  [
  (fun () -> mkTest
    sumList
    [1;2;3;4]
    10
    "sample: sumList 1"
  );
  (fun () -> mkTest 
    sumList 
    [1;-2;3;5] 
    7 
    "sample: sumList 2"
  ); 
  (fun () -> mkTest 
    sumList 
    [1;3;5;7;9;11]
    36 
    "sample: sumList 3"
  ); 
  (fun () -> mkTest 
    digitsOfInt 
    3124 
    [3;1;2;4] 
    "sample: digitsOfInt 1"
  ); 
  (fun () -> mkTest 
    digitsOfInt 
    352663 
    [3;5;2;6;6;3] 
    "sample: digitsOfInt 2"
  ); 
  (fun () -> mkTest 
    digits
    31243
    [3;1;2;4;3] 
    "sample: digits 1"
  ); 
  (fun () -> mkTest 
    digits
    (-23422)
    [2;3;4;2;2]
    "sample: digits 2"
  ); 
  (fun () -> mkTest 
    additivePersistence 
    9876 
    2 
    "sample: additivePersistence1"
  ); 
  (fun () -> mkTest 
    digitalRoot 
    9876 
    3 
    "sample: digitalRoot"
  ); 
  (fun () -> mkTest 
    listReverse
    [1;2;3;4] 
    [4;3;2;1]
    "sample: reverse 1"
  ); 
  (fun () -> mkTest 
    listReverse 
    ["a";"b";"c";"d"]
    ["d";"c";"b";"a"] 
    "sample: rev 2"
  ); 
  (fun () -> mkTest 
    palindrome 
    "malayalam" 
    true
    "sample: palindrome 1"
  ); 
  (fun () -> mkTest 
    palindrome 
    "myxomatosis" 
    false
    "sample: palindrome 2"
  )] 

(*130*************************************************************)
(*130**************** BEGIN MODIFY *******************************)
(*130*************************************************************)

let yourTests = 
  [ 
  (fun () -> mkTest 
    sumList 
    [] 
    0 
    "sumList 1"
  ); 
  (fun () -> mkTest 
    sumList 
    [10;9;8;7;6;5;4;3;2;1] 
    55
    "sumList 2"
  ); 
  (fun () -> mkTest 
    sumList 
    [-1;-2;-3;-4;-5]
    (-15)
    "sumList 3"
  ); 
  (fun () -> mkTest 
    sumList 
    [-1;2;-3;4;-5]
    (-3)
    "sumList 4"
  ); 
  (fun () -> mkTest 
    sumList 
    [1;-2;3;-4;5]
    3 
    "sumList 5"
  );

  (fun () -> mkTest 
    digitsOfInt 
    1000000
    [1;0;0;0;0;0;0] 
    "digitsOfInt 1"
  ); 
  (fun () -> mkTest 
    digitsOfInt 
    (-12345)
    [] 
    "digitsOfInt 2"
  ); 
  (fun () -> mkTest 
    digitsOfInt 
    1020304050 
    [1;0;2;0;3;0;4;0;5;0] 
    "digitsOfInt 3"
  ); 
  (fun () -> mkTest 
    digitsOfInt 
    7000007 
    [7;0;0;0;0;0;7]
    "digitsOfInt 4"
  ); 
  (fun () -> mkTest 
    digitsOfInt 
    00000001234
    [1;2;3;4] 
    "digitsOfInt 5"
  ); 

  (fun () -> mkTest 
    digits
    (-70007) 
    [7;0;0;0;7] 
    "digits 1"
  ); 
  (fun () -> mkTest 
    digits
    (-10000000) 
    [1;0;0;0;0;0;0;0] 
    "digits 2"
  ); 
  (fun () -> mkTest 
    digits
    (-302010) 
    [3;0;2;0;1;0] 
    "digits 3"
  ); 
  (fun () -> mkTest 
    digits
    (143) 
    [1;4;3]
    "digits 4"
  ); 
  (fun () -> mkTest 
    digits
    (-3410001) 
    [3;4;1;0;0;0;1]
    "digits 5"
  ); 
 
  (fun () -> mkTest 
    additivePersistence 
    9999
    2
    "additivePersistence 1"
  ); 
  (fun () -> mkTest 
    additivePersistence 
    0
	0
    "additivePersistence 2"
  ); 
  (fun () -> mkTest 
    additivePersistence 
    9
	0
    "additivePersistence 3"
  ); 
  (fun () -> mkTest 
    additivePersistence 
    10
	1
    "additivePersistence 4"
  ); 
  (fun () -> mkTest 
    additivePersistence 
    (-23)
	0
    "additivePersistence 5"
  ); 

  (fun () -> mkTest 
     digitalRoot 
     23 
     5 
     "digitalRoot 1"
  );
  (fun () -> mkTest 
     digitalRoot 
	 3
	 3
     "digitalRoot 2"
  );
  (fun () -> mkTest 
     digitalRoot 
	 9102
	 3
     "digitalRoot 3"
  );
  (fun () -> mkTest 
     digitalRoot 
     (-40)
	 0
     "digitalRoot 4"
  );
  (fun () -> mkTest 
     digitalRoot 
     123456789
	 9
     "digitalRoot 5"
  );
  (fun () -> mkTest 
     palindrome
     "poop"
	 true
     "palindrome 1"
  );
  (fun () -> mkTest 
     palindrome
     "RacecaR"
     true
	 "palindrome 2"
  );
  (fun () -> mkTest 
     palindrome
     "a  b  a"
	 true
     "palindrome 3"
  );
  (fun () -> mkTest 
     palindrome
     "    "
	 true
     "palindrome 4"
  );
  (fun () -> mkTest 
     palindrome
     "a b  c b a"
	 false
     "palindrome 5"
  );
  ]

(*130*************************************************************)
(*130**************** END MODIFY *********************************)
(*130*************************************************************)

let doTest f = 
  try f () with ex -> 
    Printf.sprintf "WARNING: INVALID TEST THROWS EXCEPTION!!: %s \n\n"
    (Printexc.to_string ex)

let _ =
  let report = List.map doTest (sampleTests @ yourTests) in
  let _ = List.iter print130 (report@([scoreMsg()])) in
  let _ = print130 ("Compiled\n") in
  (!score, !max)

