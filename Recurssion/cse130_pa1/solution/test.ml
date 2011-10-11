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
     ""
     true
     "palindrome 4 - empty palindrome"
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

