(* CSE 130 PA 2. Autotester *)

#use "misc.ml"
#use "expr.ml" 
#use "art.ml"
#use "tester.ml" 

let sampleTests =
  [
  (fun () -> mkTest
     assoc
     (-1, "william", [("ranjit",85);("william",23);("moose",44)])
     23
     "sample: assoc 1"
  );
  (fun () -> mkTest 
    assoc
    (-1, "bob", [("ranjit",85);("william",23);("moose",44)])
    (-1)
    "sample: assoc 2"
  ); 
  (fun () -> mkTest 
    removeDuplicates
    [1;6;2;4;12;2;13;6;9]
    [1;6;2;4;12;13;9]
    "sample: removeDuplicates 2"
  );
  (fun () -> mkTest 
    removeDuplicates
    [1;1;1]
    [1]
    "sample: removeDuplicates 2"
  );

  (fun () -> mkTest 
    wwhile 
    ((fun x -> let xx = x*x*x in (xx, xx < 100)), 2) 
    512 
    "sample: wwhile 1"
  ); 
  (fun () -> mkTest 
	fixpoint
    ((fun x -> truncate (1e6 *. cos (1e-6 *. float x))), 0)
    739085
    "sample: fixpoint 1"
  ); 
 
 (fun () -> mkTest 
   emitGrayscale
   (eval_fn sampleExpr, 150,"sample")
   ()
   "sample: eval_fn 1: manual"
 ); 
 (fun () -> mkTest 
   emitGrayscale
   (eval_fn sampleExpr2, 150,"sample2")
   ()
   "sample: eval_fn 2: manual"
 );
 
 (fun () -> mkTest 
   (fun () -> doRandomGray (g1 ()))
   ()
   ()
   "sample: gray 1 : manual"
 );
 (fun () -> mkTest 
   (fun () -> doRandomGray (g2 ()))
   ()
   ()
   "sample: gray 2 : manual"
 );
 (fun () -> mkTest 
   (fun () -> doRandomGray (g3 ()))
   ()
   ()
   "sample: gray 3 : manual"
 );

 (fun () -> mkTest 
   (fun () -> doRandomColor (c1 ()))
   ()
   ()
   "sample: color 1 : manual"
 );
 (fun () -> mkTest 
   (fun () -> doRandomColor (c2 ()))
   ()
   ()
   "sample: color 2 : manual"
 );
 (fun () -> mkTest 
   (fun () -> doRandomColor (c3 ()))
   ()
   ()
   "sample: color 3 : manual"
 )] 

(*130*************************************************************)
(*130**************** BEGIN MODIFY *******************************)
(*130*************************************************************)

let yourTests = 
  [ 
  (fun () -> mkTest
     assoc
     (-1, "quynh", [("quynh",1);("tom",2);("jerry",3)])
     1 
     "assoc 1 - regular case"
  );
  (fun () -> mkTest
     assoc
     (-1, "unknown", [("known",4);("annon",5);("mystery",6)]) 
    ( -1 )
     "assoc 2 - not found"
  );
  (fun () -> mkTest
     assoc
     (-1, "QUYNH", [("quynh",1);("qUynH",2);("hnyuq",3)])
     (-1)
     "assoc 3 - case SenSiTive"
  );

  (fun () -> mkTest
     assoc
     (-1, "", [("",11);(" ",22);("[]",33)])
     11
     "assoc 4 - Empty String"
  );

  (fun () -> mkTest
     assoc
    (-1, "quynh", [("quynh",1);("quynh",2);("quynh",3)])
     1
     "assoc 5 - Duplication"
  );

  (fun () -> mkTest 
    removeDuplicates
   [1;2;3;3;4;5;6;6;7]
   [1;2;3;4;5;6;7] 
    "removeDuplicates 1 - normal case"
  );
  (fun () -> mkTest 
    removeDuplicates
    [0;0;0;0;0] 
    [0] 
     "removeDuplicates 2 - nothing but 0s"
  );
  (fun () -> mkTest 
    removeDuplicates
    [1;2;2;2;2;2;2;2;2;2;3] 
    [1;2;3]
     "removeDuplicates 3 - the middle guy"
  );
  (fun () -> mkTest 
    removeDuplicates
    []
    []
     "removeDuplicates 4 - empty list"
  );
  (fun () -> mkTest 
    removeDuplicates
    [1;2;3;4]
    [1;2;3;4]
     "removeDuplicates 5 - nothing to remove"
  );

  (fun () -> mkTest 
    wwhile 
    ((fun x -> let xx = x*x in (xx, xx < 100)), 2) 
    256
    "wwhile 1 - double X"
  );
  (fun () -> mkTest 
    wwhile 
    ((fun x -> let xx = (x*x*x*x) in (xx, xx < 100)), 2)  
    65536
    "wwhile 2 - quad x"
  );
  (fun () -> mkTest 
    wwhile 
    ((fun x -> let xx = x*x*x in (xx, xx < 100)), 3) 
    19683
    "wwhile 3 - 3 as base - hattrick"
  );
(fun () -> mkTest 
    wwhile 
    ((fun x -> let xx = x*x in (xx, xx < 100)), 3) 
    6561
    "wwhile 4 - double tap base 3"
  );
(fun () -> mkTest 
    wwhile 
    ((fun x -> let xx = x*x in (xx, xx < 1)), 1) 
    1
    "wwhile 5 - 1"
  );
  
  (fun () -> mkTest 
    fixpoint 
    ((fun x -> truncate (1e2 *. sin (1e-6 *. float x))), 2)
    0 
    "fixpoint 1"
  );
  (fun () -> mkTest 
    fixpoint 
    ((fun x -> truncate (1e3 *. cos (1e-6 *. float x))), 0)
    999
    "fixpoint 2"
  );
  (fun () -> mkTest 
    fixpoint 
    ((fun x -> truncate (1e3 *. tan (1e-6 *. float x))), 0)
    0
    "fixpoint 3"
  );
(fun () -> mkTest 
    fixpoint 
    ((fun x -> truncate (1e3 *. cos (1e-3 *. float x))), 0)
    739
    "fixpoint 4"
  );
(fun () -> mkTest 
    fixpoint 
    ((fun x -> truncate (1e3 +. cos (1e-3 *. float x))), 0)
    1000
    "fixpoint 5"
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

