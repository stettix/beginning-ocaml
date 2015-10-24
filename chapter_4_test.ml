open OUnit2;;
open Chapter_4;;

let l = [1; 2; 3]

let test_rev test_ctxt =
	assert_equal [] (rev []);
	assert_equal [3; 2; 1] (rev l);;

let test_palindrome test_ctxt =
	assert_equal [] (palindrome []);
	assert_equal [1; 2; 3; 2; 1] (palindrome l);;

(* Name the test cases and group them together *)
let suite =
  "Chapter 4 test suite">:::
    ["test rev">:: test_rev;
   	 "test palindrome">:: test_palindrome]
  ;;

let () =
  run_test_tt_main suite
;;
