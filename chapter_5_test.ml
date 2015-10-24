open OUnit2;;
open Chapter_4;;
open Chapter_5;;

let l1 = [1; 5; 3; 11; 12]
let l2 = [2; 6; 4]

let test_msort test_ctxt =
	assert_equal [] (msort []);
	assert_equal [1; 3; 5; 11; 12] (msort l1);
	assert_equal [2; 4; 6] (msort l2);;

let suite =
  "Chapter 5 test suite">:::
    ["test msort">:: test_msort]
  ;;
