open OUnit2;;
open Chapter_4;;
open Chapter_5;;
open Chapter_6;;
open Chapter_7;;

let l1 = [1; 5; -3; 11; 12]

let test_smallest test_ctxt =
  assert_equal (-3) (smallest l1);
  assert_raises Not_found (fun () -> smallest []);;

let test_smallest_or_zero test_ctxt =
  assert_equal (-3) (smallest_or_zero l1);
  assert_equal (-3) (smallest_or_zero l1);;

let test_intsqrt test_ctxt =
  assert_equal 2 (intsqrt 4);
  assert_equal 2 (intsqrt 5);
  assert_raises Negative_argument (fun () -> intsqrt (-1));;

let suite =
  "Chapter 7 test suite">:::
    ["test smallest">:: test_smallest;
    "test smallest_or_zero">:: test_smallest_or_zero;
    "test intsqrt">:: test_intsqrt]
