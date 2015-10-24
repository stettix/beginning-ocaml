open OUnit2;;
open Chapter_4;;
open Chapter_5;;

let l1 = [1; 5; 3; 11; 12]
let l2 = [2; 6; 4]

let test_msort test_ctxt =
	assert_equal [] (msort []);
	assert_equal [1; 3; 5; 11; 12] (msort l1);
	assert_equal [2; 4; 6] (msort l2);;

let test_sort test_ctxt =
	assert_equal [] (sort []);
	assert_equal [1; 3; 5; 11; 12] (sort l1);
	assert_equal [2; 4; 6] (sort l2);;

let test_rev_sort test_ctxt =
	assert_equal [] (rev_sort []);
	assert_equal [12; 11; 5; 3; 1] (rev_sort l1);
	assert_equal [6; 4; 2] (rev_sort l2);;

let test_is_sorted test_ctxt =
	assert_equal true (is_sorted []);
	assert_equal true (is_sorted [1]);
	assert_equal false (is_sorted l1);
	assert_equal false (is_sorted l2);
	assert_equal true (is_sorted (sort l1));
	assert_equal true (is_sorted (sort l2));;

let suite =
  "Chapter 5 test suite">:::
    ["test msort">:: test_msort;
    "test_sort">:: test_sort;
    "test_rev_sort">:: test_rev_sort;
    "test_is_sorted">:: test_is_sorted]
  ;;
