open OUnit2;;
open Chapter_4;;
open Chapter_9;;

let l1 = [1; 2; 3; 4]
let l2 = [1; 5; 11]

let test_member_all test_ctxt =
  assert_equal false (member_all 0 [l1; l2]);
  assert_equal false (member_all 3 [l1; l2]);
  assert_equal true (member_all 1 [l1; l2])

let lll = [[l1]; [l1; l2]; []]
let times2 x = x * 2
let l1x2 = [2; 4; 6; 8]
let l2x2 = [2; 10; 22]

let test_mapll test_ctxt =
  assert_equal [] (mapll times2 []);
  assert_equal [[l1x2]; [l1x2; l2x2]; []] (mapll times2 lll)

let ll = [l1; l2; []]

let test_truncate test_ctxt =
  assert_equal ll (truncate 4 ll);
  assert_equal [[1; 2; 3]; [1; 5; 11]; []] (truncate 3 ll);
  assert_equal [[1]; [1]; []] (truncate 1 ll);
  assert_equal [[]; []; []] (truncate 0 ll);
  assert_equal [] (truncate 3 [])

let test_heads test_ctxt =
  assert_equal [] (heads 42 []);
  assert_equal [1; 1] (heads 42 [l1; l2]);
  assert_equal [1; 42; 1] (heads 42 [l1; []; l2])

let suite =
  "Chapter 9 test suite">:::
    ["test member_all">:: test_member_all;
     "test mapll">:: test_mapll;
     "test test_truncate">:: test_truncate;
     "test test_heads">:: test_heads];;
