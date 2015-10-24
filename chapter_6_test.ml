open OUnit2;;
open Chapter_4;;
open Chapter_5;;
open Chapter_6;;

let l1 = [1; 5; -3; 11; 12]
let l2 = [2; 6; 4]

let add1 = ((+) 1)
let prepend_foo x = String.concat "" ["foo"; x]
let lt5 x = if x < 5 then true else false

let test_cliplist test_ctxt =
  assert_equal [] (cliplist []);
  assert_equal [1; 5; 1; 10; 10] (cliplist l1);;

let test_apply test_ctxt =
  assert_equal 10 (apply add1 0 10);
  assert_equal 11 (apply add1 1 10);
  assert_equal 13 (apply add1 3 10);
  assert_equal "foobar" (apply prepend_foo 1 "bar");
  assert_equal "foofoofoobar" (apply prepend_foo 3 "bar");;

let test_filter test_ctxt =
  assert_equal [] (filter lt5 []);
  assert_equal [1; -3] (filter lt5 l1);
  assert_equal [] (filter lt5 [6; 7; 8]);;

let test_for_all test_ctxt =
  assert_equal true (for_all lt5 []);
  assert_equal true (for_all lt5 [2; 3; 4]);
  assert_equal false (for_all lt5 [3; 4; 5]);
  assert_equal false (for_all lt5 [3; 4; 5; 2]);;  

let test_mapl test_ctxt =
  assert_equal [] (mapl add1 []);
  assert_equal [[]] (mapl add1 [[]]);
  assert_equal [[1]] (mapl add1 [[0]]);
  assert_equal [[2; 3]; [4; 5]] (mapl add1 [[1; 2]; [3; 4]]);;

let suite =
  "Chapter 6 test suite">:::
    ["test cliplist">:: test_cliplist;
     "test apply">:: test_apply;
     "test filter">:: test_filter;
     "test for_all">:: test_for_all;
     "test mapl">:: test_mapl]
  ;;
