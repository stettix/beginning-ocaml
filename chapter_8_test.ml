open OUnit2;;
open Chapter_8;;

let d1 = [1,1; 2,2; 3,3; 4,4]
let d2 = [1,2; 2,3; 3,4]

let test_numkeys test_ctxt =
  assert_equal 0 (numkeys []);
  assert_equal 4 (numkeys d1);
  assert_equal 3 (numkeys d2);;

let test_replace test_ctxt =
  assert_equal d2 (replace 1 2 d2);
  assert_equal d2 (replace 2 3 d2);
  assert_raises Not_found (fun () -> replace 0 0 d1);;

let test_dict_of_lists test_ctxt =
  assert_equal [] (dict_of_lists [] []);
  assert_equal [1, 10] (dict_of_lists [1] [10]);
  assert_equal [1, 10; 2, 20; 3, 30] (dict_of_lists [1; 2; 3] [10; 20; 30]);
  assert_raises (Invalid_argument "Lists not of equal size") (fun () -> dict_of_lists [] [1]);;

let test_split test_ctxt =
  assert_equal ([], []) (split []);
  assert_equal ([1], [10]) (split [1, 10]);
  assert_equal ([1; 2; 3], [10; 20; 30]) (split [1, 10; 2, 20; 3, 30]);;

let test_dict_of_pairs test_ctxt =
  assert_equal [] (dict_of_pairs []);
  assert_equal [1, 10] (dict_of_pairs [1, 10]);
  assert_equal [1, 10; 2, 20] (dict_of_pairs [1, 10; 2, 20]);
  assert_equal [1, 11] (dict_of_pairs [1, 11; 1, 10]);
  assert_equal [1, 10; 2, 20] (dict_of_pairs [1, 10; 2, 20; 1, 11]);;

let suite =
  "Chapter 8 test suite">:::
    ["test numkeys">:: test_numkeys;
     "test replace">:: test_replace;
     "test dict_of_lists">:: test_dict_of_lists;
     "test test_split">:: test_split;
     "test test_dict_of_pairs">:: test_dict_of_pairs]
