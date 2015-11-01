open OUnit2;;
open Chapter_10;;

let test_area test_ctxt =
  assert_equal 9 (area (Square(3)));
  assert_equal 12 (area (Rectangle(3, 4)))

let test_portrait test_ctxt =
	assert_equal 4 (2 + 2);
  assert_equal (Rectangle(3, 4)) (portrait (Rectangle(3, 4)));
  assert_equal (Rectangle(3, 4)) (portrait (Rectangle(4, 3)));
  assert_equal (Square(3)) (portrait (Square(3)))

let suite =
  "Chapter 10 test suite">:::
    ["test area">:: test_area;
    "test portrait">:: test_portrait]
