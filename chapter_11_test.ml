open OUnit2;;
open Chapter_11;;

let tr0 = Br (1, Lf, Lf)
let tr1 = Br (2, Br (1, Lf, Lf), Br (4, Lf, Lf))

let test_contains test_ctxt =
  assert_equal false (contains 42 Lf);
  assert_equal false (contains 42 tr1);
  assert_equal true (contains 2 tr1);
  assert_equal true (contains 1 tr1);
  assert_equal true (contains 4 tr1)

let test_flip test_ctxt =
  assert_equal Lf (flip Lf);
  assert_equal tr0 (flip tr0);
  assert_equal tr1 (flip (flip tr1));
  assert_equal (Br (2, Br (4, Lf, Lf), Br (1, Lf, Lf))) (flip tr1)

let test_eq_shape test_ctxt =
  assert_equal true (eq_shape Lf Lf);
  assert_equal false (eq_shape Lf tr0);
  assert_equal false (eq_shape tr0 Lf);
  assert_equal true (eq_shape tr0 (Br (42, Lf, Lf)));
  assert_equal true (eq_shape tr1 (Br ("2", Br ("1", Lf, Lf), Br ("4", Lf, Lf))))

let suite =
  "Chapter 11 test suite">:::
    ["test contains">:: test_contains;
    "test flip">:: test_flip;
    "test eq_shape">:: test_eq_shape]
