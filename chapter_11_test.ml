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

let test_ntrees test_ctxt =
  let times2 x = x * 2 in
    let ntree1 = NBr ([1; 2; 3], NBr ([], NLf, NLf), NBr ([4], NLf, NLf)) in
      assert_equal 0 (ntree_size NLf);
      assert_equal 4 (ntree_size ntree1);
      assert_equal NLf (ntree_map times2 NLf);
      assert_equal (NBr ([2; 4; 6], NBr ([], NLf, NLf), NBr ([8], NLf, NLf))) (ntree_map times2 ntree1)

let suite =
  "Chapter 11 test suite">:::
    ["test contains">:: test_contains;
    "test flip">:: test_flip;
    "test eq_shape">:: test_eq_shape;
    "test trees with a variable number of elements at each level">:: test_ntrees]
