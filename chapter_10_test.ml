open OUnit2;;
open Chapter_10;;

let test_area test_ctxt =
  assert_equal 9 (area (Square(3)));
  assert_equal 12 (area (Rectangle(3, 4)))

let test_portrait test_ctxt =
  assert_equal (Rectangle(3, 4)) (portrait (Rectangle(3, 4)));
  assert_equal (Rectangle(3, 4)) (portrait (Rectangle(4, 3)));
  assert_equal (Square(3)) (portrait (Square(3)))

let test_ordered_rects test_ctxt =
  let r1 = Rectangle(3, 4) in
  let r2 = Rectangle(6, 5) in
    assert_equal [] (ordered_rects []);
    assert_equal [r1] (ordered_rects [r1]);
    assert_equal [r1] (ordered_rects [Rectangle(4, 3)]);
    assert_equal [r1; Rectangle(5, 6)] (ordered_rects [r1; r2]);
    assert_equal [r1; Rectangle(5, 6)] (ordered_rects [r2; r1])

let s1 = (Cons (1, Nil))
let s2 = (Cons (1, Cons (2, Cons (3, Nil))))

let test_take test_ctxt =
  assert_equal Nil (take 1 Nil);
  assert_equal Nil (take 0 s1);
  assert_equal s1 (take 1 s1);
  assert_equal s1 (take 2 s1);
  assert_equal s1 (take 1 s2);
  assert_equal s1 (take 1 (take 2 s2))

let test_drop test_ctxt =
  assert_equal Nil (drop 0 Nil);
  assert_equal Nil (drop 1 Nil);
  assert_equal s1 (drop 0 s1);
  assert_equal s2 (drop 0 s2);
  assert_equal Nil (drop 1 s1);
  assert_equal Nil (drop 3 s2);
  assert_equal (Cons(3, Nil)) (drop 2 s2)

let test_map test_ctxt =
  let add1 n = n + 1 in
    assert_equal Nil (map add1 Nil);
    assert_equal (Cons (2, Nil)) (map add1 s1);
    assert_equal (Cons (2, Cons (3, Cons (4, Nil)))) (map add1 s2)

let suite =
  "Chapter 10 test suite">:::
    ["test area">:: test_area;
    "test portrait">:: test_portrait;
    "test ordered_rects">:: test_ordered_rects;
    "test take">:: test_take;
    "test drop">:: test_drop;
    "test map">:: test_map;]
