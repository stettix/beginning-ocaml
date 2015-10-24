open OUnit2;;
open Chapter_4_test;;
open Chapter_5_test;;

let () =
  run_test_tt_main Chapter_4_test.suite;
  run_test_tt_main Chapter_5_test.suite
;;
