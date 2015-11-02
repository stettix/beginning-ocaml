open OUnit2;;
open Chapter_4_test;;
open Chapter_5_test;;
open Chapter_6_test;;

let () =
  run_test_tt_main Chapter_4_test.suite;
  run_test_tt_main Chapter_5_test.suite;
  run_test_tt_main Chapter_6_test.suite;
  run_test_tt_main Chapter_7_test.suite;
  run_test_tt_main Chapter_8_test.suite;
  run_test_tt_main Chapter_9_test.suite;
  run_test_tt_main Chapter_10_test.suite;
  run_test_tt_main Chapter_11_test.suite;
;;
