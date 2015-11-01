#!/bin/sh
# A dumb little build script that builds an executable for all tests.
ocamlfind ocamlc -o runtest -package oUnit -linkpkg -g \
  chapter_4.ml chapter_5.ml chapter_4_test.ml chapter_5_test.ml chapter_6.ml chapter_6_test.ml \
  chapter_7.ml chapter_7_test.ml chapter_8.ml chapter_8_test.ml chapter_9.ml chapter_9_test.ml \
  chapter_10.ml chapter_10_test.ml tests.ml
