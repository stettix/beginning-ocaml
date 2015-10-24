#!/bin/sh
# A dumb little build script that builds an executable for all tests.
ocamlfind ocamlc -o runtest -package oUnit -linkpkg -g chapter_4.ml chapter_5.ml chapter_4_test.ml chapter_5_test.ml tests.ml
