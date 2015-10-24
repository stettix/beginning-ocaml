#!/bin/sh
# A dumb little build script that builds and runs all tests.
ocamlfind ocamlc -o test -package oUnit -linkpkg -g chapter_4.ml chapter_4_test.ml
