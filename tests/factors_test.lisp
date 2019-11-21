;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; tests/main_test.lisp -- tests for the Fibonacci implementation.
;;;
;;; Copyright (C) 2019, Erwin van Eijk <erwinvaneijk@gmail.com>
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.

(in-package :fibonacci/tests)

(deftest test-factors
  (testing "The the factors"
    (ok (equal '(0) (factors 0)))
    (ok (equal '(1) (factors 1)))
    (ok (equal '(2) (factors 2)))
    (ok (equal '(-1 2) (factors -2)))
    (ok (equal '(2 2) (factors 4)))
    (ok (equal '(2 47) (factors (* 2 47))))
    (ok (equal '(2 7) (factors (* 2 7))))
    (ok (equal '(3 3 3) (factors 27)))
    (ok (equal '(2 2 2 2 2 2) (factors 64)))
    (ok (equal '(389) (factors 389)))
    (ok (equal '(2 3 5 7 11 13 17 19 23 29 31) (factors (* 2 3 5 7 11 13 17 19 23 29 31))))
    (ok (equal '(2 2 2 2 2 2 2 3 3 7 23 47 769 1103 2207 3167) (factors 51680708854858323072)))
    (ok (equal (sieve-odds 1000) (factors (reduce #'* (sieve-odds 1000)))))
    (ok (equal '(193 389 3084989 361040209) (factors 83621143489848422977)))
    (pass "Clearing the cache.")
    (function-cache:clear-cache-all-function-caches)
    (ok (equal '(193 389 3084989 361040209) (factors 83621143489848422977)))))
