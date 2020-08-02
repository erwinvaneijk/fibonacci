;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; tests/lucas_test.lisp -- tests for the Lucas number implementation.
;;;
;;; Copyright (C) 2020, Erwin van Eijk <erwinvaneijk@gmail.com>
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

(deftest test-lucas-number-generators
  ; This test is here as a litmus test for prove.
  (testing "Test the + function"; the description
    (ok (= 0 (+ 0 0)))
    (ok (= 4 (+ 2 2)))
    (ok (= 5 (+ 2 3)))
    (ok (= 0 (+ 2 -2))))

  (testing "Test the Trivial Lucas number function."
    (ok (= 2 (fibonacci:lucas-trivial 0)))
    (ok (= 1 (fibonacci:lucas-trivial 1)))
    (ok (= 3 (fibonacci:lucas-trivial 2)))
    (ok (= 4 (fibonacci:lucas-trivial 3)))
    (ok (= 7 (fibonacci:lucas-trivial 4)))
    (ok (= 11 (fibonacci:lucas-trivial 5)))
    (ok (= 18 (fibonacci:lucas-trivial 6)))
    (ok (= 29 (fibonacci:lucas-trivial 7)))
    (ok (= 47 (fibonacci:lucas-trivial 8)))
    (ok (= 87403803 (fibonacci:lucas-trivial 38)))
    (ok (signals (fibonacci:lucas-trivial -1) 'fibonacci:mathematically-undefined)))

  (testing "Test the Less trivial Lucas number function."
    (ok (= 2 (fibonacci:lucas 0)))
    (ok (= 1 (fibonacci:lucas 1)))
    (ok (= 3 (fibonacci:lucas 2)))
    (ok (= 4 (fibonacci:lucas 3)))
    (ok (= 7 (fibonacci:lucas 4)))
    (ok (= 11 (fibonacci:lucas 5)))
    (ok (= 18 (fibonacci:lucas 6)))
    (ok (= 29 (fibonacci:lucas 7)))
    (ok (= 47 (fibonacci:lucas 8)))
    (ok (= 87403803 (fibonacci:lucas 38)))
    (ok (= 855741617674166096212819925691459689505708239 (fibonacci:lucas 215)))
    (ok (signals (fibonacci:lucas -1) 'fibonacci:mathematically-undefined))))
