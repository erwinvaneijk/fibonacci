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

(deftest test-fibonacci-number-generators
  ; This test is here as a litmus test for prove.
  (testing "Test the + function"; the description
    (ok (= 0 (+ 0 0)))
    (ok (= 4 (+ 2 2)))
    (ok (= 5 (+ 2 3)))
    (ok (= 0 (+ 2 -2))))

  (testing "Test the Trivial Fibonacci function."
    (ok (= 0 (fibonacci:slowfib 0)))
    (ok (= 1 (fibonacci:slowfib 1)))
    (ok (= 1 (fibonacci:slowfib 2)))
    (ok (= 2 (fibonacci:slowfib 3)))
    (ok (= 5 (fibonacci:slowfib 5)))
    (ok (= 89 (fibonacci:slowfib 11)))
    (ok (signals (fibonacci:slowfib -1) 'fibonacci:mathematically-undefined)))

  (testing "Test the Fast Fibonacci function."
    (ok (= 0 (fibonacci:fib 0)))
    (ok (= 1 (fibonacci:fib 1)))
    (ok (= 1 (fibonacci:fib 2)))
    (ok (= 2 (fibonacci:fib 3)))
    (ok (= 5 (fibonacci:fib 5)))
    (ok (= 89 (fibonacci:fib 11)))
    (ok (= 2971215073 (fibonacci:fib 47)))
    (ok (= 1779979416004714189 (fibonacci:fib 89)))
    (ok (signals (fibonacci:fib -1) 'fibonacci:mathematically-undefined))))

