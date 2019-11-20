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

(deftest test-factorial
  (testing "Test Factorial"
    (ok (= 1 (fibonacci:fact 0)))
    (ok (= 1 (fibonacci:fact 1)))
    (ok (= 2 (fibonacci:fact 2)))
    (ok (= 40320 (fibonacci:fact 8)))
    (ok (= 8683317618811886495518194401280000000 (fibonacci:fact 33)))
    (ok (= 77338 (length (write-to-string (fibonacci:fact 20000)))))
    (ok (signals (fibonacci:fact -1) 'fibonacci:mathematically-undefined))))

(deftest test-trivial-factorial
  (testing "Test Trivial Factorial"
    (ok (= 1 (fibonacci:slowfact 0)))
    (ok (= 1 (fibonacci:slowfact 1)))
    (ok (= 2 (fibonacci:slowfact 2)))
    (ok (= 40320 (fibonacci:slowfact 8)))
    (ok (signals (fibonacci:fact -1) 'fibonacci:mathematically-undefined))))

(deftest test-better-factorial
  (testing "Test Better Trivial Factorial"
    (ok (= 1 (fibonacci:slowfact-better 0)))
    (ok (= 1 (fibonacci:slowfact-better 1)))
    (ok (= 2 (fibonacci:slowfact-better 2)))
    (ok (= 40320 (fibonacci:slowfact 8)))
    (ok (signals (fibonacci:fact -1) 'fibonacci:mathematically-undefined))))
