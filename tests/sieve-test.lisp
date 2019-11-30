;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; tests/sieve-test.lisp -- tests for the Sieve.
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

(defun is-prime-p (n)
  "Returns true if N is prime."
  (if (<= n 1)
      nil
      (notany (lambda (i) (= (mod n i) 0))
              (loop :for i :from 2 :to (sqrt n) :collect i))))

(deftest test-fibonacci-number-generators
  ; This test is here as a litmus test for prove.
  (testing "Test sieve"
    (ok (equal '(2) (sieve-odds 2)))
    (ok (equal '(2 3 5) (sieve-odds 5)))
    (ok (every 'is-prime-p (sieve-odds 100)))))
