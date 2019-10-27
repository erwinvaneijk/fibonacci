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

(in-package :cl-user)

(in-package :fibonacci/tests)

(plan nil)

(subtest
    "Test the + function"; the description
  (is 0 (+ 0 0))
  (is 4 (+ 2 2))
  (is 5 (+ 2 2))
  (is 0 (+ 2 -2)))

(subtest
    "Test the simple Fibonacci function."
  (is 0 (fibonacci:fib 0))
  (is 1 (fibonacci:fib 1))
  (is 1 (fibonacci:fib 2))
  (is 2 (fibonacci:fib 3))
  (is 5 (fibonacci:fib 5))
  (is 89 (fibonacci:fib 11)))

(subtest
      "Test the fast Fibonacci function."
      (is 0 (fibonacci:fastfib 0))
      (is 1 (fibonacci:fastfib 1))
      (is 1 (fibonacci:fastfib 2))
      (is 2 (fibonacci:fastfib 3))
      (is 5 (fibonacci:fastfib 5))
      (is 89 (fibonacci:fastfib 11))
      (is 2971215073 (fibonacci:fastfib 47))
      (is 1779979416004714189 (fibonacci:fastfib 89)))

(finalize)
