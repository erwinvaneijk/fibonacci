;;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; lucas.lisp --- Implementation of the computation of the n-th Lucas
;;;                number. It contains two implementations, one trivial
;;;                and one that implements the fast doubling method.
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

(in-package :fibonacci)

(defun lucas-trivial (n)
  "Trivial recursive Lucas number implementation of N."
  (cond
    ((< n 0) (error 'mathematically-undefined :num n))
    ((= n 0) 2)
    ((= n 1) 1)
    (t (+ (lucas (- n 1)) (lucas (- n 2))))))

(defun lucas (n)
  "Trivial recursive Lucas number implementation of N.

   This code uses the relation that L(n) = Fib(n-1) + Fib(n+1)"
  (cond
    ((< n 0) (error 'mathematically-undefined :num n))
    ((= n 0) 2)
    ((= n 1) 1)
    (t (+ (fib (- n 1)) (fib (+ n 1))))))
