;;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; fibonacci.lisp --- Implementation of the computation of the n-th Fibonacci
;;;                    number. It contains two implementations, one trivial
;;;                    and one that implements the fast doubling method.
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

(defun fib (n)
  "Trivial recursive Fibonacci number implementation of N."
  (cond
    ((<= n 0) 0)
    ((= n 1) 1)
    (t (+ (fib (- n 1)) (fib (- n 2))))))

(defun fastfib-part (n)
  "Implement a way of doing the fast doubling."
  (if (eq n 0)
      (cons 0 1)
      (let* ((r (fastfib-part (nth-value 0 (floor n 2))))
             (a (car r))
             (b (cdr r))
             (c (* a (- (* b 2) a)))
             (d (+ (* a a) (* b b))))
           (if (= 0 (mod n 2))
               (cons c d)
               (cons d (+ c d))))))

(defun fastfib (n)
  (car (fastfib-part n)))
