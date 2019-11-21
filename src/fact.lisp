;;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; fact.lisp --- Trivial implementation of the factorial of n
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

(in-package #:fibonacci)

(defun slowfact (n)
  "Trivial factorial of N.

This is a trivial non-tail recursive version of the factorial."
  (cond ((< n 0) (error 'mathematically-undefined :num n))
        ((= n 0) 1)
        ((= n 1) 1)
        (t (* n (fact (1- n))))))

(defun slowfact-better-part (n x)
  "Compute the factorial of N with temporary X.

This version computes the factorial in a tail-recursive fashion."
  (cond ((< n 0) (error 'mathematically-undefined :num n))
        ((< n 1) x)
        (t (slowfact-better-part (1- n) (* n x)))))

(defun slowfact-better (n)
  (slowfact-better-part n 1))

(defun split-recursive-factorial (n)
  "Compute a factorial with the split recursive algorithm.

Based on Peter Luschny's code
(http://www.luschny.de/math/factorial/LispFactorial.html),

which was rewritten in Lisp by Richard Fateman
(https://people.eecs.berkeley.edu/~fateman/papers/factorial.pdf)"
  (let ((p 1)
        (r 1)
        (NN 1)
        (log2n (floor (log n 2)))
        (h 0)
        (shift 0)
        (high 0)
        (len 0))
    (labels
        ((prod (n)
           (declare (fixnum n))
           (let ((m (ash n -1)))
             (cond ((= m 0) (incf NN 2))
                   ((= n 2) (* (incf NN 2) (incf NN 2)))
                   (t (* (prod (- n m)) (prod m)))))))
      (loop while (/= h n) do
           (incf shift h)
           (setf h (ash n (- log2n)))
           (decf log2n)
           (setf len high)
           (setf high (if (oddp h) h (1- h)))
           (setf len (ash (- high len) -1))
           (cond ((> len 0)
                  (setf p (* p (prod len)))
                  (setf r (* r p)))))
      (ash r shift))))

(defun fact (n)
  (cond ((< n 0) (error 'mathematically-undefined :num n))
        ((= n 0) 1)
        ((= n 1) 1)
        (t (split-recursive-factorial n))))
