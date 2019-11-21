;;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; sieve.lisp --- Contains the sieve of Erostatenes
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

(defun sieve-odds (maximum)
  "Prime numbers sieve for odd numbers.
   Returns a list with all the primes that are less than or equal to maximum."
  (loop :with maxi = (ash (1- maximum) -1)
        :with stop = (ash (isqrt maximum) -1)
        :with sieve = (make-array (1+ maxi) :element-type 'bit :initial-element 0)
        :for i :from 1 :to maxi
        :for odd-number = (1+ (ash i 1))
        :when (zerop (sbit sieve i))
          :collect odd-number :into values
        :when (<= i stop)
          :do (loop :for j :from (* i (1+ i) 2) :to maxi :by odd-number
                    :do (setf (sbit sieve j) 1))
        :finally (return (cons 2 values))))

(function-cache:defcached (cached-sieve-odds :timeout 120) (maximum)
  (sieve-odds maximum))

