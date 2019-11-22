;;;
;;; factors.lisp
;;;
;;; An implementation of the algorithm to factorize a number n into
;;; its prime components.
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

(defun is-divisor-p (n k)
  (= 0 (mod n k)))

(defconstant NUM-PRECOMPUTED-PRIMES 1000000
  "The number of primes we compute in the sieve.")

(defun factors (n)
  "Factorize the number N into its prime factors."
  (cond ((< n 0) (alexandria:flatten (list -1 (factors (- n)))))
        ((= n 0) (cons 0 nil))
        ((= n 1) (cons 1 nil))
        (t
         (alexandria:flatten
          (list
           ;; First get rid of all the even factors
           (loop
             :while (evenp n)
             :do (setf n (/ n 2))
             :collect 2)
           ;; Then collect all the odd factors.
           ;; First use a number of primes which are pre-collected.
           (loop
             :while (> n 1)
             :for k in (cached-sieve-odds (min NUM-PRECOMPUTED-PRIMES n))
             :collect (loop
                        :while (is-divisor-p n k)
                        :collect k
                        :do (setf n (/ n k))))
           ;; Now go look for the last ones by just trying. This might
           ;; take some time
           (loop
             :while (> n 1)
             :for k = (min (1+ NUM-PRECOMPUTED-PRIMES) n) then (+ 2 k)
             ;:for k = 3 then (if (is-divisor-p n k) k (+ 2 k))
             :for is-divisor = (is-divisor-p n k)
             ; :do (format t "O: ~a ~a ~%" n k)
             :when is-divisor :collect k
             :when is-divisor :do (setf n (/ n k))))))))
