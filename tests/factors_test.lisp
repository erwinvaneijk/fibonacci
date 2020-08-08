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

(deftest test-divisors
  (testing "The divisors function."
    (ok (equal '(0) (divisors 0)))
    (ok (equal '(1) (divisors 1)))
    (ok (equal '(1 2) (divisors 2)))
    (ok (equal '(1 2) (divisors -2)))
    (ok (equal '(1 2 2) (divisors 4)))
    (ok (equal '(1 2 47) (divisors (* 2 47))))
    (ok (equal '(1 2 7) (divisors (* 2 7))))
    (ok (equal '(1 3 3 3) (divisors 27)))
    (ok (equal '(1 2 2 2 2 2 2) (divisors 64)))
    (ok (equal '(1 389) (divisors 389)))
    (ok (equal '(1 2 3 5 7 11 13 17 19 23 29 31) (divisors (* 2 3 5 7 11 13 17 19 23 29 31))))
    (ok (equal '(1 2 2 2 2 2 2 2 3 3 7 23 47 769 1103 2207 3167) (divisors 51680708854858323072)))
    (ok (equal (append '(1) (sieve-odds 1000)) (divisors (reduce #'* (sieve-odds 1000)))))
    (when (string/= (lisp-implementation-type) "armedbear")
        (ok (equal '(1 193 389 3084989 361040209) (divisors 83621143489848422977)))
        (function-cache:clear-cache-all-function-caches)
        (ok (equal '(1 193 389 3084989 361040209) (divisors 83621143489848422977))))))

(deftest test-factors
  (testing "The factors function."
    (ok (equal '(0) (factors 0)))
    (ok (equal '(1) (factors 1)))
    (ok (equal '(1 2) (factors 2)))
    (ok (equal '(1 2) (factors 4)))
    (ok (equal '(1 2 47) (factors (* 2 47))))
    (ok (equal '(1 2 7) (factors (* 2 7))))
    (ok (equal '(1 3) (factors 27)))
    (ok (equal '(1 2) (factors 64)))
    (ok (equal '(1 389) (factors 389)))
    (ok (equal '(1 2 3 5 7 11 13 17 19 23 29 31) (factors (* 2 3 5 7 11 13 17 19 23 29 31))))
    (ok (equal '(1 2 3 7 23 47 769 1103 2207 3167) (factors 51680708854858323072)))
    (ok (equal '(1 193 389 3084989 361040209) (factors 83621143489848422977))))

  (testing "The trivial factors function."
    (ok (equal '(0) (trivial-factors 0)))
    (ok (equal '(1) (trivial-factors 1)))
    (ok (equal '(1 2) (trivial-factors 2)))
    (ok (equal '(1 2 4) (trivial-factors 4)))
    (ok (equal '(1 2 47 94) (trivial-factors (* 2 47))))
    (ok (equal '(1 2 7 14) (trivial-factors (* 2 7))))
    (ok (equal '(1 3 9 27) (trivial-factors 27)))
    (ok (equal '(1 2 4 8 16 32 64) (trivial-factors 64)))
    (ok (equal '(1 389) (trivial-factors 389)))
    (ok (equal '(1 2 3 5 6 7 10 11 13 14 15 17 21 22 26 30 33 34 35 39 42 51 55 65 66
                 70 77 78 85 91 102 105 110 119 130 143 154 165 170 182 187 195 210
                 221 231 238 255 273 286 330 357 374 385 390 429 442 455 462 510 546
                 561 595 663 714 715 770 858 910 935 1001 1105 1122 1155 1190 1309
                 1326 1365 1430 1547 1785 1870 2002 2145 2210 2310 2431 2618 2730
                 2805 3003 3094 3315 3570 3927 4290 4641 4862 5005 5610 6006 6545
                 6630 7293 7735 7854 9282 10010 12155 13090 14586 15015 15470 17017
                 19635 23205 24310 30030 34034 36465 39270 46410 51051 72930 85085
                 102102 170170 255255 510510) (trivial-factors (* 2 3 5 7 11 13 17))))))
