;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
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
(defpackage fibonacci-asd
  (:use :cl :asdf))
(in-package fibonacci-asd)

(defsystem "fibonacci"
  :name "Fibonacci series"
  :version "0.0.0"
  :author "Erwin J. van Eijk"
  :maintainer "Erwin J. van Eijk"
  :license "MIT"
  :description "Speedy Fibonacci numbers"
  ;;:depends-on(trivial-features)
  :components ((:module source
                :pathname "src/"
                :serial t
                :components ((:file "package")
                             (:file "error")
                             (:file "helpers")
                             (:file "fact"
                              :depends-on ("error"))
                             (:file "factors"
                              :depends-on ("error"))
                             (:file "fibonacci"
                              :depends-on ("error")))))
  :in-order-to ((test-op (test-op :fibonacci/tests))))

;;; vim: ft=lisp et
