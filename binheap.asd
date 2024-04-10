(defpackage :com.wutka.binheap-system (:use :asdf :cl))
(in-package :com.wutka.binheap-system)

(defsystem binheap
  :name "binheap"
  :author "Mark Wutka <mark@wutka.com"
  :version "1.0"
  :license "BSD"
  :description "Binary heap"
  :long-description ""
  :components
  ((:file "packages")
   (:file "binheap" :depends-on ("packages")))
  :depends-on ())
