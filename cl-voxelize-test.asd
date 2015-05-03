#|
  This file is a part of cl-voxelize project.
  Copyright (c) 2014 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-voxelize-test-asd
  (:use :cl :asdf))
(in-package :cl-voxelize-test-asd)

(defsystem cl-voxelize-test
  :author "Masayuki Takagi"
  :license "LLGPL"
  :depends-on (:cl-voxelize
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-voxelize"))))
  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove.asdf) c)
                    (asdf:clear-system c)))
