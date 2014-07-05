#|
  This file is a part of cl-voxelize project.
  Copyright (c) 2014 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-voxelize-test
  (:use :cl
        :cl-voxelize
        :cl-test-more)
  (:import-from :cl-voxelize
                :triangle-intersect-p
                :quadtree
                :point-intersect-p
                :query-quadtree))
(in-package :cl-voxelize-test)

(plan nil)


;;;
;;; test TRIANGLE-INTERSECT-P function
;;;

(diag "TRIANGLE-INTERSECT-P")

(is (triangle-intersect-p '(0.0 0.0 2.0 2.0) '((1.0 1.0 0.0)
                                               (3.0 1.0 0.0)
                                               (2.0 3.0 0.0)))
    t "basic case 1")

(is (triangle-intersect-p '(0.0 0.0 2.0 2.0) '((-1.0 1.0 0.0)
                                               (1.0 1.0 0.0)
                                               (0.0 3.0 0.0)))
    t "basic case 2")

(is (triangle-intersect-p '(0.0 0.0 2.0 2.0) '((-1.0 1.0 0.0)
                                               (3.0 1.0 0.0)
                                               (1.0 3.0 0.0)))
    t "basic case 3")

(is (triangle-intersect-p '(0.0 0.0 2.0 2.0) '((1.0 3.0 0.0)
                                               (3.0 3.0 0.0)
                                               (2.0 5.0 0.0)))
    nil "basic case 4")


;;;
;;; test POINT-INTERSECT-P function
;;;

(diag "POINT-INTERSECT-P")

(is (point-intersect-p '(0.0 0.0 2.0 2.0) 1.0 1.0)
    t "basic case 1")

(is (point-intersect-p '(0.0 0.0 2.0 2.0) -1.0 -1.0)
    nil "basic case 2")


;;;
;;; test QUADTREE and QUERY-QUADTREE functions
;;;

(diag "QUADTREE and QUERY-QUADTREE")

(let ((qt (quadtree '(((0.0 0.0 0.0) (1.0 0.0 0.0) (0.0 1.0 0.0))
                      ((0.0 1.0 0.0) (1.0 1.0 1.0) (1.0 2.0 0.0))))))
  (is (query-quadtree qt 1.0 1.0)
      '(((0.0 1.0 0.0) (1.0 1.0 1.0) (1.0 2.0 0.0))
        ((0.0 0.0 0.0) (1.0 0.0 0.0) (0.0 1.0 0.0)))
      "basic case 1"))

(let ((qt (quadtree '(((0.0 0.0 0.0) (1.0 0.0 0.0) (0.0 0.9 0.0))
                      ((1.1 1.0 0.0) (2.0 1.0 1.0) (2.0 2.0 0.0))))))
  (cl-voxelize::devide-quadtree qt)
  (is (query-quadtree qt 0.5 0.5)
      '(((0.0 0.0 0.0) (1.0 0.0 0.0) (0.0 0.9 0.0)))
      "basic case 2"))

(let ((qt (quadtree '(((0.0 0.0 0.0) (1.0 0.0 0.0) (0.0 0.80 0.0))
                      ((0.0 0.0 0.0) (1.0 0.0 0.0) (0.0 0.81 0.0))
                      ((0.0 0.0 0.0) (1.0 0.0 0.0) (0.0 0.82 0.0))
                      ((0.0 0.0 0.0) (1.0 0.0 0.0) (0.0 0.83 0.0))
                      ((0.0 0.0 0.0) (1.0 0.0 0.0) (0.0 0.84 0.0))
                      ((0.0 0.0 0.0) (1.0 0.0 0.0) (0.0 0.85 0.0))
                      ((0.0 0.0 0.0) (1.0 0.0 0.0) (0.0 0.86 0.0))
                      ((0.0 1.0 0.0) (1.0 1.0 0.0) (1.0 2.0 0.0))))))
  (is (cl-voxelize::quadtree-leaf-p qt)
      t "basic case 3")
  (cl-voxelize::insert-quadtree qt '((0.0 0.0 0.0) (1.0 0.0 0.0)
                                     (0.0 0.87 0.0)))
  (is (cl-voxelize::quadtree-node-p qt)
      t "basic case 4")
  (is (query-quadtree qt 0.5 1.5)
      '(((0.0 1.0 0.0) (1.0 1.0 0.0) (1.0 2.0 0.0)))
      "basic case 5"))

(let ((qt (quadtree '(((0.0 0.0 0.0) (1.0 0.0 0.0) (0.0 0.80 0.0))
                      ((0.0 0.0 0.0) (1.0 0.0 0.0) (0.0 0.81 0.0))
                      ((0.0 0.0 0.0) (1.0 0.0 0.0) (0.0 0.82 0.0))
                      ((0.0 0.0 0.0) (1.0 0.0 0.0) (0.0 0.83 0.0))
                      ((0.0 0.0 0.0) (1.0 0.0 0.0) (0.0 0.84 0.0))
                      ((0.0 0.0 0.0) (1.0 0.0 0.0) (0.0 0.85 0.0))
                      ((0.0 0.0 0.0) (1.0 0.0 0.0) (0.0 0.86 0.0))
                      ((0.0 0.0 0.0) (1.0 0.0 0.0) (0.0 0.87 0.0))
                      ((0.0 0.0 0.0) (1.0 0.0 0.0) (0.0 0.88 0.0))))))
  (is (cl-voxelize::quadtree-depth (cl-voxelize::quadtree-nw
                                     (cl-voxelize::quadtree-nw qt)))
      cl-voxelize::*max-depth* "basic case 6"))


;;;
;;; test VOXELIZE function
;;;

(diag "VOXELIZE")

(let ((triangles '(((0.0 0.0 0.0) (1.0 0.0 0.0) (0.0 1.0 0.0))
                   ((1.0 0.0 0.0) (1.0 1.0 0.0) (0.0 1.0 0.0))
                   ((0.0 1.0 0.0) (1.0 1.0 0.0) (0.0 1.0 1.0))
                   ((1.0 1.0 0.0) (1.0 1.0 1.0) (0.0 1.0 1.0))
                   ((0.0 0.0 1.0) (1.0 0.0 1.0) (0.0 0.0 0.0))
                   ((1.0 0.0 1.0) (1.0 0.0 0.0) (0.0 0.0 0.0))
                   ((1.0 0.0 1.0) (0.0 0.0 1.0) (1.0 1.0 1.0))
                   ((0.0 0.0 1.0) (0.0 1.0 1.0) (1.0 1.0 1.0))
                   ((0.0 0.0 1.0) (0.0 0.0 0.0) (0.0 1.0 1.0))
                   ((0.0 0.0 0.0) (0.0 1.0 0.0) (0.0 1.0 1.0))
                   ((1.0 0.0 0.0) (1.0 0.0 1.0) (1.0 1.0 0.0))
                   ((1.0 0.0 1.0) (1.0 1.0 1.0) (1.0 1.0 0.0)))))
  (is (length (voxelize triangles 0.33))
      27 "basic case 1"))


(finalize)
