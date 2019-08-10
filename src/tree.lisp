(defpackage cl-blog.tree
  (:use :common-lisp
        :cl-blog.util)
  (:export :tree-walk
           :tree-keep
           :tree-add
           :tree-remove-tag))

(in-package cl-blog.tree)

(defun tree-walk (fn tree)
  (cond ((atom tree) (funcall fn tree))
	(t (cons (tree-walk fn (first tree))
	         (tree-walk fn (rest tree))))))

(dotests
 (test= (tree-walk #'identity '(a b c (e f (g h)) d))
        '(A B C (E F (G H)) D))
 (test= (tree-walk #'(lambda (x)
                       (if (numberp x)
                           (* x x)
                           x))
                   '(1 2 3 (4 5 (6 7) 8)))
        '(1 4 9 (16 25 (36 49) 64))))

(defun tree-keep (fn tree)
  (cond ((atom tree) tree)
        ((funcall fn (first tree))
         (cons (tree-keep fn (first tree))
	       (tree-keep fn (rest tree))))
	(t (tree-keep fn (rest tree)))))

(dotests
 (test= (tree-keep #'identity '())
        ())
 (test= (tree-keep #'identity '(a b c))
        '(a b c))
 (test= (tree-keep #'identity '(a b (c d) e))
        '(a b (c d) e))
 (test= (tree-keep #'(lambda (x) (not (equal x 'b)))
                   '(a b (c d) e))
        '(a (c d) e))
 (test= (tree-keep #'(lambda (x) (or (not (numberp x))
                                     (evenp x)))
                   '(1 2 (3 4) 5 6))
        '(2 (4) 6)))

(defun tree-remove-tag (tag-kw tree)
  (tree-keep #'(lambda (x)
                 (or (not (listp x))
                     (not (listp (car x)))
                     (not (equal (caar x) tag-kw))))
             tree))

(dotests
 (let ((scr '((:SCRIPT :TYPE "text/javascript") " ... javascript ... ")))
   (test= '(a b c)          (tree-remove-tag :script `(a ,scr b c)))
   (test= '(a (:d :e) b c)  (tree-remove-tag :script '(a (:d :e) b c)))
   (test= '(a b c)          (tree-remove-tag :script `(a ,scr b c ,scr ,scr)))
   (test= '(a b c)          (tree-remove-tag :script '(a b c)))))

(defun tree-add (fn el tree &key (order :after) (once nil))
  (labels ((fff (done tree order once)
             (cond ((atom tree) tree)
                   ((and (or (not once)
                             (not done))
                         (funcall fn (first tree)))
                    (if (equal order :after)
                        (list* (fff t (first tree) order once)
                               el
	                       (fff t (rest tree) order once))
                        (list* el
                               (fff t (first tree) order once)
	                       (fff t (rest tree) order once))))
	           (t (cons (fff done (first tree) order once)
	                    (fff done (rest tree) order once))))))
    (fff nil tree order once)))

(dotests
 (let ((invec '(1 1 3 5 9 8 9 10 (1 16 1)))
       (pred #'(lambda (x) (and (numberp x)
                                (evenp x)))))
   (test= (tree-add pred 666 invec
                    :once nil
                    :order :before)
          '(1 1 3 5 9 666 8 9 666 10 (1 666 16 1)))

   (test= (tree-add pred 666 invec
                    :once t
                    :order :before)
          '(1 1 3 5 9 666 8 9 10 (1 16 1)))

   (test= (tree-add pred 666 invec
                    :once nil
                    :order :after)
          '(1 1 3 5 9 8 666 9 10 666 (1 16 666 1)))

   (test= (tree-add pred 666 invec
                    :once t
                    :order :after)
          '(1 1 3 5 9 8 666 9 10 (1 16 1)))))

(defun locate-element (fn tree)
  (let ((ft (funcall fn tree)))
    (cond
      ((not tree) nil)
      (ft tree)
      ((atom tree) nil)
      (t (or ft
             (locate-element fn (car tree))
             (locate-element fn (cdr tree)))))))

(dotests
 (let ((f #'(lambda (x)
              (and (atom x) (equalp x 'z)))))
   (test= nil (locate-element f '(1 2 3)))
   (test= nil (locate-element f nil))
   (test= 'z (locate-element f '(1 2 z))))
 (let ((f #'(lambda (x)
              (and (listp x)
                   (equalp (car x) :horse)))))
   (test= nil (locate-element f ()))
   (test= nil (locate-element f '(1 2 3)))
   (test= '(:horse) (locate-element f '((:horse))))
   (test= '(:horse) (locate-element f '(1 2 (:horse) 3)))
   (test= '(:horse) (locate-element f '(1 2 ((:horse)) 3)))
   (test= '(:horse) (locate-element f '(((:horse)) 1 2 3)))))
