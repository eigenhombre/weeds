(defpackage weeds.tree
  (:use :common-lisp
        :weeds.util)
  (:export :tree-add
           :tree-find
           :tree-keep
           :tree-remove
           :tree-remove-tag
           :tree-walk))

(in-package weeds.tree)

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
        '(2 (4) 6))
 (test= (tree-keep #'symbolp
                   '(1 a (3 4) 5 6))
        '(a))
 ;; FIXME: this doesn't work, `tree-keep` is broken!
 #+x(test= (tree-keep #'(lambda (x)
                          (and (consp x)
                               (equal 'a (car x))))
                      '(1 (a) (3 4) 5 6))
           '((a))))

(defun tree-remove (fn tree)
  (tree-keep (complement fn) tree))

(dotests
 (test= (tree-remove #'(lambda (x) (equal x 'b))
                     '(a b (c d) e))
        '(a (c d) e))
 (test= (tree-remove #'(lambda (x) (and (numberp x)
                                        (evenp x)))
                     '(1 2 (3 4) 5 6))
        '(1 (3) 5)))

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

(defun tree-find (fn tree)
  (let ((ft (funcall fn tree)))
    (cond
      ((not tree) nil)
      (ft tree)
      ((atom tree) nil)
      (t (or ft
             (tree-find fn (car tree))
             (tree-find fn (cdr tree)))))))

(dotests
 (test= 1 (tree-find #'(lambda (x) (equal x 1)) '(1 2 3)))
 (test= 2 (tree-find #'(lambda (x) (equal x 2)) '(1 2 3)))
 (let ((f #'(lambda (x)
              (and (atom x) (equalp x 'z)))))
   (test= nil (tree-find f '(1 2 3)))
   (test= nil (tree-find f nil))
   (test= 'z (tree-find f '(1 2 z))))
 (let ((f #'(lambda (x)
              (and (listp x)
                   (equalp (car x) :horse)))))
   (test= nil (tree-find f ()))
   (test= nil (tree-find f '(1 2 3)))
   (test= '(:horse) (tree-find f '((:horse))))
   (test= '(:horse) (tree-find f '(1 2 (:horse) 3)))
   (test= '(:horse) (tree-find f '(1 2 ((:horse)) 3)))
   (test= '(:horse) (tree-find f '(((:horse)) 1 2 3)))))
