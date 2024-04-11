(in-package :com.wutka.binheap)

(defstruct bh
  (tree (make-array 50 :fill-pointer 0 :adjustable t)) (comp #'<) (key-update nil))

(defmacro with-binheap (binheap &body body)
  `(with-accessors ((tree bh-tree)
		   (comp bh-comp)
		   (key-update bh-key-update))
      ,binheap
    ,@body))

(defun make-binheap (&key (comp #'<) (initial-size 50) (initial-contents nil) (key-update nil))
  (if (null initial-contents)
      (make-bh :tree (make-array initial-size :fill-pointer 0 :adjustable t) :comp comp :key-update key-update)
      (let* ((initial-len (length initial-contents))
	     (tree-array (make-array initial-len :fill-pointer initial-len
						 :initial-contents initial-contents :adjustable t)))
	(sort tree-array comp)
	(make-bh :tree tree-array :comp comp :key-update key-update))))

(defun binheap-empty (binheap)
  (= (length (bh-tree binheap)) 0))

(defun parent-loc (loc)
  (floor (- loc 1) 2))

(defun left-loc (loc)
  (- (right-loc loc) 1))

(defun right-loc (loc)
  (* (+ loc 1) 2))

(defun binheap-pop (binheap)
  (if (eq (length (bh-tree binheap)) 0) nil
    (with-binheap binheap
		  (let ((result (aref tree 0))
			(new-top (vector-pop tree)))
;;; Take the last element from the vector and put it at the beginning
		    (setf (aref tree 0) new-top)
;;; Rebalance the tree from top down
		    (binheap-rebalance-down binheap 0)
		    result))))

(defun binheap-push (binheap x)
  (with-binheap binheap
    ;;; Add the new item to the end of the vector
    (vector-push-extend x tree)
    (when key-update
      (funcall key-update (aref tree (- (length tree) 1)) (- (length tree) 1)))
;;; Rebalance the tree from the bottom up
    (binheap-rebalance-up binheap (- (length tree) 1))))

(defun binheap-swap (binheap a b)
  (let ((a-val (aref (bh-tree binheap) a)))
    (setf (aref (bh-tree binheap) a) (aref (bh-tree binheap) b))
    (setf (aref (bh-tree binheap) b) a-val)
    (if (bh-key-update binheap)
	(let ((update (bh-key-update binheap)))
	  (funcall update (aref (bh-tree binheap) a) a)
	  (funcall update (aref (bh-tree binheap) b) b)))

    (defun binheap-rebalance-down (binheap parent)
      (with-binheap
	  binheap
	(let* ((right (right-loc parent))
	       (left (left-loc parent)))
	  (cond
	    ((< right (length tree))
	     (let ((left-val (aref tree left))
		   (right-val (aref tree right))
		   (parent-val (aref tree parent)))
	   ;;; If the left is less than the parent, we need to swap
	       (if (funcall comp left-val parent-val)
	       ;;; If the right is less than the left, swap it with the parent
		   (if (funcall comp right-val left-val)
		       (progn
			 (binheap-swap binheap right parent)
			 (binheap-rebalance-down binheap right))
		   ;;; Otherwise swap the left with the parent
		       (progn
			 (binheap-swap binheap left parent)
			 (binheap-rebalance-down binheap left)))
	       ;;; If the left wasn't less than the parent, see if the right one is
		   (when (funcall comp right-val parent-val)
		     (binheap-swap binheap right parent)
		     (binheap-rebalance-down binheap right)))))
	    ((< left (length tree))
	 ;;; If only the left exists, if it is less than the parent, swap and rebalance
	     (let ((left-val (aref tree left))
		   (parent-val (aref tree parent)))
	       (when (funcall comp left-val parent-val)
		 (binheap-swap binheap left parent))))))))))

(defun binheap-rebalance-up (binheap from)
  (when (> from 0)
    (with-binheap binheap
      (let* ((parent (parent-loc from))
	     (from-val (aref tree from))
	     (parent-val (aref tree parent)))
	;;; If this value is smaller than its parent, swap them, and rebalance
	;;; upward from the parent position
	(when (funcall comp from-val parent-val)
	  (binheap-swap binheap parent from)
	  (binheap-rebalance-up binheap parent))))))

(defun binheap-rebalance-at (binheap pos)
  (if (= pos 0) (binheap-rebalance-down binheap pos)
    (with-binheap binheap
		  (let* ((parent (parent-loc pos))
			 (parent-val (aref tree parent))
			 (pos-val (aref tree pos)))
		    (if (funcall comp pos-val parent-val)
			(binheap-rebalance-up binheap pos)
		      (binheap-rebalance-down binheap pos))))))



