(use-package :com.wutka.binheap)

(defun randlist (n max)
  (loop for i from 0 below n collect (random max)))

(defun binheap-to-list (binheap)
  (loop while (not (binheap-empty binheap)) collect (binheap-pop binheap)))

(defun binheap-from-list (l)
  (let ((binheap (make-binheap)))
    (dolist (x l) (binheap-push binheap x))
    binheap))

(defstruct heapable-item
  (name "")
  (key -1))

(defun compare-heapables (a b)
  (string< (heapable-item-name a) (heapable-item-name b)))

(defun update-heapable (a n)
  (setf (heapable-item-key a) n))
