;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(defun make-bag (lst)
  (let ((hs (make-hash-table)))
    (labels ((%make-bag (lst)
               (if lst
                   (multiple-value-bind (val exists)
                       (gethash (car lst) hs)
                     (if exists
                         (setf (gethash (car lst) hs) (1+ val))
                         (setf (gethash (car lst) hs) 1))
                     (%make-bag (cdr lst)))
                   hs)))
      (%make-bag lst))))




(gethash '0 (make-bag harm-struct))
