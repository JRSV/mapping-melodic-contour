;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 477 notes. 6 chords, and 12 equivalents of 4 notes each. Use Remix
;;;
;;; Main Chords--- 4-5 & 4-14:: (0 1 2 6)/(0 4 5 6) ||| (0 2 3 7)/(0 4 5 7)
;;;
;;; 4-4      (0 1 2 5) (0 3 4 5)
;;; 4-Z15    (0 1 4 6) (0 2 5 6)
;;; 4-Z29    (0 1 3 7) (0 4 6 7)
;;; 4-16     (0 1 5 7) (0 2 6 7)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter keys (flatten (fibonacci-transitions 119 '(a b c d e f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; WITH KEYS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fibo-translation (sequence)
  (loop for i in sequence
     when i
     do
       (when (equal i 'a)
	 (setf i (elt '((0 1 2 6) (0 4 5 6)) (random 2))));;4-5
       (when (equal i 'b)
	 (setf i (elt '((0 1 4 6) (0 2 5 6)) (random 2))));;4-z15
       (when (equal i 'c)
	 (setf i (elt '((0 1 2 5) (0 3 4 5)) (random 2))));;4-4
       (when (equal i 'd)
	 (setf i (elt '((0 1 3 7) (0 4 6 7)) (random 2))));;4-z29
       (when (equal i 'e)
	 (setf i (elt '((0 1 5 7) (0 2 6 7) ) (random 2))));;4-16
       (when (equal i 'f)
	 (setf i (elt '((0 2 3 7) (0 4 5 7)) (random 2))));;4-14
     collect i))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter harm-struct (flatten (fibo-translation keys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(gethash '1 (make-bag harm-struct))
(gethash '2 (make-bag harm-struct))
(gethash '3 (make-bag harm-struct))
(gethash '4 (make-bag harm-struct))
(gethash '5 (make-bag harm-struct))
(gethash '6 (make-bag harm-struct))
(gethash '7 (make-bag harm-struct))


(print harm-struct) 

(length harm-struct)
