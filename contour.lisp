;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Contour Mapping Algorithm for PDLISP
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Header --- contour
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Contour Function
;;;   Because I do not use associated lists, the function only works if there
;;;   no repeated notes in each set. 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun contour (both-sets)
  (let*((result)
	(split-point (/ (length both-sets) 2))
	(original)
	(new_set)
	(key-idx)
	(original-sort)
	(new-set-sort)
	(map-key))

    ;; unpacks the first half of the entered argument
    (setq original (loop :for i :below split-point :collect
	 (nth i both-sets)))

    ;; unpacks the second half of the entered argument
    (setq new_set (loop :for i from split-point :below (length both-sets) :collect
	 (nth i both-sets)))
    
    ;; I copy the tree from original [arg] to original-sort
    (setq original-sort (copy-tree original))    
    ;; I sort the copy of original in oiriginal sort
    (setf original-sort (stable-sort original-sort #'<))
    ;; I copy the tree from new_set [arg] to new_set-sort
    (setq new_set-sort (copy-tree new_set))
    ;; I sort the copy of new_set in new_set-sort
    (setf new_set-sort (sort new_set-sort #'<))
    ;; create the key for indexes
    (setf key-idx (loop :for note in original-sort :collect
		     (position note original)))
    ;; I collect the positions of the indexes in the key 
    (setf map-key (loop :for i :below split-point :collect 
		       (position i key-idx)))
    ;; I use map key to collect from the new_set-sort in the corresponding
    ;; order
    (setf result (loop :for i in map-key :collect 
		      (elt new_set-sort i)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass contour (pdlisp)
  ((inlets :initform 1 :reader inlets)
   (outlets :initform 1 :reader outlets)
   (receiver :initform nil :accessor receiver)))

(defmethod initialize ((contour contour) &rest rest)
  (setf (receiver contour) (receiver-new contour "recv"))
  (with-pd-stream nil
    (format t "It is all in the curves")))

(defmethod finalize ((contour contour))
  (when (receiver contour)
    (receiver-free (receiver contour))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod list-1 ((contour contour) list)
  (outlet 1 (contour list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

