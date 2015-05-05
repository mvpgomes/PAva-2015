(defclass tensor ()
    ((data :type array
           :reader tensor-content
           :initarg :initial-content)))

(defun map-tensor (f tensor)
    (let* ((content (tensor-content tensor))
          (new-array (make-array (array-dimensions content))))
        (array-map f content)))

(defun map-array (function &rest arrays)
  "maps the function over the arrays.
   Assumes that all arrays are of the same dimensions.
   Returns a new result array of the same dimension."
  (flet ((make-displaced-array (array)
           (make-array (reduce #'* (array-dimensions array))
                       :displaced-to array)))
    (let* ((displaced-arrays (mapcar #'make-displaced-array arrays))
           (result-array (make-array (array-dimensions (first arrays))))
           (displaced-result-array (make-displaced-array result-array)))
      (apply #'map-into displaced-result-array function displaced-arrays)
      result-array)))

(defmethod print-object ((tensor tensor) (stream stream))
    "TODO"
    (let ((cur-dim (length subscripts)))
        (if (eql cur-dim (array-rank array))
            (setf (apply #'aref new-array subscripts)(funcall f (apply #'aref array subscripts)))
            (dotimes (i (nth cur-dim (array-dimensions array)))
                (map-array f array new-array (append subscripts (list i)))))))
