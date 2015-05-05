(defclass tensor ()
    ((data :type array
           :reader tensor-content
           :initarg :initial-content)))

(defun map-tensor (f tensor)
    (let* ((content (tensor-content tensor))
          (new-array (make-array (array-dimensions content))))
        (map-array f content)))

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


" --------------------------- Tensor Constructors ---------------------------- "

" - s : element -> tensor : receives a parameter and returns a scalar."
(defmethod s (element) (make-instance 'tensor :initial-content (make-array '(1) :initial-contents (list element))))

" - v : element -> tensor : receives a parameter list and returns a vector."
(defmethod v (&rest elements) (make-instance 'tensor :initial-content (make-array (length elements) :initial-contents elements)))

" ---------------------------- Monadic Functions ----------------------------- "

" - .! : tensor -> tensor : receives a tensor and returns a new tensor where the function factorial is applied element-wise."
(defmethod .! (tensor) (map-tensor #'! tensor))

" - .sin : tensor -> tensor : receives a tensor and returns a new tensor where the function sin is applied element-wise. "
(defmethod .sin (tensor) (map-tensor #'sin tensor))
