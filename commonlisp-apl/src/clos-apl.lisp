(defclass tensor ()
    ((data :type array
           :reader tensor-content
           :initarg :initial-content)))

(defun scalar? (tensor)
    (and (eql (array-rank (tensor-content tensor)) 1)
         (eql (car (array-dimensions (tensor-content tensor))) 1)))

(defun map-tensor (f tensor)
    (let* ((content (tensor-content tensor))
           (new-array (make-array (array-dimensions content))))
        (array-map f content)))

(defun map-array (function &rest arrays)
    "Maps the function over the arrays.
    Assumes that all arrays are of the same dimensions.
    Returns a new result array of the same dimension."
    (flet ((make-displaced-array (array)
           (make-array (reduce #'* (array-dimensions array)) :displaced-to array)))
        (let* ((displaced-arrays (mapcar #'make-displaced-array arrays))
               (result-array (make-array (array-dimensions (first arrays))))
               (displaced-result-array (make-displaced-array result-array)))
            (apply #'map-into displaced-result-array function displaced-arrays)
            result-array)))

(defmethod print-object ((tensor tensor) (stream stream))
    "Implementation of the generic method print-object for the tensor data structure.
     If the tensor is a scalar, print a single-element.
     If the tensor is a vector, prints its elements separated by a whitespace.
     If the tensor is not one of the previous cases, then for each sub-tensor of the
     first dimension, prints the sub-tensor separated from the next sub-tensor by a
     number of empty lines that is equal to the number of dimensions minus one."
    (labels ((rec (array subscripts)
        (let ((cur-dim (length subscripts)))
            (if (eql cur-dim (array-rank array))
                (format stream "~a " (apply #'aref array subscripts))
                (dotimes (i (nth cur-dim (array-dimensions array)))
                    (rec array (append subscripts (list i)))
                    (dotimes (num (- (array-rank array) cur-dim 1))
                        (format stream "~%")))))))
        (rec (tensor-content tensor) '())))

(defun test-print-object ()
    (make-instance 'tensor :initial-content (make-array '(2 2 2) :initial-contents '(((1 2) (3 4)) ((5 6) (7 8))))))
