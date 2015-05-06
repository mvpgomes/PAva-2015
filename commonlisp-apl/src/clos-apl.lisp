;;; Utility functions
(defun compose (fn1 fn2)
    "Composes two functions. It only works for the case where the arguments
     passed to the resulting function belong to the second function."
    (lambda (&rest args)
        (funcall fn1 (apply fn2 args))))

(defun bool-to-int (bool)
    "Returns a number depending on the argument.
     If the argument is true, it returns 1.
     If the argument is false, it returns 0."
    (if bool
        1
        0))

;;; Project implementation
(defclass tensor ()
    ((data :type array
           :reader tensor-content
           :initarg :initial-content)))

(defclass scalar (tensor) ())

(defun scalar? (tensor)
   (zerop (array-rank (tensor-content tensor))))

(defun map-tensor (f left-tensor &rest right-tensor)
    (let* ((content (tensor-content left-tensor))
          (additional-content (tensor-content (first right-tensor)))
          (new-array (make-array (array-dimensions content))))
        (map-array f content additional-content)))

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
                (let* ((cur-dim (length subscripts))
                      (last-iter (eql (- (nth cur-dim subscripts) 1) (nth cur-dim (array-dimensions array)))))
                    (if (eql cur-dim (array-rank array))
                        (format stream "~d " (apply #'aref array subscripts))
                        (progn
                            (dotimes (i (nth cur-dim (array-dimensions array)))
                                (rec array (append subscripts (list i))))
                            (unless last-iter (format stream "~%")))))))
        (rec (tensor-content tensor) '())))

(defun test-print-object ()
    (make-instance 'tensor
        :initial-content (make-array '(2 2 2) :initial-contents '(((1 2) (3 4)) ((5 6) (7 8))))))


" --------------------------- Tensor Constructors ---------------------------- "

" - s : element -> tensor : receives a parameter and returns a scalar."
(defun s (element) (make-instance 'scalar :initial-content (make-array nil :initial-contents element)))

" - v : element -> tensor : receives a parameter list and returns a vector."
(defun v (&rest elements) (make-instance 'tensor :initial-content (make-array (length elements) :initial-contents elements)))

" ---------------------------- Generic Functions ----------------------------- "
(defgeneric .- (tensor &optional tensor2))

(defgeneric symmetric (tensor))

(defgeneric subtract (tensor1 tensor2))

(defgeneric ./ (tensor &optional tensor2))

(defgeneric .! (tensor))

(defgeneric .sin (tensor))

(defgeneric .cos (tensor))

(defgeneric .not (tensor))

(defgeneric shape (tensor))

(defgeneric .+ (tensor2 tensor))

(defgeneric .% (tensor2 tensor))

(defgeneric .> (tensor2 tensor))

(defgeneric .>= (tensor2 tensor))

(defgeneric .or (tensor2 tensor))

(defgeneric .* (tensor2 tensor))

(defgeneric .// (tensor2 tensor))

(defgeneric .< (tensor2 tensor))

(defgeneric .<= (tensor2 tensor))

(defgeneric .= (tensor2 tensor))

(defgeneric .and (tensor2 tensor))

" ---------------------------- Monadic Functions ----------------------------- "

(defmethod .- (tensor &optional tensor2)
    "Creates a new tensor whose elements are the symmetic of the corresponding
     elements of the argument tensor."
     (if tensor2
         (subtract tensor tensor2)
         (symmetric tensor)))

(defmethod symmetric ((tensor tensor))
    (map-tensor #'- tensor))

(defmethod substract ((tensor1 tensor) (tensor2 tensor))
    (map-tensor #'- tensor1 tensor2))

(defmethod ./ ((tensor tensor) &optional (tensor2 tensor))
    "Creates a new tensor whose elements are the inverse of the corresponding
     elements of the argument tensor."
    (map-tensor #'/ tensor tensor2))

(defmethod .! ((tensor tensor))
    " - .! : tensor -> tensor : receives a tensor and returns a new tensor
     where the function factorial is applied element-wise."
    (map-tensor #'! tensor))

(defmethod .sin ((tensor tensor))
    " - .sin : tensor -> tensor : receives a tensor and returns a new tensor
     where the function sin is applied element-wise. "
    (map-tensor #'sin tensor))

(defmethod .cos ((tensor tensor))
    "Creates a new tensor whose elements are the result of applying the cos
     function to the corresponding elements of the argument tensor."
    (map-tensor #'cos tensor))

(defmethod .not ((tensor tensor))
    " - .not : tensor -> tensor : receives a tensor and returns a new tensor
     where the function not is applied element-wise."
    (map-tensor #'(lambda (x) (if (> x 0) 0 1)) tensor))

(defmethod shape ((tensor tensor))
    " - shape : tensor -> tensor : receives a tensor and return a new tensor
     that contains the length of each dimension of the tensor."
    (v (array-dimensions (tensor-content tensor))))

(defun interval (n)
    "Creates a vector containing an enumeration of all integers starting
     from 1 up to the argument."
    (labels ((rec (i n)
                (if (> i n)
                    '()
                    (cons i (rec (1+ i) n)))))
        (v (rec 1 n))))

" ---------------------------- Dyadic Functions ----------------------------- "

(defmethod .+ ((tensor tensor) (tensor2 tensor))
    "Creates a tensor with the sum of the corresponding elements of the argument
     tensors."
    (map-tensor #'+ tensor tensor2))

(defmethod .% ((tensor tensor) (tensor2 tensor))
    " - .% : tensor, tensor -> tensor : receives two tensors and return a new tensor
      that contains the remainder between the elements of the tensors."
    (map-tensor #'% tensor tensor2))

(defmethod .% ((scalar scalar) (tensor tensor))
    " - .% : scalar, tensor -> tensor : receives two tensors and return a new tensor that
      contains the remainder between the scalar and the elements of the tensor."
    (map-tensor #'% scalar tensor))

(defmethod .% ((tensor tensor) (scalar scalar))
  " - .% : tensor, scalar -> tensor : receives two tensors and return a new tensor that contains the
    remainder between the elements of the tensor and the scalar."
    (map-tensor #'% tensor scalar))

(defmethod .> ((tensor tensor) (tensor2 tensor))
  " - .> : tensor, tensor -> tensor : receives two tensors and return a new tensor that contains the
    result of the comparsion (greater then) between the elements of the tensors."
    (map-tensor #'> tensor tensor2))

(defmethod .> ((scalar scalar) (tensor tensor))
  " - .> : tensor, tensor -> tensor : receives two tensors and return a new tensor that contains the
    result of the comparsion (greater then) between the scalar and the elements of the tensor."
    (map-tensor #'> scalar tensor))

(defmethod .> ((tensor tensor) (scalar scalar))
  " - .> : tensor, tensor -> tensor : receives two tensors and return a new tensor that contains the
    result of the comparsion (greater then) between the elements of the tensor and the scalar."
    (map-tensor #'> tensor scalar))

(defmethod .>= ((tensor tensor) (tensor2 tensor))
  " - .>= : tensor, tensor -> tensor : receives two tensors and return a new tensor that contains the
    result of the comparsion (greater equals then) between the elements of the tensors."
    (map-tensor #'>= tensor tensor2))

(defmethod .>= ((scalar scalar) (tensor tensor))
  " - .>= : tensor, tensor -> tensor : receives two tensors and return a new tensor that contains the
    result of the comparsion (greater equals then) between the scalar and the elements of the tensor."
    (map-tensor #'>= scalar tensor))

(defmethod .>= ((tensor tensor) (scalar scalar))
  " - .>= : tensor, tensor -> tensor : receives two tensors and return a new tensor that contains the
    result of the comparsion (greater equals then) between the elements of the tensor and the scalar.."
    (map-tensor #'>= tensor scalar))

(defmethod .or ((tensor tensor) (tensor2 tensor))
  " - .or : tensor, tensor -> tensor : receives two tensors and return a new tensor that contains the
    result of the logical comparsion (or) between the elements of the tensors."
    (map-tensor #'or tensor tensor2))

(defmethod .* ((tensor tensor) (tensor2 tensor))
    "Creates a tensor with the multiplication of the corresponding elements of
     the argument tensors."
    (map-tensor #'* tensor tensor2))

(defmethod .// ((tensor tensor) (tensor2 tensor))
    "Creates a tensor with the integer division of the corresponding elements
     of the argument tensors."
    (map-tensor (lambda (e1 e2) (truncate (/ e1 e2))) tensor tensor2))

(defmethod .// ((scalar scalar) (tensor tensor))
    "Creates a tensor with the integer division of the corresponding elements
     of the scalar and the argument tensor."
    (map-tensor (lambda (e1 e2) (truncate (/ e1 e2))) scalar tensor))

(defmethod .// ((tensor tensor) (scalar scalar))
    "Creates a tensor with the integer division of the corresponding elements
    of the argument tensor and the scalar."
    (map-tensor (lambda (e1 e2) (truncate (/ e1 e2))) tensor scalar))

(defmethod .< ((tensor tensor) (tensor2 tensor))
    "Creates a tensor using the relation \"less than\" on the corresponding
     elements of the argument tensors. The result tensor will have, as elements,
     the integers 0 or 1."
    (map-tensor (compose #'bool-to-int #'<) tensor tensor2))

(defmethod .< ((scalar scalar) (tensor tensor))
    "Creates a tensor using the relation \"less than\" on the corresponding
    elements scalar and the argument tensor. The result tensor will have, as elements,
    the integers 0 or 1."
    (map-tensor (compose #'bool-to-int #'<) scalar tensor))

(defmethod .< ((tensor tensor) (scalar scalar))
    "Creates a tensor using the relation \"less than\" on the corresponding
    elements of the argument tensor and the scalar. The result tensor will have, as
    elements, the integers 0 or 1."
    (map-tensor (compose #'bool-to-int #'<) tensor scalar))

(defmethod .<= ((tensor tensor) (tensor2 tensor))
    "Creates a tensor using the relation \"less or equal than\" on the corresponding
     elements of the argument tensors. The result tensor will have, as elements,
     the integers 0 or 1."
    (map-tensor (compose #'bool-to-int #'<=) tensor tensor2))

(defmethod .<= ((scalar scalar) (tensor tensor))
    "Creates a tensor using the relation \"less or equal than\" on the corresponding
     elements scalar and the argument tensors. The result tensor will have, as elements,
     the integers 0 or 1."
    (map-tensor (compose #'bool-to-int #'<=) scalar tensor))

(defmethod .<= ((tensor tensor) (scalar scalar))
     "Creates a tensor using the relation \"less or equal than\" on the corresponding
      elements of the argument tensors. The result tensor will have, as elements,
      the integers 0 or 1."
      (map-tensor (compose #'bool-to-int #'<=) tensor scalar))

(defmethod .= ((tensor tensor) (tensor2 tensor))
    "Creates a tensor using the relation \"less or equal than\" on the corresponding
     elements of the argument tensors. The result tensor will have, as elements,
     the integers 0 or 1."
    (map-tensor (compose #'bool-to-int #'=) tensor tensor2))

(defmethod .and ((tensor tensor) (tensor2 tensor))
    "Creates a tensor using the relation \"less or equal than\" on the corresponding
     elements of the argument tensors. The result tensor will have, as elements,
     the integers 0 or 1."
    (map-tensor (compose #'bool-to-int (lambda (e1 e2) (and e1 e2))) tensor tensor2))
