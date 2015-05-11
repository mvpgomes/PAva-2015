;;; Utility functions
(defun compose (fn1 fn2)
    "Composes two functions. It only works for the case where the arguments
     passed to the resulting function belong to the second function."
    (lambda (&rest args)
        (funcall fn1 (apply fn2 args))))

(defun bool->int (bool)
    "Returns a number depending on the argument.
     If the argument is true, it returns 1.
     If the argument is false, it returns 0."
    (if bool
        1
        0))

;;; Project implementation
(defclass tensor ()
    ((data :type list
           :reader tensor-content
           :initarg :initial-content)))

(defclass scalar (tensor) ())

(defun reduce-subsets (fn vector begin end)
  (if (< (length vector) end)
      (v)
   (v (reduce fn vector :start begin :end end) (reduce-n-times fn vector begin (+ end 1)))))

(defun scalar-to-tensor (scalar tensor)
  (let* ((n (aref (tensor-content scalar)))
        (dim (array-dimensions (tensor-content tensor))))
    (make-instance 'tensor :initial-content (make-array dim :initial-element n))))

(defun map-tensor (function &rest tensors)
    (make-instance 'tensor :initial-content (apply #'map-tree function (mapcar #'tensor-content tensors))))

(defun map-tree (function &rest lists)
    "Maps the function over the lists.
     Assumes that all lists are of the same dimensions.
     Returns a new list of the same dimension."
    (let ((lst (car lists)))
        (cond ((null lst)
                  nil)
              ((atom (car lst))
                  (cons (apply function (mapcar #'car lists))
                        (apply #'map-tree function (mapcar #'cdr lists))))
              (t
                  (cons (apply #'map-tree function (mapcar #'car lists))
                        (apply #'map-tree function (mapcar #'cdr lists)))))))

(defun fold-tensor (function tensor initial-value)
    (fold-tree function (tensor-content tensor) initial-value))

(defun linearize-list (arg)
    (cond ((null arg) nil)
          ((atom (car arg)) (cons (car arg) (cdr arg)))
          (t (append (linearize-list (car arg)) (linearize-list (cdr arg))))))

(defun fold-tree (function list initial-value)
    (reduce function (linearize-list list) :initial-value initial-value))

(defun print-list (list stream)
    (labels ((rec (arg last-iteration)
                (cond ((null arg) nil)
                      ((atom (car arg))
                          (format stream
                                  (if (null (cdr arg)) "~A" "~A ")
                                  (car arg))
                          (rec (cdr arg) nil))
                      ((and (listp (car arg)) (null (cdr arg)))
                          (rec (car arg) last-iteration)
                          (unless last-iteration
                            (format stream "~%")))
                      (t
                          (rec (car arg) nil)
                          (format stream "~%")
                          (rec (cdr arg) last-iteration)))))
        (rec list t)))

(defmethod print-object ((scalar scalar) (stream stream))
    "Implementation of the generic method print-object for the scalar data structure.
     If the tensor is a scalar, print a single-element."
    (format stream "~A" (aref (tensor-content scalar))))

(defmethod print-object ((tensor tensor) (stream stream))
    "Implementation of the generic method print-object for the tensor data structure.
     If the tensor is a vector, prints its elements separated by a whitespace.
     If the tensor is not one of the previous cases, then for each sub-tensor of the
     first dimension, prints the sub-tensor separated from the next sub-tensor by a
     number of empty lines that is equal to the number of dimensions minus one."
    (labels ((rec (array subscripts from-last-iteration)
                (let* ((cur-dim (length subscripts))
                       (cur-dim-size (nth cur-dim (array-dimensions array))))
                    (if (eql cur-dim (array-rank array))
                        (format stream
                                (if (zerop (first (last subscripts))) "~A" " ~A")
                                (apply #'aref array subscripts))
                        (dotimes (i cur-dim-size)
                            (let ((last-iteration (and from-last-iteration
                                                       (eql i (- cur-dim-size 1)))))
                                (rec array (append subscripts (list i)) last-iteration)
                                (unless (or last-iteration (eql cur-dim (- (array-rank array) 1)))
                                    (format stream "~%"))))))))
        (rec (tensor-content tensor) '() t)))

" --------------------------- Tensor Constructors ---------------------------- "

" - s : element -> tensor : receives a parameter and returns a scalar."
(defun s (element) (make-instance 'scalar :initial-content element))

" - v : element -> tensor : receives a parameter list and returns a vector."
(defun v (&rest elements) (make-instance 'tensor :initial-content elements))

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

(defmethod subtract ((tensor1 tensor) (tensor2 tensor))
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
                (unless (> i n)
                    (cons i (rec (1+ i) n)))))
        (apply #'v (rec 1 n))))

" ---------------------------- Dyadic Functions ----------------------------- "

(defmethod .+ ((tensor tensor) (tensor2 tensor))
    "Creates a tensor with the sum of the corresponding elements of the argument
     tensors."
    (map-tensor #'+ tensor tensor2))

(defmethod .% ((tensor tensor) (tensor2 tensor))
    " - .% : tensor, tensor -> tensor : receives two tensors and return a new tensor
      that contains the remainder between the elements of the tensors."
    (map-tensor #'rem tensor tensor2))

(defmethod .% ((scalar scalar) (tensor tensor))
    " - .% : scalar, tensor -> tensor : receives two tensors and return a new tensor that
      contains the remainder between the scalar and the elements of the tensor."
    (map-tensor #'rem (scalar-to-tensor scalar tensor) tensor))

(defmethod .% ((tensor tensor) (scalar scalar))
  " - .% : tensor, scalar -> tensor : receives two tensors and return a new tensor that contains the
    remainder between the elements of the tensor and the scalar."
    (map-tensor #'rem tensor (scalar-to-tensor scalar tensor)))

(defmethod .> ((tensor tensor) (tensor2 tensor))
  " - .> : tensor, tensor -> tensor : receives two tensors and return a new tensor that contains the
    result of the comparsion (greater then) between the elements of the tensors."
    (map-tensor #'> tensor tensor2))

(defmethod .> ((scalar scalar) (tensor tensor))
  " - .> : tensor, tensor -> tensor : receives two tensors and return a new tensor that contains the
    result of the comparsion (greater then) between the scalar and the elements of the tensor."
    (map-tensor #'> (scalar-to-tensor scalar tensor) tensor))

(defmethod .> ((tensor tensor) (scalar scalar))
  " - .> : tensor, tensor -> tensor : receives two tensors and return a new tensor that contains the
    result of the comparsion (greater then) between the elements of the tensor and the scalar."
    (map-tensor #'> tensor (scalar-to-tensor scalar tensor)))

(defmethod .>= ((tensor tensor) (tensor2 tensor))
  " - .>= : tensor, tensor -> tensor : receives two tensors and return a new tensor that contains the
    result of the comparsion (greater equals then) between the elements of the tensors."
    (map-tensor #'>= tensor tensor2))

(defmethod .>= ((scalar scalar) (tensor tensor))
  " - .>= : tensor, tensor -> tensor : receives two tensors and return a new tensor that contains the
    result of the comparsion (greater equals then) between the scalar and the elements of the tensor."
    (map-tensor #'>= (scalar-to-tensor scalar tensor) tensor))

(defmethod .>= ((tensor tensor) (scalar scalar))
  " - .>= : tensor, tensor -> tensor : receives two tensors and return a new tensor that contains the
    result of the comparsion (greater equals then) between the elements of the tensor and the scalar.."
    (map-tensor #'>= tensor (scalar-to-tensor scalar tensor)))

(defmethod .or ((tensor tensor) (tensor2 tensor))
  " - .or : tensor, tensor -> tensor : receives two tensors and return a new tensor that contains the
    result of the logical comparsion (or) between the elements of the tensors."
    (map-tensor (compose #'bool->int (lambda (e1 e2) (or e1 e2))) tensor tensor2))

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
    (map-tensor (lambda (e1 e2) (truncate (/ e1 e2))) (scalar-to-tensor scalar tensor) tensor))

(defmethod .// ((tensor tensor) (scalar scalar))
    "Creates a tensor with the integer division of the corresponding elements
    of the argument tensor and the scalar."
    (map-tensor (lambda (e1 e2) (truncate (/ e1 e2))) tensor (scalar-to-tensor scalar tensor)))

(defmethod .< ((tensor tensor) (tensor2 tensor))
    "Creates a tensor using the relation \"less than\" on the corresponding
     elements of the argument tensors. The result tensor will have, as elements,
     the integers 0 or 1."
    (map-tensor (compose #'bool->int #'<) tensor tensor2))

(defmethod .< ((scalar scalar) (tensor tensor))
    "Creates a tensor using the relation \"less than\" on the corresponding
    elements scalar and the argument tensor. The result tensor will have, as elements,
    the integers 0 or 1."
    (map-tensor (compose #'bool->int #'<) (scalar-to-tensor scalar tensor) tensor))

(defmethod .< ((tensor tensor) (scalar scalar))
    "Creates a tensor using the relation \"less than\" on the corresponding
    elements of the argument tensor and the scalar. The result tensor will have, as
    elements, the integers 0 or 1."
    (map-tensor (compose #'bool->int #'<) tensor (scalar-to-tensor scalar tensor)))

(defmethod .<= ((tensor tensor) (tensor2 tensor))
    "Creates a tensor using the relation \"less or equal than\" on the corresponding
     elements of the argument tensors. The result tensor will have, as elements,
     the integers 0 or 1."
    (map-tensor (compose #'bool->int #'<=) tensor tensor2))

(defmethod .<= ((scalar scalar) (tensor tensor))
    "Creates a tensor using the relation \"less or equal than\" on the corresponding
     elements scalar and the argument tensors. The result tensor will have, as elements,
     the integers 0 or 1."
    (map-tensor (compose #'bool->int #'<=) (scalar-to-tensor scalar tensor) tensor))

(defmethod .<= ((tensor tensor) (scalar scalar))
     "Creates a tensor using the relation \"less or equal than\" on the corresponding
      elements of the argument tensors. The result tensor will have, as elements,
      the integers 0 or 1."
      (map-tensor (compose #'bool->int #'<=) tensor (scalar-to-tensor scalar tensor)))

(defmethod .= ((tensor tensor) (tensor2 tensor))
    "Creates a tensor using the relation \"less or equal than\" on the corresponding
     elements of the argument tensors. The result tensor will have, as elements,
     the integers 0 or 1."
    (map-tensor (compose #'bool->int #'=) tensor tensor2))

(defmethod .and ((tensor tensor) (tensor2 tensor))
    "Creates a tensor using the relation \"less or equal than\" on the corresponding
     elements of the argument tensors. The result tensor will have, as elements,
     the integers 0 or 1."
    (map-tensor (compose #'bool->int (lambda (e1 e2) (and e1 e2))) tensor tensor2))

(defmethod member? ((tensor tensor) (elements tensor))
    (map-tensor (compose #'bool->int
                         (lambda (tensor-elem)
                            (fold-tensor (lambda (elem acc)
                                            (or (eql tensor-elem elem)
                                                 acc))
                                         elements
                                         nil)))
                tensor))

" ---------------------------- Monadic Operators ----------------------------- "

(defun scan (fn)
  (lambda (tensor)
    (reduce-subsets fn (tensor-content tensor) 0 1)))

(defun reshape (tensor-dimensions tensor-content)
    (let ((counter 0))
        (labels ((rec (dimensions content)
                    (cond ((null (cdr dimensions))
                            (let ((result '()))
                                (dotimes (i (car dimensions))
                                    (setf result (cons (nth (mod counter (length content)) content) result))
                                    (incf counter))
                                (reverse result)))
                          (t
                            (let ((result '()))
                                (dotimes (i (car dimensions))
                                    (setf result (cons (rec (cdr dimensions) content) result)))
                                (reverse result))))))
            (rec (tensor-content tensor-dimensions) (tensor-content tensor-content)))))
