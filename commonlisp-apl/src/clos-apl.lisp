;;; Utility functions
(defun compose (fn1 fn2)
    "Composes two functions. It only works for the case where the arguments
     passed to the resulting function belong to the second function."
    (lambda (&rest args)
        (funcall fn1 (apply fn2 args))))

(defun curry (fn &rest args)
    (lambda (&rest more-args)
        (apply fn (append args more-args))))

(defun bool->int (bool)
    "Returns a number depending on the argument.
     If the argument is true, it returns 1.
     If the argument is false, it returns 0."
    (if bool
        1
        0))

(defun int->bool (int)
    (if (zerop int)
        nil
        t))

;;; Project implementation
(defclass tensor ()
    ((data :type list
           :reader tensor-content
           :initarg :initial-content)))

(defclass scalar (tensor) ())

(defun list-dimensions (lst)
    (when (listp lst)
          (cons (length lst) (list-dimensions (car lst)))))

(defun matrix? (tensor)
    (> (first (tensor-content (rank tensor))) 1))

(defun vector? (tensor)
    (= (first (tensor-content (rank tensor))) 1))

(defun remove-element (index list)
    (cond ((eql 0 index)
            list)
          ((eql 1 index)
            (cdr list))
          ((> index 0)
            (cons (car list) (remove-element (- index 1) (cdr list))))
          ((< index 0)
            (remove-element (+ (- (length list) (- index)) 1) list))))

(defun reduce-subsets (fn vector begin end)
  (if (< (length vector) end)
      nil
   (cons (reduce fn vector :start begin :end end) (reduce-subsets fn vector begin (+ end 1)))))

(defun scalar-to-tensor (scalar tensor)
  (let* ((n (first (tensor-content scalar)))
        (dim (list-length (tensor-content tensor))))
    (make-instance 'tensor :initial-content (make-list dim :initial-element n))))

(defun map-tensor (function &rest tensors)
    (make-instance 'tensor :initial-content (apply #'map-tree function (mapcar #'tensor-content tensors))))

(defun map-tree (function &rest lists)
    "Maps the function over the lists.
     Assumes that all lists are of the same dimensions.
     Returns a new list of the same dimension."
    (let ((lst (car lists)))
        (cond ((null lst) nil)
              ((atom (car lst))
                  (cons (apply function (mapcar #'car lists))
                        (apply #'map-tree function (mapcar #'cdr lists))))
              (t
                  (cons (apply #'map-tree function (mapcar #'car lists))
                        (apply #'map-tree function (mapcar #'cdr lists)))))))

(defun fold-tensor (function tensor initial-value)
    (fold-tree function (tensor-content tensor) initial-value))

(defun flatten (arg)
    (cond ((null arg)
            nil)
          ((atom (car arg))
            (cons (car arg) (cdr arg)))
          (t
            (append (flatten (car arg)) (flatten (cdr arg))))))

(defun fold-tree (function list initial-value)
    (reduce function (flatten list) :initial-value initial-value))

(defmethod print-object ((tensor tensor) (stream stream))
    "Implementation of the generic method print-object for the tensor data structure.
     If the tensor is a vector, prints its elements separated by a whitespace.
     If the tensor is not one of the previous cases, then for each sub-tensor of the
     first dimension, prints the sub-tensor separated from the next sub-tensor by a
     number of empty lines that is equal to the number of dimensions minus one."
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
        (rec (tensor-content tensor) t)))

" --------------------------- Tensor Constructors ---------------------------- "

" - s : element -> tensor : receives a parameter and returns a scalar."
(defun s (element) (make-instance 'scalar :initial-content (list element)))

" - v : element -> tensor : receives a parameter list and returns a vector."
(defun v (&rest elements) (make-instance 'tensor :initial-content elements))

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

(defmethod shape ((scalar scalar))
    (make-instance 'tensor :initial-content '()))

(defmethod shape ((tensor tensor))
    " - shape : tensor -> tensor : receives a tensor and return a new tensor
     that contains the length of each dimension of the tensor."
     (make-instance 'tensor :initial-content (list-dimensions (tensor-content tensor))))

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
    (map-tensor (compose #'bool->int #'>) tensor tensor2))

(defmethod .> ((scalar scalar) (tensor tensor))
  " - .> : tensor, tensor -> tensor : receives two tensors and return a new tensor that contains the
    result of the comparsion (greater then) between the scalar and the elements of the tensor."
    (map-tensor (compose #'bool->int #'>) (scalar-to-tensor scalar tensor) tensor))

(defmethod .> ((tensor tensor) (scalar scalar))
  " - .> : tensor, tensor -> tensor : receives two tensors and return a new tensor that contains the
    result of the comparsion (greater then) between the elements of the tensor and the scalar."
    (map-tensor (compose #'bool->int #'>) tensor (scalar-to-tensor scalar tensor)))

(defmethod .>= ((tensor tensor) (tensor2 tensor))
  " - .>= : tensor, tensor -> tensor : receives two tensors and return a new tensor that contains the
    result of the comparsion (greater equals then) between the elements of the tensors."
    (map-tensor (compose #'bool->int #'>=) tensor tensor2))

(defmethod .>= ((scalar scalar) (tensor tensor))
  " - .>= : tensor, tensor -> tensor : receives two tensors and return a new tensor that contains the
    result of the comparsion (greater equals then) between the scalar and the elements of the tensor."
    (map-tensor (compose #'bool->int #'>=) (scalar-to-tensor scalar tensor) tensor))

(defmethod .>= ((tensor tensor) (scalar scalar))
  " - .>= : tensor, tensor -> tensor : receives two tensors and return a new tensor that contains the
    result of the comparsion (greater equals then) between the elements of the tensor and the scalar.."
    (map-tensor (compose #'bool->int #'>=) tensor (scalar-to-tensor scalar tensor)))

(defmethod .or ((tensor tensor) (tensor2 tensor))
  " - .or : tensor, tensor -> tensor : receives two tensors and return a new tensor that contains the
    result of the logical comparsion (or) between the elements of the tensors."
    (map-tensor (compose #'bool->int (lambda (e1 e2) (or (int->bool e1) (int->bool e2)))) tensor tensor2))

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
    (map-tensor (compose #'bool->int #'(lambda (e1 e2) (and (int->bool e1) (int->bool e2))))
                tensor
                tensor2))

(defun drop (t1 t2)
    (labels ((rec (remove-list lst)
                (if (eql (length remove-list) 1)
                    (remove-element (car remove-list) lst)
                    (let ((mod-lst (car remove-list) lst))
                        (mapcar (curry #'rec (cdr remove-list)) mod-lst)))))
        (make-instance 'tensor :initial-content (rec (tensor-content t1) (tensor-content t2)))))

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
            (make-instance 'tensor :initial-content (rec (tensor-content tensor-dimensions)
                                                         (flatten (tensor-content tensor-content)))))))

(defmethod catenate ((s1 scalar) (s2 scalar))
    (make-instance 'tensor
                   :initial-content (list (first (tensor-content s1)) (first (tensor-content s2)))))

(defmethod catenate ((scalar scalar) (tensor tensor))
    (catenate (reshape (shape tensor) scalar) tensor))

(defmethod catenate ((tensor tensor) (scalar scalar))
    (catenate tensor (reshape (shape tensor) scalar)))

(defmethod catenate ((t1 tensor) (t2 tensor))
    (labels ((append-elements-last-dim (list1 list2)
                (cond ((null list1)
                        nil)
                      ((atom (car list1))
                        (append list1 list2))
                      (t
                        (cons (append-elements-last-dim (car list1) (car list2))
                              (append-elements-last-dim (cdr list1) (cdr list2))))))
             (add-dim-1 (tensor)
                (reshape (apply #'v (append (tensor-content (shape tensor)) (list 1)))
                         (make-instance 'tensor :initial-content (flatten (tensor-content tensor))))))
        (let ((r1 (car (tensor-content (rank t1))))
              (r2 (car (tensor-content (rank t2)))))
            (make-instance 'tensor
                           :initial-content (cond ((eql r1 r2)
                                                    (append-elements-last-dim (tensor-content t1) (tensor-content t2)))
                                                  ((< r1 r2)
                                                    (append-elements-last-dim (add-dim-1 t1) (tensor-content t2)))
                                                  (t
                                                    (append-elements-last-dim (tensor-content t1) (add-dim-1 t2))))))))

(defun member? (tensor elements)
    (map-tensor (compose #'bool->int
                         (lambda (tensor-elem)
                            (fold-tensor (lambda (acc elem)
                                            (or acc (eql tensor-elem elem)))
                                         elements
                                         nil)))
                tensor))

(defun select (filter-tensor elements-tensor)
    (labels ((rec (filter elements)
                (cond ((null elements)
                        nil)
                      ((atom (car elements))
                        (if (zerop (car filter))
                            (rec (cdr filter) (cdr elements))
                            (cons (car elements) (rec (cdr filter) (cdr elements)))))
                      (t
                        (cons (rec filter (car elements)) (rec filter (cdr elements)))))))
        (make-instance 'tensor :initial-content (rec (tensor-content filter-tensor) (tensor-content elements-tensor)))))

" ---------------------------- Monadic Operators ----------------------------- "

(defun fold (fn)
    (lambda (tensor)
        (make-instance 'tensor :initial-content (tensor-content (reduce fn (mapcar #'s (tensor-content tensor)))))))

(defun scan (fn)
  (lambda (tensor)
    (make-instance 'tensor :initial-content (reduce-subsets fn (mapcar #'s (tensor-content tensor)) 0 1))))

(defun outer-product-aux (fn lst1 lst2)
  (let ((result-lst '()))
    (loop for i in lst1
      do (loop for j in lst2
        do (setf result-lst (append result-lst (list (funcall fn i j))))))
  result-lst))

(defun outer-product (fn)
  (lambda (tensor1 tensor2)
    (let ((row-dim (tensor-content (shape tensor1)))
          (col-dim (tensor-content (shape tensor2))))
      (reshape (make-instance 'tensor :initial-content (append row-dim col-dim))
        (make-instance 'tensor :initial-content
          (outer-product-aux fn (mapcar #'s (flatten (tensor-content tensor1)))
                                (mapcar #'s (flatten (tensor-content tensor2)))))))))

" --------------------------- Dyadic Operators ------------------------------- "

(defun inner-product (fn1 fn2)
    (labels ((number-columns (tensor)
                (car (last (tensor-content (shape tensor)))))
             (number-lines (tensor)
                (first (tensor-content (shape tensor))))
             (apply-fn1 (line t1 t2)
                (let ((columns '()))
                    (dotimes (col-idx (number-columns t2))
                        (setf columns (cons (reduce fn1 (apply-fn2 col-idx line t1 t2))
                                            columns)))
                    (reverse columns)))
             (apply-fn2 (col-idx line t1 t2)
                (let ((flat-t2 (flatten (tensor-content t2)))
                      (result '()))
                    (format t "line: ~A~%" line)
                    (format t "flat-t2: ~A~%~%" flat-t2)
                    (dotimes (elem-idx (number-columns t1))
                        (format t "col-idx: ~A~%" col-idx)
                        (format t "t1 elem: ~A~%" (nth elem-idx line))
                        (format t "t2 elem: ~A~%" (nth (+ col-idx (* elem-idx (number-columns t2))) flat-t2))
                        (setf result (cons (funcall fn2
                                                    (nth elem-idx line)
                                                    (nth (+ col-idx (* elem-idx (number-columns t2))) flat-t2))
                                     result)))
                    (reverse result)))
             (compute-matrix (t1 t2)
                (mapcar (lambda (line)
                            (apply-fn1 line t1 t2))
                        (tensor-content t1))))
        (lambda (t1 t2)
            (make-instance 'tensor
                    :initial-content (cond ((and (vector? t1) (vector? t2)))
                                           ((and (vector? t1) (matrix? t2))
                                             (compute-matrix (reshape (apply #'v (append (list 1) (tensor-content (shape t1))))
                                                                      t1)
                                                             t2))
                                           ((and (matrix? t1) (vector? t2))
                                             (compute-matrix t1
                                                             (reshape (apply #'v (append (tensor-content (shape t2)) (list 1)))
                                                                      t2)))
                                           (t
                                             (compute-matrix t1 t2)))))))

" ---------------------------- Exercises -------------------------------------- "

(defun tally (tensor)
    (funcall (fold #'.*) (shape tensor)))

(defmethod rank ((scalar scalar))
    (s 0))

(defmethod rank ((tensor tensor))
    (funcall (fold #'.+) (.< (s 0) (shape tensor))))

(defun within (numbers inf sup)
    (select (.and (.>= numbers inf) (.<= numbers sup)) numbers))

(defun ravel (tensor)
    (reshape (tally tensor) tensor))

(defun primes (index)
  (let ((numbers (drop (s 1) (interval index))))
    (select (.not (member? numbers (funcall (outer-product #'.*) numbers numbers))) numbers)))
