" ------------------------------- Classes ----------------------------------- "

"Defines the class that represents a tensor. A tensor is an object that
 contains a list that stores the elements of the tensor."
(defclass tensor ()
    ((data :type list
           :reader tensor-content
           :initarg :initial-content)))

"Defines the class that represents a scalar. A scalar is a subclass of tensor."
(defclass scalar (tensor)
  ())

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

" ------------------------------- Utility Functions ------------------------- "
(defun bool->int (bool)
    "Returns a number depending on the argument.
     If the argument is true, it returns 1.
     If the argument is false, it returns 0."
    (if bool
        1
        0))

(defun int->bool (int)
    "Returns a boolean (true or false) depending on the argument. If
     the argument is greater than 0, it returns true, otherwise returns nil."
    (if (zerop int)
        nil
        t))

(defun compose (fn1 fn2)
    "Composes two functions. It only works for the case where the arguments
     passed to the resulting function belong to the second function."
    (lambda (&rest args)
        (funcall fn1 (apply fn2 args))))

(defun curry (fn &rest args)
    "Returns a function that already has some arguments applied."
    (lambda (&rest more-args)
        (apply fn (append args more-args))))

(defun flatten (arg)
    "Returns a list without nested lists. The elements of the nested lists are inserted in order in
     the result list."
    (cond ((null arg)
            nil)
          ((atom (car arg))
            (cons (car arg) (cdr arg)))
          (t
            (append (flatten (car arg)) (flatten (cdr arg))))))

(defun list-dimensions (lst)
    "Returns the dimensions of a list. If the list does not contain nested lists, it returns a list
     that contains the length of the argument list,oOtherwise returns a list that contains the
     dimensions of the sublists of the argument list."
    (when (listp lst)
          (cons (length lst) (list-dimensions (car lst)))))

(defun fold-tensor (function tensor initial-value)
    "Applies the fold-tree function to the contents of the tensor."
    (fold-tree function (tensor-content tensor) initial-value))

(defun fold-tree (function list initial-value)
    "Applies the reduce function in the flatten list. If the
     argument list is empty, the initial value is returned."
    (reduce function (flatten list) :initial-value initial-value))

(defun map-tensor (function &rest tensors)
    "Applies the map-tree function to the contents of the tensors."
    (make-instance 'tensor :initial-content (apply #'map-tree function (mapcar #'tensor-content tensors))))

(defun map-tree (function &rest lists)
    "Applies the function over the elements of the lists, even if there are nested lists.
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

(defun reduce-subsets (fn lst begin end)
    "Returns a list that contains the result of applying the function fn to all
     subsets of the list."
    (if (< (length lst) end)
        nil
        (cons (reduce fn lst :start begin :end end) (reduce-subsets fn lst begin (+ end 1)))))

(defun remove-element (index list)
    "Returns a list without the element that is placed at index *index* at the original list.
     The index either starts at 1 or -1, where 1 corresponds to the first element of the list and
     -1 means corresponds to the last element of the list."
    (cond ((eql 0 index)
            list)
          ((eql 1 index)
            (cdr list))
          ((> index 0)
            (cons (car list) (remove-element (- index 1) (cdr list))))
          ((< index 0)
            (remove-element (+ (- (length list) (- index)) 1) list))))

(defun remove-elements (number list)
    (cond ((zerop number)
            list)
          ((> number 0)
            (remove-elements (- number 1)
                             (remove-element 1 list)))
          ((< number 0)
            (remove-elements (+ number 1)
                             (remove-element -1 list)))))


(defun scalar-to-real (scalar)
    "Receives a scalar and returns the value of the scalar."
    (car (tensor-content scalar)))

(defun scalar-to-tensor (scalar tensor)
    "Receives a scalar and a tensor and returns a new tensor with the dimension
     of the argument tensor. The new tensor is populated with the value of the
     argument scalar."
    (if (null (tensor-content (shape tensor)))
        scalar
        (reshape (shape tensor) scalar)))

(defun scalar? (tensor)
    "Receives a tensor and returns true if the tensor is a scalar, otherwise returns false."
    (= (first (tensor-content (rank tensor))) 0))

(defun vector? (tensor)
    "Receives a tensor and returns true if the tensor is a vector, otherwise returns false."
    (= (first (tensor-content (rank tensor))) 1))

(defun matrix? (tensor)
    "Receives a tensor and returns true if the tensor is a matrix, otherwise returns false."
    (> (first (tensor-content (rank tensor))) 1))

" --------------------------- Tensor Constructors ---------------------------- "

(defun s (element)
    "Receives an element and returns a scalar with that element."
    (make-instance 'scalar :initial-content (list element)))

(defun v (&rest elements)
    "Receives zero or more elements and returns a tensor with those elements."
    (make-instance 'tensor :initial-content elements))

" ---------------------------- Monadic Functions ----------------------------- "

(defun .- (tensor &optional tensor2)
    "Creates a new tensor whose elements are the symmetric of the corresponding elements of the
     argument tensor."
    (if tensor2
        (subtract tensor tensor2)
        (symmetric tensor)))

(defun symmetric (tensor)
    (map-tensor #'- tensor))

(defun ./ (tensor &optional tensor2)
    "Creates a new tensor whose elements are the inverse of the corresponding
     elements of the argument tensor."
    (if tensor2
        (division tensor tensor2)
        (inverse tensor)))

(defun inverse (tensor)
    (map-tensor #'/ tensor))

(defun .! (tensor)
    "Receives a tensor and returns a new tensor where the function factorial is applied
     element-wise."
    (map-tensor #'! tensor))

(defun .sin (tensor)
    "Receives a tensor and returns a new tensor where the function sin is applied element-wise."
    (map-tensor #'sin tensor))

(defun .cos (tensor)
    "Creates a new tensor whose elements are the result of applying the cos function to the
     corresponding elements of the argument tensor."
    (map-tensor #'cos tensor))

(defun .not (tensor)
    "Receives a tensor and returns a new tensor where the function not is applied element-wise."
    (map-tensor #'(lambda (x) (if (zerop x) 1 0))
                tensor))

(defmethod shape ((scalar scalar))
    "Receives a scalar and returns a new tensor that contains an empty list because a scalar has 0
     dimensions."
    (make-instance 'tensor :initial-content '()))

(defmethod shape ((tensor tensor))
    "Receives a tensor and returns a new tensor that contains the length of each dimension of the
     argument tensor."
    (make-instance 'tensor :initial-content (list-dimensions (tensor-content tensor))))

(defun interval (n)
    "Returns a tensor containing an enumeration of all integers starting
     from 1 up to the argument."
    (labels ((rec (i n)
                (unless (> i n)
                    (cons i (rec (1+ i) n)))))
        (apply #'v (rec 1 n))))

" ---------------------------- Dyadic Functions ----------------------------- "

(defmethod .+ ((tensor tensor) (tensor2 tensor))
    "Returns a tensor with the sum of the corresponding elements of the argument tensors."
    (map-tensor #'+ tensor tensor2))

(defmethod .+ ((scalar scalar) (tensor tensor))
    "Returns a tensor with the sum of the corresponding elements of the scalar and the argument
     tensor."
    (map-tensor #'+ (scalar-to-tensor scalar tensor) tensor))

(defmethod .+ ((tensor tensor) (scalar scalar))
    "Returns a tensor with the sum of the corresponding elements of the argument tensor and the
     scalar."
    (map-tensor #'+ tensor (scalar-to-tensor scalar tensor)))

(defmethod subtract ((tensor1 tensor) (tensor2 tensor))
    "Returns a tensor with the subtraction of the corresponding elements of the argument tensors."
    (map-tensor #'- tensor1 tensor2))

(defmethod subtract ((scalar scalar) (tensor tensor))
    "Returns a tensor with the subtraction of the corresponding elements of the scalar and the
     argument tensor."
    (map-tensor #'- (scalar-to-tensor scalar tensor) tensor))

(defmethod subtract ((tensor tensor) (scalar scalar))
    "Returns a tensor with the subtraction of the corresponding elements of the argument tensor and
     the scalar."
    (map-tensor #'- tensor (scalar-to-tensor scalar tensor)))

(defmethod .* ((tensor tensor) (tensor2 tensor))
    "Returns a tensor with the multiplication of the corresponding elements of the argument
     tensors."
    (map-tensor #'* tensor tensor2))

(defmethod .* ((scalar scalar) (tensor tensor))
    "Returns a tensor with the multiplication of the corresponding elements of the scalar and the
     argument tensor."
    (map-tensor #'* (scalar-to-tensor scalar tensor) tensor))

(defmethod .* ((tensor tensor) (scalar scalar))
    "Returns a tensor with the multiplication of the corresponding elements of the argument tensor
     and the scalar."
    (map-tensor #'* tensor (scalar-to-tensor scalar tensor)))

(defmethod division ((tensor1 tensor) (tensor2 tensor))
    "Returns a tensor with the multiplication of the corresponding elements of the argument tensors."
    (map-tensor #'/ tensor1 tensor2))

(defmethod division ((scalar scalar) (tensor tensor))
    "Returns a tensor with the multiplication of the corresponding elements of the scalar and the
     argument tensor."
    (map-tensor #'/ (scalar-to-tensor scalar tensor) tensor))

(defmethod division ((tensor tensor) (scalar scalar))
    "Returns a tensor with the division of the corresponding elements of the argument tensor and the
     scalar."
    (map-tensor #'/ tensor (scalar-to-tensor scalar tensor)))

(defmethod .// ((tensor tensor) (tensor2 tensor))
    "Returns a tensor with the integer division of the corresponding elements
     of the argument tensors."
    (map-tensor (lambda (e1 e2) (truncate (/ e1 e2))) tensor tensor2))

(defmethod .// ((scalar scalar) (tensor tensor))
    "Returns a tensor with the integer division of the scalar and the corresponding elements of the
     argument tensor."
    (map-tensor (lambda (e1 e2) (truncate (/ e1 e2))) (scalar-to-tensor scalar tensor) tensor))

(defmethod .// ((tensor tensor) (scalar scalar))
    "Returns a tensor with the integer division of the corresponding elements of the argument tensor
     and the scalar."
    (map-tensor (lambda (e1 e2) (truncate (/ e1 e2))) tensor (scalar-to-tensor scalar tensor)))

(defmethod .% ((tensor tensor) (tensor2 tensor))
    "Receives two tensors and returns a new tensor that contains the remainder between the elements
     of the tensors."
    (map-tensor #'rem tensor tensor2))

(defmethod .% ((scalar scalar) (tensor tensor))
    "Receives a scalar and a tensor and returns a new tensor that contains the remainder between the
     scalar and the elements of the tensor."
    (map-tensor #'rem (scalar-to-tensor scalar tensor) tensor))

(defmethod .% ((tensor tensor) (scalar scalar))
    "Receives a tensor and a scalar and returns a new tensor that contains the remainder between the
     elements of the tensor and the scalar."
    (map-tensor #'rem tensor (scalar-to-tensor scalar tensor)))

(defmethod .and ((tensor tensor) (tensor2 tensor))
    "Returns a tensor using the logical comparison (and) on the corresponding elements of the
     argument tensors. The result tensor will have, as elements, the integers 0 or 1."
    (map-tensor (compose #'bool->int
                         (lambda (e1 e2) (and (int->bool e1) (int->bool e2))))
                tensor
                tensor2))

(defmethod .and ((scalar scalar) (tensor tensor))
    "Returns a tensor using the relation \"less or equal than\" on the corresponding elements of
     scalar and the scalar and the argument tensor. The result tensor will have, as elements, the
     integers 0 or 1."
    (map-tensor (compose #'bool->int
                         (lambda (e1 e2) (and (int->bool e1) (int->bool e2))))
                (scalar-to-tensor scalar tensor)
                tensor))

(defmethod .and ((tensor tensor) (scalar scalar))
    "Returns a tensor using the relation \"less or equal than\" on the corresponding elements of the
     argument tensor and the scalar. The result tensor will have, as elements, the integers 0 or 1."
    (map-tensor (compose #'bool->int
                         (lambda (e1 e2) (and (int->bool e1) (int->bool e2))))
                tensor
                (scalar-to-tensor scalar tensor)))

(defmethod .or ((tensor tensor) (tensor2 tensor))
    "Receives two tensors and returns a new tensor that contains the result of the logical
     comparison (or) between the elements of the tensors."
    (map-tensor (compose #'bool->int
                         (lambda (e1 e2) (or (int->bool e1) (int->bool e2))))
                tensor
                tensor2))

(defmethod .or ((scalar scalar) (tensor tensor))
    "Receives a scalar and a tensor and returns a new tensor that contains the result of the logical
     comparison (or) between the scalar and the elements of the tensor."
    (map-tensor (compose #'bool->int
                        (lambda (e1 e2) (or (int->bool e1) (int->bool e2))))
                (scalar-to-tensor scalar tensor)
                tensor))

(defmethod .or ((tensor tensor) (scalar scalar))
    "Receives a tensor and a scalar and returns a new tensor that contains the result of the logical
     comparison (or) between the elements of the tensor and the scalar."
    (map-tensor (compose #'bool->int
                         (lambda (e1 e2) (or (int->bool e1) (int->bool e2))))
                tensor
                (scalar-to-tensor scalar tensor)))

(defmethod .> ((tensor tensor) (tensor2 tensor))
    "Receives two tensors and returns a new tensor that contains the result of the comparison
     (greater than) between the elements of the tensors."
    (map-tensor (compose #'bool->int #'>) tensor tensor2))

(defmethod .> ((scalar scalar) (tensor tensor))
    "Receives a scalar and a tensor and returns a new tensor that contains the result of the
     comparison (greater than) between the scalar and the elements of the tensor."
    (map-tensor (compose #'bool->int #'>) (scalar-to-tensor scalar tensor) tensor))

(defmethod .> ((tensor tensor) (scalar scalar))
    "Receives a tensor and a scalar and returns a new tensor that contains the result of the
     comparison (greater than) between the elements of the tensor and the scalar."
    (map-tensor (compose #'bool->int #'>) tensor (scalar-to-tensor scalar tensor)))

(defmethod .>= ((tensor tensor) (tensor2 tensor))
    "Receives two tensors and returns a new tensor that contains the result of the comparison
     (greater or equal than) between the elements of the tensors."
    (map-tensor (compose #'bool->int #'>=) tensor tensor2))

(defmethod .>= ((scalar scalar) (tensor tensor))
    "Receives a scalar and a tensor and returns a new tensor that contains the result of the
     comparison (greater or equal than) between the scalar and the elements of the tensor."
    (map-tensor (compose #'bool->int #'>=) (scalar-to-tensor scalar tensor) tensor))

(defmethod .>= ((tensor tensor) (scalar scalar))
    "Receives a tensor and a scalar and returns a new tensor that contains the result of the
     comparison (greater or equal than) between the elements of the tensor and the scalar."
    (map-tensor (compose #'bool->int #'>=) tensor (scalar-to-tensor scalar tensor)))

(defmethod .= ((tensor tensor) (tensor2 tensor))
    "Returns a tensor using the relation \"equal than\" on the corresponding elements of the
     argument tensors. The result tensor will have, as elements, the integers 0 or 1."
    (map-tensor (compose #'bool->int #'=) tensor tensor2))

(defmethod .= ((scalar scalar) (tensor tensor))
    "Returns a tensor using the relation \"equal than\" on the corresponding elements of the scalar
     and the argument tensor. The result tensor will have, as elements, the integers 0 or 1."
    (map-tensor (compose #'bool->int #'=) (scalar-to-tensor scalar tensor) tensor))

(defmethod .= ((tensor tensor) (scalar scalar))
    "Returns a tensor using the relation \"equal than\" on the corresponding elements of the
     argument tensor and the scalar. The result tensor will have, as elements, the integers 0 or 1."
    (map-tensor (compose #'bool->int #'=) tensor (scalar-to-tensor scalar tensor)))

(defmethod .< ((tensor tensor) (tensor2 tensor))
    "Returns a tensor using the relation \"less than\" on the corresponding elements of the argument
     tensors. The result tensor will have, as elements, the integers 0 or 1."
    (map-tensor (compose #'bool->int #'<) tensor tensor2))

(defmethod .< ((scalar scalar) (tensor tensor))
    "Returns a tensor using the relation \"less than\" on the corresponding elements scalar and the
     argument tensor. The result tensor will have, as elements, the integers 0 or 1."
    (map-tensor (compose #'bool->int #'<) (scalar-to-tensor scalar tensor) tensor))

(defmethod .< ((tensor tensor) (scalar scalar))
    "Returns a tensor using the relation \"less than\" on the corresponding elements of the argument
     tensor and the scalar. The result tensor will have, as elements, the integers 0 or 1."
    (map-tensor (compose #'bool->int #'<) tensor (scalar-to-tensor scalar tensor)))

(defmethod .<= ((tensor tensor) (tensor2 tensor))
    "Returns a tensor using the relation \"less or equal than\" on the corresponding elements of the
     argument tensors. The result tensor will have, as elements, the integers 0 or 1."
    (map-tensor (compose #'bool->int #'<=) tensor tensor2))

(defmethod .<= ((scalar scalar) (tensor tensor))
    "Returns a tensor using the relation \"less or equal than\" on the corresponding elements scalar
     and the argument tensors. The result tensor will have, as elements, the integers 0 or 1."
    (map-tensor (compose #'bool->int #'<=) (scalar-to-tensor scalar tensor) tensor))

(defmethod .<= ((tensor tensor) (scalar scalar))
     "Returns a tensor using the relation \"less or equal than\" on the corresponding
      elements of the argument tensor and the scalar. The result tensor will have, as elements,
      the integers 0 or 1."
     (map-tensor (compose #'bool->int #'<=) tensor (scalar-to-tensor scalar tensor)))

(defun drop (t1 t2)
   "Accepts a scalar n1 or vector (of elements ni) and a non-scalar tensor and returns a tensor
    where the first (if n > 0) or last (if n < 0) n elements of the i dimension of the tensor
    are removed."
   (labels ((rec (remove-list lst)
                (if (eql (length remove-list) 1)
                    (remove-elements (car remove-list) lst)
                    (let ((mod-lst (remove-elements (car remove-list) lst)))
                        (mapcar (curry #'rec (cdr remove-list)) mod-lst)))))
        (make-instance 'tensor :initial-content (rec (tensor-content t1) (tensor-content t2)))))

(defun reshape (tensor-dimensions tensor-content)
    "Returns a tensor with the dimensions provided in the first argument and elements taken from the
     second argument, repeating them as necessary to fill the resulting tensor."
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
            (make-instance 'tensor :initial-content (rec (flatten (tensor-content tensor-dimensions))
                                                         (flatten (tensor-content tensor-content)))))))

(defmethod catenate ((s1 scalar) (s2 scalar))
    "Returns a tensor containing the concatenated values of the arguments scalars."
    (make-instance 'tensor
                   :initial-content (list (first (tensor-content s1)) (first (tensor-content s2)))))

(defmethod catenate ((scalar scalar) (tensor tensor))
    (let ((correct-dim (remove-element -1 (tensor-content (shape tensor)))))
        (catenate (reshape (apply #'v (append correct-dim (list 1)))
                           scalar)
                  tensor)))

(defmethod catenate ((tensor tensor) (scalar scalar))
    (let ((correct-dim (remove-element -1 (tensor-content (shape tensor)))))
        (catenate tensor
                  (reshape (apply #'v (append correct-dim (list 1)))
                           scalar))))

(defmethod catenate ((t1 tensor) (t2 tensor))
    "Returns a tensor that joins the arguments along their last dimension."
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
                                                    (append-elements-last-dim (tensor-content (add-dim-1 t1)) (tensor-content t2)))
                                                  (t
                                                    (append-elements-last-dim (tensor-content t1) (tensor-content (add-dim-1 t2)))))))))

(defun member? (tensor elements)
    "Returns a tensor of booleans with the same shape and dimension of the first argument,
     containing 1 for each element in the corresponding location in the first argument that
     occurs somewhere in the second argument and 0 otherwise."
    (map-tensor (compose #'bool->int
                         (lambda (tensor-elem)
                            (fold-tensor (lambda (acc elem)
                                            (or acc (eql tensor-elem elem)))
                                         elements
                                         nil)))
                tensor))

(defun select (filter-tensor elements-tensor)
    "From a tensor of booleans and another tensor, returns a tensor containing only the elements of
     the last dimension of the second argument whose corresponding element in the first tensor is 1."
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
    "Accepts a function and returns another function that, given a vector, computes the application
     of the function to sucessive elements of the vector."
    (lambda (tensor)
        (reduce fn (mapcar #'s (tensor-content tensor)))))

(defun scan (fn)
    "Similar to the fold function but using increasingly larger subsets of the elements of the
     vector, starting from a subset containing just the first element up to a subset containing all
     elements."
    (lambda (tensor)
      (make-instance 'tensor :initial-content (reduce-subsets fn (mapcar #'s (tensor-content tensor)) 0 1))))

(defun outer-product-aux (fn lst1 lst2)
    (let ((result-lst '()))
      (loop for i in lst1
        do (loop for j in lst2
          do (setf result-lst (append result-lst (list (funcall fn i j))))))
    (map 'list #'scalar-to-real result-lst)))

(defun outer-product (fn)
    "Accepts a function and returns another functions that, given two tensors, returns a
     new tensor with the result of applying the function to every combination of values from the
     first and second tensors."
    (lambda (tensor1 tensor2)
      (let ((row-dim (tensor-content (shape tensor1)))
            (col-dim (tensor-content (shape tensor2))))
            (reshape (make-instance 'tensor :initial-content (append row-dim col-dim))
                      (make-instance 'tensor :initial-content
                        (outer-product-aux fn (mapcar #'s (flatten (tensor-content tensor1)))
                                              (mapcar #'s (flatten (tensor-content tensor2)))))))))

" --------------------------- Dyadic Operators ------------------------------- "

(defun inner-product (fn1 fn2)
    "Accepts two functions and returns a function that, given two tensors, returns a new tensor
     computed according to the rules of the algebraic inner product but replacing the algebraic
     sum and product with the first and second functions."
    (labels ((number-columns (tensor)
                (car (last (tensor-content (shape tensor)))))
             (number-lines (tensor)
                (first (tensor-content (shape tensor))))
             (scalar->matrix (tensor size)
                (let ((result '())
                      (num (car (tensor-content tensor))))
                    (dotimes (i size)
                        (dotimes (j size)
                            (setf result (cons (if (eql i j) num 0)
                                               result))))
                    (reshape (v size size) (apply #'v (reverse result)))))
             (vec1->matrix (tensor)
                (reshape (apply #'v (append (list 1) (tensor-content (shape tensor))))
                         tensor))
             (vec2->matrix (tensor)
                (reshape (apply #'v (append (tensor-content (shape tensor)) (list 1)))
                         tensor))
             (apply-fn1 (line t1 t2)
                (let ((columns '()))
                    (dotimes (col-idx (number-columns t2))
                        (setf columns (cons (reduce fn1 (apply-fn2 col-idx line t1 t2))
                                            columns)))
                    (reverse columns)))
             (apply-fn2 (col-idx line t1 t2)
                (let ((flat-t2 (flatten (tensor-content t2)))
                      (result '()))
                    (dotimes (elem-idx (number-columns t1))
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
            (make-instance
                'tensor
                :initial-content (let ((mod-t1 (cond ((scalar? t1) (scalar->matrix t1 (number-lines t2)))
                                                     ((vector? t1) (vec1->matrix t1))
                                                     (t t1)))
                                       (mod-t2 (cond ((scalar? t2) (scalar->matrix t2 (number-columns t1)))
                                                     ((vector? t2) (vec2->matrix t2))
                                                     (t t2))))
                                    (compute-matrix
                                        (map-tensor #'s mod-t1)
                                        (map-tensor #'s mod-t2)))))))

" ---------------------------- Exercises -------------------------------------- "

(defun tally (tensor)
    "Returns a scalar with the number of elements of the tensor."
    (funcall (fold #'.*) (shape tensor)))

(defmethod rank ((scalar scalar))
    "Returns a scalar with the number of dimensions of the tensor, which is 0 in the case of a scalar."
    (s 0))

(defmethod rank ((tensor tensor))
    "Returns a scalar with the number of dimensions of the tensor."
    (funcall (fold #'.+) (.<= (s 0) (shape tensor))))

(defun within (numbers inf sup)
    "Given a vector of numbers *numbers* and two numbers inf and sup, returns a vector containing
     only the elements of *numbers* that are in the range between inf and sup."
    (select (.and (.>= numbers inf) (.<= numbers sup)) numbers))

(defun ravel (tensor)
    "Returns a vector containing all the elements of the tensor."
    (reshape (tally tensor) tensor))

(defun primes (index)
    "Returns a vector with all prime numbers from 2 up to the scalar, including the scalar."
    (let ((numbers (drop (s 1) (interval (car (tensor-content index))))))
        (select (.not (member? numbers (funcall (outer-product #'.*) numbers numbers)))
                numbers)))
