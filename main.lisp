(in-package #:its)

(defun %make-object-vars (count)
  (map-into (make-list count)
            (lambda ()
              (gensym (string '#:object)))))

(defmacro its (specification &body object)
  (destructuring-bind (&optional (object nil object-specified-p))
      object
    (let* ((object-var (first (%make-object-vars 1)))
           (body `((declare (ignorable ,object-var))
                   ,(funcall (expand-specification *kind-definitions* specification)
                             object-var))))
      (if object-specified-p
          `(let ((,object-var ,object))
             ,@body)
          `(lambda (,object-var)
             ,@body)))))

(define-setf-expander its (specification &body object &environment env)
  (destructuring-bind (&optional (object nil object-specified-p))
      object
    (if object-specified-p
        (let ((object-var (first (%make-object-vars 1))))
          (multiple-value-bind (vars vals stores writer reader)
              (get-setf-expansion
               (funcall (expand-specification *kind-definitions* specification)
                        object-var)
               env)
            (values (cons object-var vars) (cons object vals) stores writer reader)))
        (error "~S requires an object argument." '(setf its)))))

(defmacro their (specification &body objects)
  (typecase objects
    ((or null (cons (cons (integer 0) null) null))
     (let ((count (and objects
                       (destructuring-bind (count) (first objects)
                         (setf objects nil)
                         count))))
       (if count
           (let ((vars (%make-object-vars count)))
             `(lambda (,@vars)
                (declare (ignorable ,@vars))
                (their ,specification ,@vars)))
           (let ((its-function-var (gensym (string '#:its-function)))
                 (objects-var (gensym (string '#:objects))))
             `(let ((,its-function-var (its ,specification)))
                (lambda (&rest ,objects-var)
                  (values-list
                   (mapcan (lambda (object)
                             (multiple-value-list (funcall ,its-function-var object)))
                           ,objects-var))))))))
    ((cons t null)
     `(its ,specification ,(first objects)))
    (t
     (let ((its-function-var (gensym (string '#:its-function)))
           (values-operator (if (= (or (nth-value 1 (expand-specification *kind-definitions*
                                                                          specification))
                                       1)
                                   1)
                                '(values)
                                '(multiple-value-call #'values))))
       ;; Leave ITS unexpanded for more readable macroexpansion.
       `(let ((,its-function-var (its ,specification)))
          (,@values-operator
           ,@(mapcar (lambda (object)
                       `(funcall ,its-function-var ,object))
                     objects)))))))

(defun %extract-object-var-substitutions (object-var vars vals)
  (let* ((object-var-substitutions nil)
         (vars-vals
          (mapcan (lambda (var val)
                    (if (eq val object-var)
                        (prog1 nil
                          (push (list var object-var)
                                object-var-substitutions))
                        (list (cons var val))))
                  vars
                  vals)))
    (values (mapcar #'car vars-vals)
            (mapcar #'cdr vars-vals)
            (nreverse object-var-substitutions))))

;; This one was surprisingly hard to write...
(define-setf-expander their (specification &body objects &environment env)
  (let ((object-count (length objects)))
    (case object-count
      (0 (error "~S requires at least one object argument" '(setf their)))
      (1 (get-setf-expansion `(its ,specification ,(first objects)) env))
      (t (let ((object-vars (%make-object-vars object-count))
               (object-var (first (%make-object-vars 1)))
               (object-var-substitutions nil))
           (multiple-value-bind (template values-count) (expand-specification *kind-definitions* specification)
             (multiple-value-bind (vars vals stores writer reader)
                 (get-setf-expansion (funcall template object-var) env)
               (assert (= values-count (length stores)))
               (setf (values vars vals object-var-substitutions)
                     (%extract-object-var-substitutions object-var vars vals))
               ;; Using separate reader and writer variables would generate an "unused variable" warning.
               (let ((writer-reader-var (gensym (string '#:writer-reader)))
                     (writer-reader-val `(symbol-macrolet ,object-var-substitutions
                                           (cons (lambda (,object-var ,@stores)
                                                   ,writer)
                                                 (lambda (,object-var)
                                                   ,reader))))
                     (writer-var (gensym (string '#:writer)))
                     (reader-var (gensym (string '#:reader)))
                     (outer-stores-groups
                      (map-into (make-list object-count)
                                (lambda ()
                                  (map-into (make-list values-count)
                                            (lambda ()
                                              (gensym (string '#:outer-store)))))))
                     (values-operator (if (= values-count 1)
                                          '(values)
                                          '(multiple-value-call #'values))))
                 (values (nconc object-vars (append vars (list writer-reader-var)))
                         (append objects vals (list writer-reader-val))
                         (apply #'append outer-stores-groups)
                         `(let ((,writer-var (car ,writer-reader-var)))
                            (,@values-operator
                             ,@(mapcar (lambda (object-var outer-stores)
                                         `(funcall ,writer-var ,object-var ,@outer-stores))
                                       object-vars
                                       outer-stores-groups)))
                         `(let ((,reader-var (cdr ,writer-reader-var)))
                            (,@values-operator
                             ,@(mapcar (lambda (object-var)
                                         `(funcall ,reader-var ,object-var))
                                       object-vars))))))))))))
