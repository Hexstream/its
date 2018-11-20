(in-package #:its)

(defclass kind-definitions (defsys:standard-system)
  ())

(defvar *kind-definitions* (make-instance 'kind-definitions :name 'kind-definitions))

(setf (defsys:locate (defsys:root-system) 'its)
      *kind-definitions*)

(defclass kind-definition () ())

(defclass standard-kind-definition (kind-definition defsys:standard-system)
  ())

(defgeneric lambda-list (object))
(defgeneric expander (object))

(defclass access-specifier-definition (defsys:name-mixin)
  ((%lambda-list :initarg :lambda-list
                 :reader lambda-list)
   (%expander :initarg :expander
              :reader expander)))

(defun %ensure-kind (name)
  (or (defsys:locate *kind-definitions* name :errorp nil)
      (setf (defsys:locate *kind-definitions* name)
            (make-instance 'standard-kind-definition :name name))))

(defun %ensure (kind name lambda-list expander)
  (setf (defsys:locate kind name)
        (make-instance 'access-specifier-definition
                       :name name
                       :lambda-list lambda-list
                       :expander expander)))

(defgeneric expand-specification (expander specification))

(defmethod expand-specification ((root kind-definitions) (specification symbol))
  (let ((kind-name specification))
    (expand-specification (defsys:locate root kind-name) specification)))

(defmethod expand-specification ((root kind-definitions) (specification cons))
  (destructuring-bind (kind-name &rest args) specification
    (declare (ignore args))
    (expand-specification (defsys:locate root kind-name) specification)))

(defmacro multiple-value-values (&body body)
  `(multiple-value-call #'values ,@body))

(define-setf-expander multiple-value-values (&body places &environment env)
  (let (all-vars all-vals all-stores all-writers all-readers)
    (dolist (place places)
      (multiple-value-bind (vars vals stores writer reader) (get-setf-expansion place env)
        (push vars all-vars)
        (push vals all-vals)
        (push stores all-stores)
        (push writer all-writers)
        (push reader all-readers)))
    (values (apply #'append (nreverse all-vars))
            (apply #'append (nreverse all-vals))
            (apply #'append (nreverse all-stores))
            `(multiple-value-call #'values ,@(nreverse all-writers))
            `(multiple-value-call #'values ,@(nreverse all-readers)))))

(defun %generate-templates (function args)
  (let ((templates nil)
        (values-counts nil))
    (dolist (arg args)
      (multiple-value-call (lambda (template &optional (count 1))
                             (push template templates)
                             (push count values-counts))
        (funcall function arg)))
    (values (nreverse templates) values-counts)))

(defun %combine-templates (templates values-counts)
  (values (if (= (length templates) 1)
              (first templates)
              (let ((operator (if (apply #'= 1 values-counts)
                                  'values
                                  'multiple-value-values)))
                (lambda (object-var)
                  `(,operator ,@(mapcar (lambda (template)
                                          (funcall template object-var))
                                        templates)))))
          (apply #'+ values-counts)))

(defun %expand-args (expander args)
  (multiple-value-call #'%combine-templates
    (%generate-templates (lambda (spec)
                           (expand-specification
                            (defsys:locate expander (etypecase spec
                                                      (cons (first spec))
                                                      (symbol spec)))
                            spec))
                         args)))

(defmethod expand-specification ((kind standard-kind-definition) (specification symbol))
  (expand-specification kind '(t t)))

(defmethod expand-specification ((kind standard-kind-definition) (specification cons))
  (destructuring-bind (kind-name &rest access-specifiers) specification
    (declare (ignore kind-name))
    (%expand-args kind access-specifiers)))

(defmethod expand-specification ((access-specifier access-specifier-definition) (specification symbol))
  (expand-specification access-specifier (list specification)))

(defmethod expand-specification ((access-specifier access-specifier-definition) (specification cons))
  (destructuring-bind (access-specifier-name &rest args) specification
    (declare (ignore access-specifier-name))
    (apply (expander access-specifier) args)))

(defmethod defsys:expand-definition ((system kind-definitions) kind-name environment body &key)
  (declare (ignore environment))
  (let ((kind-var (gensym (string '#:kind))))
    (destructuring-bind ((&optional value-var) &body body) body
      (let ((body
             (mapcar (lambda (access-specifier)
                       (when (typep access-specifier '(cons (eql t) (cons list null)))
                         (setf access-specifier
                               (let ((access-specifiers (second access-specifier)))
                                 `(t () (:expander (expand-specification ,kind-var ',(cons t access-specifiers)))))))
                       (destructuring-bind (name lambda-list &body body) access-specifier
                         (multiple-value-bind (value-var body)
                             (if (typep body '(cons (cons (eql :expander) list)
                                               null))
                                 (values nil (rest (first body)))
                                 (values value-var body))
                           `(%ensure ,kind-var ',name ',lambda-list
                                     (lambda ,lambda-list
                                       ,@(if value-var
                                             (list `(lambda (,value-var) ,@body))
                                             body))))))
                     body)))
        `(let ((,kind-var (%ensure-kind ',kind-name)))
           ,@body
           ,kind-var)))))

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
