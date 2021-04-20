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
