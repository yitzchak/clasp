(in-package "MP")

(defun cas-expander (symbol)
  (core:get-sysprop symbol 'cas-method))
(defun (setf cas-expander) (expander symbol)
  (core:put-sysprop symbol 'cas-method expander))

(defmacro cas (place old new &environment env)
  (multiple-value-bind (temps values oldvar newvar cas read)
      (get-cas-expansion place env)
    (declare (ignore read))
    `(let* (,@(mapcar #'list temps values)
            (,oldvar ,old) (,newvar ,new))
       ,cas)))

(defmacro atomic-update (place update-fn &rest arguments &environment env)
  (multiple-value-bind (vars vals old new cas read)
      (get-cas-expansion place env)
    `(let* (,@(mapcar #'list vars vals)
            (,old ,read))
       (loop for ,new = (funcall ,update-fn ,@arguments ,old)
             until (eq ,old (setf ,old ,cas))
             finally (return ,new)))))

(defmacro atomic-incf (place &optional (delta 1))
  `(atomic-update ,place #'+ ,delta))

(defmacro atomic-decf (place &optional (delta 1))
  `(atomic-update ,place #'(lambda (y x) (- x y)) ,delta))

(defun get-cas-expansion (place &optional env)
  (etypecase place
    (symbol
     (multiple-value-bind (expansion expanded)
         (macroexpand-1 place env)
       (if expanded
           (get-cas-expansion expansion env)
           (error "CAS on variables not supported yet")))
     #+(or)
     (let ((info (cleavir-env:variable-info env place)))
       (etypecase info
         (cleavir-env:symbol-macro-info
          (get-cas-expansion (macroexpand-1 place env) env))
         (cleavir-env:special-variable-info
          (get-cas-expansion `(symbol-value ',place) env))
         (cleavir-env:lexical-variable-info
          (lexical-cas-expansion place env)))))
    (cons
     (let* ((name (car place))
            (expander (cas-expander name)))
       (if expander
           (funcall expander place env)
           (multiple-value-bind (expansion expanded)
               (macroexpand-1 place env)
             (if expanded
                 (get-cas-expansion expansion env)
                 (default-cas-expansion place env))))))))

#+(or)
(defun lexical-cas-expansion (var &optional env)
  ;; So: For a regular local, cas is meaningless.
  ;; We can reasonably say it succeeds, i.e.
  ;; (cas x old new) = (prog1 old (setq x new))
  ;; For a closed over variable, we could do an
  ;; actual CAS. Closures are just objects, so
  ;; I think this is even reasonable. But to
  ;; support it we kind of need a special form
  ;; so that the compiler can determine the
  ;; closed-over-ness of the variable.
  ;; ...but none of this is supported right now.
  (let ((old (gensym "OLD")) (new (gensym "NEW")))
    (values nil nil old new
            `(casq ,var ,old ,new)
            var)))

(defun default-cas-expansion (place &optional env)
  (declare (ignore env))
  (error "~a is not a supported place to CAS" place)
  #+(or)
  (let* ((op (car place)) (args (cdr place))
         (temps (loop for form in args collect (gensym)))
         (new (gensym "NEW")) (old (gensym "OLD")))
    (values temps args old new
            `(funcall #'(cas ,op) ,@temps)
            `(,op ,@temps))))

(defmacro define-cas-expander (name lambda-list &body body
                               &environment env)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (cas-expander ',name)
           ,(ext:parse-macro name lambda-list body env))
     ',name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Particular CAS expansions
;;;

(define-cas-expander the (type place &environment env)
  (multiple-value-bind (vars vals old new cas read)
      (get-cas-expansion place env)
    (values vars vals old new
            `(let ((,old (the ,type ,old))
                   (,new (the ,type ,new)))
               ,cas)
            `(the ,type ,read))))

(define-cas-expander car (cons)
  (let ((old (gensym "OLD")) (new (gensym "NEW"))
        (ctemp (gensym "CONS")) (ctemp2 (gensym "CHECKED-CONS")))
    (values (list ctemp ctemp2)
            ;; We type check ahead of time so we don't have to do it on every
            ;; iteration in ATOMIC-UPDATE and suchlike.
            (list cons `(if (consp ,ctemp)
                            ,ctemp
                            (error 'type-error :datum ,ctemp
                                   :expected-type 'cons)))
            old new
            `(core::cas-car ,ctemp2 ,old ,new)
            `(car ,ctemp))))

(define-cas-expander cdr (cons)
  (let ((old (gensym "OLD")) (new (gensym "NEW"))
        (ctemp (gensym "CONS")) (ctemp2 (gensym "CHECKED-CONS")))
    (values (list ctemp ctemp2)
            (list cons `(if (consp ,ctemp)
                            ,ctemp
                            (error 'type-error :datum ,ctemp
                                               :expected-type 'cons)))
            old new
            `(core::cas-cdr ,ctemp2 ,old ,new)
            `(car ,ctemp))))
