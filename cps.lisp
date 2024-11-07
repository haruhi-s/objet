;; TODO
;; embed dynamic variables in continuation
;; labels, let*
;; eval&
;; defun
;; separate closure variables and *env*

(defvar *meta-lvl* 0)

(defvar *env* `((t . t)
                (get-meta-lvl . ,(lambda (k) (funcall (car k) *meta-lvl*)))
                ,@(mapcar
                   (lambda (x) (cons x (lambda (k &rest args) (funcall (car k) (apply x args)))))
                   '(+ - * / cons car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr eq list list* read not prin1 print write-string rplaca rplacd terpri concatenate remove-if-not))
                (funcall . ,(lambda (k f &rest args) (apply& f args k)))
                (call/cc . ,(lambda (k f) (apply& f (list (lambda (_ v) (funcall (car k) v)))
                                                  (list (lambda (v) v) (cadr k)))))
                (eval . ,(lambda (k form) (eval& form () k)))
                (eval-in-host . ,(lambda (k x) (funcall (car k) (eval x))))
                (repl . ,(lambda (k)
                           (let ((*meta-lvl* (1+ *meta-lvl*)))
                             (eval& '(loop (write-string "bobject> ")
                                      (if (eq (let ((v (prin1 (eval (read))))) (terpri) v) 'quit) 'loop))
                                    *env* k))))))

(defun map& (f list k)
  (if (consp list)
      (funcall f (car list)
               (list (lambda (head)
                       (map& f (cdr list)
                             (list (lambda (tail) (funcall (car k) (cons head tail)))
                                   (cadr k))))
                     (cadr k)))
      (funcall (car k) nil)))

(defun apply& (f args k)
  (if (consp f) ; (closure env args body)
      (let ((bound `(,@(mapcar #'cons (caddr f) args) ,@(cadr f))))
        (map& (lambda (x k) (eval& x bound k)) (cdddr f)
              (list (lambda (v) (funcall (car k) (car (last v))))
                    (cadr k))))
      (apply f k args)))

(defun eval& (form env k)
  (labels ((eval-with-env& (form k) (eval& form env k))
           (eval-list-with-env& (x k)
             (map& #'eval-with-env& x (list (lambda (v) (funcall (car k) (car (last v))))
                                            (cadr k))))
           (eval-quasi& (form k)
             (cond ((sb-int:comma-p form) (eval-with-env& (sb-int:comma-expr form) k))
                   ((consp form) (map& #'eval-quasi& form k))
                   (t (funcall (car k) form)))))
    (cond
      ((symbolp form) (funcall (car k) (cdr (assoc form (append env (cadr k) *env*)))))
      ((consp form)
       (cond
         ((and (symbolp (car form)) (assoc (car form) (append env (cadr k) *env*)))
          (eval-with-env& (car form) (list (lambda (v) (map& #'eval-with-env& (cdr form)
                                                        (list (lambda (w) (apply& v w k))
                                                              (cadr k))))
                                           (cadr k))))
         ((eq (car form) 'lambda) (funcall (car k) `(closure ,env ,@(cdr form))))
         ((eq (car form) 'closure) (funcall (car k) form))
         ((eq (car form) 'quote) (funcall (car k) (cadr form)))
         ((eq (car form) 'sb-int:quasiquote) (eval-quasi& (cadr form) k))
         ((eq (car form) 'define) (eval& (caddr form) env
                                         (list (lambda (v)
                                                 (setf *env* (cons (cons (cadr form) v) *env*))
                                                 (funcall (car k) nil))
                                               (cadr k))))
         ((eq (car form) 'set!) (eval& (caddr form) env
                                       (list (lambda (v)
                                               (rplacd (assoc (cadr form) (append env (cadr k) *env*)) v)
                                               (funcall (car k) v))
                                             (cadr k))))
         ((eq (car form) 'progn) (eval-list-with-env& (cdr form) k))
         ((eq (car form) 'let)
          (map&
           (lambda (x k) (eval-list-with-env& (cdr x) (list (lambda (v) (funcall (car k) (cons (car x) v)))
                                                          (cadr k))))
           (cadr form)
           (list (lambda (q) (eval-with-env& `((closure ,(remove-if-not
                                                          (lambda (x) (not (assoc (car x) (cadr k)))) q)
                                                () ,@(cddr form)))
                                             (list (car k) (append
                                                            (remove-if-not (lambda (x) (assoc (car x) (cadr k))) q)
                                                            (cadr k)))))
                 (cadr k))))
         ((eq (car form) 'loop)
          (labels ((l (k) (eval-list-with-env& (cdr form)
                            (list (lambda (v) (if (eq 'loop v) (funcall (car k) v) (l k)))
                                  (cadr k)))))
            (l k)))
         ((eq (car form) 'if) (eval-with-env&
                               (cadr form)
                               (list (lambda (b) (eval-with-env& (if b (caddr form) (cadddr form)) k))
                                     (cadr k))))
         (t (eval-with-env& (car form)
                            (list (lambda (f)
                                    (map& #'eval-with-env& (cdr form)
                                          (list (lambda (args)
                                                  (apply& f args k))
                                                (cadr k))))
                                  (cadr k))))))
      (t (funcall (car k) form)))))

(defun toy-eval (x)
  (let ((result nil))
    (eval& x () (list (lambda (v) (setq result v)) nil))
    result))

(defun repl ()
  (labels ((l ()
             (write-string "
object> ")  (eval& (read) nil (list (lambda (v) (if (eq v 'quit) 'quit (progn (prin1 v) (l))))
                                    nil))))
    (l)))

;; (loop
;;   (write-string "bobject> ")
;;   (if (eq (let ((v (prin1 (eval (read))))) (terpri) v) 'quit) 'loop))
