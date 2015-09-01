;;; defproject -- Manage dir-locals and project specifics
;;;
;;; Commentary:
;;;
;;; Code:

(defun defproject--ismode? (symbol)
  "Predicate for determining if a symbol is a mode"
  (or (s-contains? "-mode" (symbol-name symbol))
      (equal ":nil" (symbol-name symbol))))


(defun defproject--filter-plist (fn plist)
  "Apply FN, a lambda function with signatures (key value), to PLIST.
Remove elements that return nil."
  (let ((pl plist)
        (vals ()))
    (while pl
      (push (funcall fn (car pl) (cadr pl)) vals)
      (setq pl (cddr pl)))
    (delq nil (nreverse vals))))


(defun defproject-get-dir-locals (args)
  "Filter ARGS, returning cons cels of (PROPERTY . VALUE)."
  (defproject--filter-plist (lambda (key val)
		  (when (defproject--ismode? key)
		    (cons key val))) args))

(defun defproject-eval-dir-locals (args)
  "Map over each (MODE . VARS) in dir-locals like list ARGS.
Evaluate each cdr in VARS list unless the car is 'eval."
  ;; for each mode and its var/value list
  (-map (lambda (mode_args)
	  (let ((mode (car mode_args))
		(body (cdr mode_args)))
	    ;; for each var and its value
	    (cons (intern (s-chop-prefix ":" (symbol-name mode)))
		  (-map (lambda (var_val)
			  (let ((var (car var_val))
				(val (cdr var_val)))
			    ;; if first element is 'eval pass through
			    ;; otherwise evaluate the cdr
			    (if (eq var 'eval)
				var_val
			      (cons var (eval val))))) body)))) args))

(defmacro defproject(project-name &rest args)
  "Define a project of type PROJECT-NAME.  ARGS is a plist which
requires a :path symbol as well as mode symbols (e.g. :python-mode).
Also accepts :vars variable and :init."
  (declare (indent 1))
  (let* ((project-name-symbol (if (stringp project-name)
                                  (intern project-name)
                                project-name)))

    `(let* ((project-path ,(plist-get args :path))
	    ,@(plist-get args :vars)
	    (dir-locals (defproject-eval-dir-locals
			  (quote ,(defproject-get-dir-locals args)))))
       (when dir-locals
	 (-map (lambda(class-vars-list)
		 (-map (lambda(class-var)
			 (add-to-list 'safe-local-variable-values class-var))
		       (cdr class-vars-list))) dir-locals)

	 (dir-locals-set-class-variables (quote ,project-name)
					 dir-locals))
       (when (file-exists-p project-path)
	 (dir-locals-set-directory-class project-path (quote ,project-name))
	 ,@(plist-get args :init)))))

(provide 'defproject)
;;; defproject ends here
