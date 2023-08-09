;;; early-init.el --- Kurnevsky's Emacs configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Early init with some Emacs hardening.

;;; Code:

;; TODO:
;; call-process-region
;; call-process

(defun sec-wrap-function (symbol callback)
  "Wrap a function with SYMBOL name and call CALLBACK instead."
  (let* ((orig (symbol-function symbol))
          (orig-doc (documentation symbol t))
          (wrapper `(lambda (&rest args)
                      ,orig-doc
                      (apply ,callback (cons ,orig args))))
          (wrapper (byte-compile wrapper)))
    (fset symbol wrapper)))

(sec-wrap-function 'make-process
  '(lambda (orig &rest args)
     (let ((name (plist-get args :name))
            (command (mapconcat #'identity (plist-get args :command) " "))
            (allow))
       (mapbacktrace
         (lambda (_evald func _args _flags)
           (setq allow (or allow
                         (eq func 'ispell-start-process)
                         (eq func 'flycheck-start-command-checker)))))
       (if (or allow (yes-or-no-p (format "Name: %.512s\nCommand: %.512s\nAllow `make-process' call?" name command)))
         (apply orig args)
         (signal 'error nil)))))

(sec-wrap-function 'make-serial-process
  '(lambda (orig &rest args)
     (let ((name (plist-get args :name))
            (port (plist-get args :port)))
       (if (yes-or-no-p (format "Name: %.512s\nPort: %.512s\nAllow `make-serial-process' call?" name port))
         (apply orig args)
         (signal 'error nil)))))

(sec-wrap-function 'make-network-process
  '(lambda (orig &rest args)
     (let ((name (plist-get args :name))
            (allow))
       (mapbacktrace
         (lambda (_evald func _args _flags)
           (setq allow (or allow
                         (eq func 'server-start)
                         (eq func 'server-running-p)
                         (eq func 'server-eval-at)))))
       (if (or allow (yes-or-no-p (format "Name: %.512s\nAllow `make-network-process' call?" name)))
         (apply orig args)
         (signal 'error nil)))))
;;; early-init.el ends here

;; Local Variables:
;; lisp-indent-offset: 2
;; End:
