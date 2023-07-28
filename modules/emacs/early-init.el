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
       (if (or allow (yes-or-no-p (format "Allow make process call?\nName: %.512s\nCommand: %.512s\n" name command)))
         (apply orig args)
         (throw 'make-process-wrapper nil)))))

(sec-wrap-function 'make-serial-process
  '(lambda (orig &rest args)
     (let ((name (plist-get args :name))
            (port (plist-get args :port)))
       (if (yes-or-no-p (format "Allow make serial process call?\nName: %.512s\nPort: %.512s\n" name port))
         (apply orig args)
         (throw 'make-process-wrapper nil)))))

(sec-wrap-function 'make-network-process
  '(lambda (orig &rest args)
     (let ((name (plist-get args :name)))
       (if (yes-or-no-p (format "Allow make network process call?\nName: %.512s\n" name))
         (apply orig args)
         (throw 'make-process-wrapper nil)))))
;;; early-init.el ends here

;; Local Variables:
;; lisp-indent-offset: 2
;; End:
