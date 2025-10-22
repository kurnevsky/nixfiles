;;; early-init.el --- Kurnevsky's Emacs configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Early init with some Emacs hardening.

;;; Code:

;; Speed up the initialization reducing garbage collection runs.
(setq gc-cons-threshold (* 64 1024 1024))
;; Disable deferred compilation.
(setq native-comp-jit-compilation nil)

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
  `(lambda (orig &rest args)
     (let* ((inhibit-message t)
             (name (plist-get args :name))
             (command (plist-get args :command))
             (command-str (mapconcat #'identity command " ")))
       (message "`make-process' call:
Name: %s
Command: %s" name command-str)
       (apply orig args))))

(sec-wrap-function 'make-serial-process
  '(lambda (orig &rest args)
     (let ((inhibit-message t)
            (name (plist-get args :name))
            (port (plist-get args :port)))
       (message "`make-serial-process' call:
Name: %s
Port: %s" name port)
       (apply orig args))))

(sec-wrap-function 'make-network-process
  `(lambda (orig &rest args)
     (let ((inhibit-message t)
            (name (plist-get args :name))
            (host (plist-get args :host))
            (service (plist-get args :service))
            (type (plist-get args :type))
            (family (plist-get args :family))
            (local (plist-get args :local))
            (remote (plist-get args :remote)))
       (message "`make-network-process' call:
Name: %s
Host: %s
Service: %s
Type: %s
Family: %s
Local: %s
Remote: %s" name host service type family local remote)
       (apply orig args))))

(sec-wrap-function 'call-process
  `(lambda (orig &rest args)
     (let ((inhibit-message t)
            (program (car args)))
       (message "`call-process' call:
Program: %s" program)
       (apply orig args))))

(sec-wrap-function 'call-process-region
  `(lambda (orig &rest args)
     (let ((inhibit-message t)
            (program (car (cdr (cdr args)))))
       (message "`call-process-region' call:
Program: %s" program)
       (apply orig args))))

(unintern "sec-wrap-function" obarray)
(unintern "sec-allow-make-process" obarray)
(unintern "sec-allow-make-network-process" obarray)
(unintern "sec-allow-call-process" obarray)
;;; early-init.el ends here

;; Local Variables:
;; lisp-indent-offset: 2
;; End:
