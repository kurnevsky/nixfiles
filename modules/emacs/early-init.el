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
            (backtrace)
            (allow))
       (mapbacktrace
         (lambda (_evald func _args _flags)
           (when (symbolp func)
             (push func backtrace))
           (setq allow (or allow
                         (eq func 'ispell-start-process)
                         (eq func 'flycheck-start-command-checker)
                         (eq func 'rg-run)
                         (eq func 'mu4e--server-start)
                         (eq func 'mu4e--update-mail-and-index-real)
                         (eq func 'lsp--start-workspace)
                         (eq func 'magit-parse-git-async)
                         (eq func 'epg--start)))))
       (setq backtrace (butlast backtrace))
       (if (or allow (yes-or-no-p (format "Name: %.1024s\nCommand: %.1024s\nBacktrace: %.1024S\nAllow `make-process' call?" name command backtrace)))
         (apply orig args)
         (signal 'error nil)))))

(sec-wrap-function 'make-serial-process
  '(lambda (orig &rest args)
     (let ((name (plist-get args :name))
            (port (plist-get args :port))
            (backtrace))
       (mapbacktrace
         (lambda (_evald func _args _flags)
           (when (symbolp func)
             (push func backtrace))))
       (setq backtrace (butlast backtrace))
       (if (yes-or-no-p (format "Name: %.1024s\nPort: %.1024s\nBacktrace: %.1024S\nAllow `make-serial-process' call?" name port backtrace))
         (apply orig args)
         (signal 'error nil)))))

(sec-wrap-function 'make-network-process
  '(lambda (orig &rest args)
     (let ((name (plist-get args :name))
            (backtrace)
            (allow))
       (mapbacktrace
         (lambda (_evald func _args _flags)
           (when (symbolp func)
             (push func backtrace))
           (setq allow (or allow
                         (eq func 'server-start)
                         (eq func 'server-running-p)
                         (eq func 'server-eval-at)
                         (eq func 'forge-pull)))))
       (setq backtrace (butlast backtrace))
       (if (or allow (yes-or-no-p (format "Name: %.1024s\nBacktrace: %.1024S\nAllow `make-network-process' call?" name backtrace)))
         (apply orig args)
         (signal 'error nil)))))
;;; early-init.el ends here

;; Local Variables:
;; lisp-indent-offset: 2
;; End:
