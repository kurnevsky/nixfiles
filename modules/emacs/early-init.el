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

(defvar sec-allow-make-process
  '(pcase name
     ("ispell"
       (pcase command
         (`(,(pred (string= (executable-find "aspell"))) "-a" "-m" . ,_) t)))
     ((or "flycheck-emacs-lisp" "flycheck-emacs-lisp-checkdoc")
       (pcase command
         (`(,(rx bos "/nix/store/" (* nonl) "/emacs" eos) "-Q" "--batch" . ,_) t)))
     ("flycheck-scala"
       (pcase command
         (`(,(pred (string= (executable-find "scalac"))) "-Ystop-after:parser" ,_) t)))
     ("flycheck-markdown-markdownlint-cli"
       (pcase command
         (`(,(pred (string= (executable-find "markdownlint"))) "--" ,_) t)))
     ("flycheck-sh-bash"
       (pcase command
         (`(,(pred (string= (executable-find "bash"))) "--norc" "-n" "--") t)))
     ("flycheck-sh-shellcheck"
       (pcase command
         (`(,(pred (string= (executable-find "shellcheck"))) "--format" "checkstyle" "--shell" "bash" "--external-sources" "-") t)))
     ("flycheck-rust-cargo"
       (pcase command
         (`(,(pred (string= (executable-find "cargo"))) "test" "--no-run" "--lib" "--message-format=json") t)))
     ("flycheck-nix"
       (pcase command
         (`(,(pred (string= (executable-find "nix-instantiate"))) "--parse" "-") t)))
     ("doom-modeline-env"
       (pcase command
         (`(,(pred (string= (executable-find "rustc"))) "--version") t)))
     ("rg"
       (pcase command
         (`(,(pred (string= shell-file-name)) "-c" ,(pred (string-prefix-p (executable-find "rg")))) t)))
     (" *mu4e-server*"
       (pcase command
         (`(,(pred (string= (executable-find "mu"))) "server") t)))
     ("mu4e-update"
       (pcase command
         (`(,(pred (string= shell-file-name)) "-c" "mbsync --all") t)))
     ("git"
       (pcase command
         (`("git" "--no-pager" "--literal-pathspecs" . ,_) t)))
     ("epg"
       (pcase command
         (`(,(pred (string= (executable-find "gpg2"))) "--no-tty" "--status-fd" "1" "--yes" "--enable-progress-filter" "--command-fd" "0" . ,_) t)))
     ((rx bos "*tramp/ssh ")
       (pcase command
         (`("/bin/sh" "-i") t)))
     ;; LSP
     ("metals"
       (pcase command
         (`(,(pred (string= (executable-find "env"))) "JAVA_TOOL_OPTIONS=-Dmetals.allow-multiline-string-formatting=off -Dmetals.icons=unicode" "metals") t)))
     ("rust-analyzer"
       (pcase command
         (`(,(pred (string= (executable-find "rust-analyzer")))) t)))
     ("nix-nil"
       (pcase command
         (`("nil") t)))))

(defvar sec-allow-make-network-process
  '(pcase name
     ((or "server-client-test" "server" "eval-at")
       (eq family 'local))
     ;; magit forge
     ("api.github.com"
       (and
         (string= "api.github.com" host)
         (eq service 443)))
     ("gitlab.com"
       (and
         (string= "gitlab.com" host)
         (eq service 443)))
     ("gitlab.evolution.com"
       (and
         (string= "gitlab.evolution.com" host)
         (eq service 443)))))

(sec-wrap-function 'make-process
  `(lambda (orig &rest args)
     (let* ((name (plist-get args :name))
             (command (plist-get args :command))
             (command-str (mapconcat #'identity command " ")))
       (if (or ,sec-allow-make-process (yes-or-no-p (format "Name: %.1024s
Command: %.1024s
Allow `make-process' call?" name command-str)))
         (apply orig args)
         (signal 'error nil)))))

(sec-wrap-function 'make-serial-process
  '(lambda (orig &rest args)
     (let ((name (plist-get args :name))
            (port (plist-get args :port)))
       (if (yes-or-no-p (format "Name: %.1024s
Port: %.1024s
Allow `make-serial-process' call?" name port))
         (apply orig args)
         (signal 'error nil)))))

(sec-wrap-function 'make-network-process
  `(lambda (orig &rest args)
     (let ((name (plist-get args :name))
            (host (plist-get args :host))
            (service (plist-get args :service))
            (type (plist-get args :type))
            (family (plist-get args :family))
            (local (plist-get args :local))
            (remote (plist-get args :remote)))
       (if (or ,sec-allow-make-network-process (yes-or-no-p (format "Name: %.1024s
Host: %.1024s
Service: %.1024s
Type: %.1024s
Family: %.1024s
Local: %.1024s
Remote: %.1024s
Allow `make-network-process' call?" name host service type family local remote)))
         (apply orig args)
         (signal 'error nil)))))

(fmakunbound 'sec-wrap-function)
(makunbound 'sec-allow-make-process)
(makunbound 'sec-allow-make-network-process)
;;; early-init.el ends here

;; Local Variables:
;; lisp-indent-offset: 2
;; End:
