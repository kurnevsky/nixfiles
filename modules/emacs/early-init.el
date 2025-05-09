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
     ("flycheck-sh-zsh"
       (pcase command
         (`(,(pred (string= (executable-find "zsh"))) "--no-exec" "--no-globalrcs" "--no-rcs" ,_) t)))
     ("flycheck-sh-shellcheck"
       (pcase command
         (`(,(pred (string= (executable-find "shellcheck"))) "--format" "checkstyle" "--shell" "bash" "--external-sources" "-") t)))
     ("flycheck-rust-cargo"
       (pcase command
         (`(,(pred (string= (executable-find "cargo"))) "test" "--no-run" "--lib" "--message-format=json") t)))
     ("flycheck-nix"
       (pcase command
         (`(,(pred (string= (executable-find "nix-instantiate"))) "--parse" "-") t)))
     ("flycheck-c/c++-gcc"
       (pcase command
         (`(,(pred (string= (executable-find "gcc"))) "-fshow-column" "-iquote" ,_ "-Wall" "-Wextra" "-x" "c" "-S" "-o" "/dev/null" "-") t)))
     ("flycheck-c/c++-clang"
       (pcase command
         (`(,(pred (string= (executable-find "clang"))) "-fsyntax-only" "-fno-color-diagnostics" "-fno-caret-diagnostics" "-fno-diagnostics-show-option" "-iquote" ,_ "-Wall" "-Wextra" "-x" ,_ "-") t)))
     ("flycheck-haskell-ghc"
       (pcase command
         (`(,(pred (string= (executable-find "ghc"))) "-Wall" "-no-link" . ,_) t)))
     ("flycheck-xml-xmlstarlet"
       (pcase command
         (`(,(pred (string= (executable-find "xmlstarlet"))) "val" "--err" "--quiet" "-") t)))
     ("flycheck-json-jq"
       (pcase command
         (`(,(pred (string= (executable-find "jq"))) "." ,_ "/dev/null") t)))
     ("flycheck-json-python-json"
       (pcase command
         (`(,(pred (string= (executable-find "python3"))) "-m" "json.tool" ,_ "/dev/null") t)))
     ("flycheck-haskell-hlint"
       (pcase command
         (`(,(pred (string= (executable-find "hlint"))) "--no-exit-code" ,_) t)))
     ("flycheck-groovy"
       (pcase command
         (`(,(pred (string= (executable-find "groovy"))) "-e" . ,_) t)))
     ("flycheck-python-pycompile"
       (pcase command
         (`(,(pred (string= (executable-find "python3"))) "-m" "py_compile" ,_) t)))
     ("flycheck-tex-chktex"
       (pcase command
         (`(,(pred (string= (executable-find "chktex"))) "--verbosity=0" "--quiet" "--inputfiles") t)))
     ("ess-r-flymake"
       (pcase command
         (`("R" "--no-save" "--no-restore" "--no-site-file" "--no-init-file" "--slave" . ,_) t)))
     ("doom-modeline-env"
       (pcase command
         (`(,(pred (string= (executable-find "rustc"))) "--version") t)
         (`(,(pred (string= (executable-find "bash"))) "-c" ,_) t)
         (`(,(pred (string= (executable-find "python3"))) "--version") t)))
     ("rg"
       (pcase command
         (`("rg" . ,_) t)
         (`(,(pred (string= shell-file-name)) "-c" ,(pred (string-prefix-p (executable-find "rg")))) t)
         ;; Tramp
         (`("/bin/sh" "-i") t)))
     (" *mu4e-server*"
       (pcase command
         (`(,(pred (string= (executable-find "mu"))) "server") t)))
     (" *mu4e-update*"
       (pcase command
         (`(,(pred (string= shell-file-name)) "-c" "mbsync --all") t)))
     ("git"
       (pcase command
         (`("git" "--no-pager" "--literal-pathspecs" . ,_) t)
         ;; Tramp
         (`("/bin/sh" "-i") t)))
     ("epg"
       (pcase command
         (`(,(pred (string= (executable-find "gpg2"))) "--no-tty" "--status-fd" "1" "--yes" "--enable-progress-filter" "--command-fd" "0" . ,_) t)))
     ((rx bos "*tramp/")
       (pcase command
         (`("/bin/sh" "-i") t)))
     ("Agda2"
       (pcase command
         (`("agda" "--interaction") t)))
     ("LanguageTool"
       (pcase command
         (`("languagetool-commandline" . ,_) t)))
     ("epdfinfo"
       (pcase command
         (`(,(rx bos "/nix/store/" (* nonl) "/epdfinfo" eos)) t)))
     (" *consult-async-stderr*"
       (pcase command
         ;; Tramp
         (`("cat" ,(rx bos "/tmp/tramp." (+ alnum) eos)) t)
         (`("/bin/sh" "-i") t)))
     ("khalel-vdirsyncer-process"
       (pcase command
         (`("vdirsyncer" "sync") t)))
     ("vterm"
       (pcase command
         (`("/bin/sh" . ,_) t)))
     ("gptel-curl"
       (pcase command
         (`("curl" . ,_) t)))
     ;; Treemacs
     ("Process Future"
       (pcase command
         (`(,(pred (string= (executable-find "python3"))) "-O" . ,_) t)))
     ;; LSP
     ("metals"
       (pcase command
         (`("env" "JAVA_TOOL_OPTIONS=-Dmetals.allow-multiline-string-formatting=off -Dmetals.icons=unicode" "metals") t)
         (`("emacs-lsp-booster" ,(pred (string= (executable-find "env"))) "JAVA_TOOL_OPTIONS=-Dmetals.allow-multiline-string-formatting=off -Dmetals.icons=unicode" "metals") t)))
     ("rust-analyzer"
       (pcase command
         (`(,(pred (string= (executable-find "rust-analyzer")))) t)
         (`("emacs-lsp-booster" ,(pred (string= (executable-find "rust-analyzer")))) t)))
     ("nix-nil"
       (pcase command
         (`("nil") t)
         (`("emacs-lsp-booster" ,(pred (string= (executable-find "nil")))) t)))
     ("lsp-r"
       (pcase command
         (`("R" "--slave" "-e" "languageserver::run()") t)
         (`("emacs-lsp-booster" ,(pred (string= (executable-find "R"))) "--slave" "-e" "languageserver::run()") t)))
     ("bash-ls"
       (pcase command
         (`(,(pred (string= (executable-find "bash-language-server"))) "start") t)
         (`("emacs-lsp-booster" ,(pred (string= (executable-find "bash-language-server"))) "start") t)))
     ("lsp-haskell"
       (pcase command
         (`("haskell-language-server-wrapper" "--lsp" "-l" ,_) t)
         (`("emacs-lsp-booster" ,(pred (string= (executable-find "haskell-language-server-wrapper"))) "--lsp" "-l" ,_) t)))
     ("pursls"
       (pcase command
         (`("purescript-language-server" "--stdio") t)
         (`("emacs-lsp-booster" ,(pred (string= (executable-find "purescript-language-server"))) "--stdio") t)))
     ("famulus"
       (pcase command
         (`(,(pred (string= (executable-find "famulus")))) t)
         (`("emacs-lsp-booster" ,(pred (string= (executable-find "famulus")))) t)))))

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
         (eq service 443)))
     ("llama"
       (string= "localhost" host))))

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

(defvar sec-allow-call-process
  '(pcase program
     ((rx bos "/nix/store/" (* nonl) "/emacsclient") (member 'with-editor-emacsclient-version backtrace))
     ("git" t)
     ("tar" t)
     ("diff" t)
     ("openssl" t)
     ("khal" t)
     ("jq" t)
     ("sh" t)
     ("gcc" t)
     ("g++" t)
     ("locale" t)
     ("ssh" (member 'tramp-call-process backtrace))
     ("chown" t)
     ("bzr" t)
     ("env" t)
     ("delta" t)
     ("/bin/sh" t)
     ("tty" (member 'epa-file-handler backtrace))
     ((pred (string= "/run/current-system/sw/bin/zsh")) t)
     ((pred (string= (executable-find "cargo"))) (member 'flycheck-call-checker-process backtrace))
     ((pred (string= (executable-find "gpg2"))) (member 'epa-file-handler backtrace))
     ((pred (string= (executable-find "aspell"))) t)
     ((pred (string= (executable-find "direnv"))) t)))

(sec-wrap-function 'call-process
  `(lambda (orig &rest args)
     (let ((program (car args))
            (backtrace))
       (mapbacktrace (lambda (_evald fun _args _flags) (setq backtrace (cons fun backtrace))))
       (if (or ,sec-allow-call-process (yes-or-no-p (format "Program: %.1024s
Allow `call-process' call?" program)))
         (apply orig args)
         (signal 'error nil)))))

(sec-wrap-function 'call-process-region
  `(lambda (orig &rest args)
     (let ((program (car (cdr (cdr args))))
            (backtrace))
       (mapbacktrace (lambda (_evald fun _args _flags) (setq backtrace (cons fun backtrace))))
       (if (or ,sec-allow-call-process (yes-or-no-p (format "Program: %.1024s
Allow `call-process-region' call?" program)))
         (apply orig args)
         (signal 'error nil)))))

(unintern "sec-wrap-function" obarray)
(unintern "sec-allow-make-process" obarray)
(unintern "sec-allow-make-network-process" obarray)
(unintern "sec-allow-call-process" obarray)
;;; early-init.el ends here

;; Local Variables:
;; lisp-indent-offset: 2
;; End:
