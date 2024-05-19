;;; init.el --- Kurnevsky's Emacs configuration -*- lexical-binding: t -*-

;;; Commentary:

;; This is my personal Emacs configuration.

;;; Code:

;; ========== Configure Emacs ==========

;; Speed up the initialization reducing garbage collection runs.
(defun display-startup-time ()
  "Set GC threshold and display Emacs startup time."
  (setq gc-cons-threshold (* 4 1024 1024))
  (message "Emacs loaded in %s with %d garbage collections."
    (format "%.2f seconds"
      (float-time
        (time-subtract after-init-time before-init-time)))
    gcs-done))
(add-hook 'emacs-startup-hook #'display-startup-time)
;; Speed up the initialization temporary disabling file-name-handler-alist.
(defvar file-name-handler-alist-copy file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook (lambda () (setq file-name-handler-alist file-name-handler-alist-copy)))
;; Collect the garbage when not used.
(add-function :after after-focus-change-function
  (lambda ()
    (unless (frame-focus-state)
      (garbage-collect))))
;; Don't print startup message.
(advice-add #'display-startup-echo-area-message :override #'ignore)
;; Remove gap in maximized window mode.
(setq frame-resize-pixelwise t)
;; Disable tool bar.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
;; Set font.
(set-face-attribute 'default nil :font "Iosevka Normal:size=15")
(add-to-list 'default-frame-alist '(font . "Iosevka Normal:size=15"))
;; Disable deferred compilation.
(setq native-comp-jit-compilation nil)
;; It causes compilation call at startup.
(setq comp--delayed-sources nil)
;; Don't do backup files.
(setq make-backup-files nil)
;; Don't create lock files.
(setq create-lockfiles nil)
;; Don't save discarded files.
(setq auto-save-default nil)
;; Inhibit startup/splash screen.
(setq inhibit-splash-screen t)
;; Cursor as line.
(setq-default cursor-type 'bar)
;; Don't use tabs for tabbing.
(setq-default indent-tabs-mode nil)
;; Disable dialog boxes.
(setq use-dialog-box nil)
;; Show clock.
(display-time-mode t)
;; Minimum shown number of lines before and after cursor.
(setq scroll-margin 2)
;; Reduce tab width.
(setq-default tab-width 4)
;; Don't jump when scrolling.
(setq scroll-conservatively 10000)
;; Don't show scratch message.
(setq initial-scratch-message nil)
;; File size in percents.
(size-indication-mode t)
;; Short messages.
(setq use-short-answers t)
;; Highlight trailing whitespaces.
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
;; Easy transition between buffers: M-arrow-keys.
(windmove-default-keybindings 'meta)
;; Don't show cursor in inactive buffers.
(setq-default cursor-in-non-selected-windows nil)
;; Enable scroll while searching.
(setq isearch-allow-scroll t)
;; Don't exit search mode on navigation.
(setq search-exit-option nil)
;; Move mouse to newly selected frames.
(setq focus-follows-mouse t)
;; Resize windows pixelwise.
(setq window-resize-pixelwise t)
;; Allow minibuffer commands while in the minibuffer.
(setq enable-recursive-minibuffers t)
;; Increase undo history limits.
(setq undo-limit (* 1024 1024))
(setq undo-strong-limit (* 2 1024 1024))
(setq undo-outer-limit (* 16 1024 1024))
;; Increase the amount of data which Emacs reads from the process.
(setq read-process-output-max (* 1024 1024))
;; Don’t consider case significant in completion.
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
;; Don't allow cursor in the read only minibuffer text.
(setq minibuffer-prompt-properties
  '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
;; Hide commands in M-x which do not work in the current mode.
(setq read-extended-command-predicate #'command-completion-default-include-p)
;; Show line and column numbers.
(line-number-mode t)
(column-number-mode t)
;; Enable the downcase-region and upcase-region commands.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
;; Don't align existing lines on RET.
(setq-default electric-indent-inhibit t)
;; File to write custom-set-variables.
(setq custom-file (concat user-emacs-directory "custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)
;; Ask before killing new buffer.
(defvar new-untitled nil)
(put 'new-untitled 'permanent-local t)
(defun kill-buffer-ask-first (orig-fun &rest args)
  "Prompt before killing buffer if it isn't associated with a file.
ORIG-FUN is original `kill-buffer' function.
ARGS is `kill-buffer' arguments."
  (let ((buffer (get-buffer (if (and args (car args)) (car args) (buffer-name)))))
    (if (and (buffer-local-value 'new-untitled buffer)
          (buffer-modified-p buffer)
          (not (buffer-file-name buffer)))
      (when (yes-or-no-p (format "Buffer '%s' modified and not associated with a file, kill it anyway?" (buffer-name buffer)))
        (apply orig-fun args))
      (apply orig-fun args))))
(advice-add #'kill-buffer :around #'kill-buffer-ask-first)
;; Add possibility to use C-m as hotkey in graphic mode.
(when (display-graphic-p)
  (define-key input-decode-map [?\C-m] [C-m]))
(when (daemonp)
  (add-hook 'after-make-frame-functions
    (lambda (frame)
      (when (display-graphic-p frame)
        (with-selected-frame frame
          (define-key input-decode-map [?\C-m] [C-m]))))))
;; Unbind keys
(dolist (key '("C-a" "C-b" "C-d" "C-e" "C-f" "C-j" "C-k" "C-n" "C-o" "C-p" "C-r"
                "C-s" "C-t" "C-w" "C-y" "C-z" "M-w"))
  (global-unset-key (kbd key)))
;; Disable bell.
(defun flash-mode-line ()
  "Flash the modeline on error or warning instead of the bell."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))
(setq ring-bell-function #'flash-mode-line)
;; Enable mouse support in terminal.
(unless (display-graphic-p)
  (xterm-mouse-mode t))

;; ========== Configure packages ==========

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-verbose t)
  (setq use-package-expand-minimally byte-compile-current-file))

(use-package bind-key
  :demand t)

(use-package dash
  :demand t)

(use-package f
  :demand t)

(use-package el-patch
  :demand t
  :config
  (eval-when-compile
    (setq el-patch-validate-during-compile t)))

(use-package hexrgb
  :ensure nil
  :demand t
  :config
  (defun set-base16-terminal-colors ()
    (when (eq (tty-display-color-cells) 256)
      (--each-indexed
        '(("black" "#3f4451")
           ("red" "#e05561")
           ("green" "#8cc265")
           ("yellow" "#d18f52")
           ("blue" "#4aa5f0")
           ("magenta" "#c162de")
           ("cyan" "#42b3c2")
           ("white" "#e6e6e6")
           ("brightblack" "#4f5666")
           ("brightred" "#ff616e")
           ("brightgreen" "#a5e075")
           ("brightyellow" "#f0a45d")
           ("brightblue" "#4dc4ff")
           ("brightmagenta" "#de73ff")
           ("brightcyan" "#4cd1e0")
           ("brightwhite" "#ffffff"))
        (-let* (((name color) it)
                 ((r g b) (--map (* it 256) (hexrgb-hex-to-color-values color))))
          (tty-modify-color-alist (list name it-index r g b))))))
  (add-hook 'tty-setup-hook #'set-base16-terminal-colors))

(use-package color
  :demand t
  :ensure nil)

(use-package base16-theme
  :demand t
  :custom
  (base16-highlight-mode-line 'contrast)
  (base16-distinct-fringe-background nil)
  (base16-theme-256-color-source 'colors)
  :config
  (defun color-blend (c1 c2 a)
    "Combine A C1 with (1-a) C2."
    (hexrgb-color-values-to-hex
      (cl-mapcar
        (lambda (c1 c2) (+ (* a c1) (* (- 1 a) c2)))
        (hexrgb-hex-to-color-values c1)
        (hexrgb-hex-to-color-values c2))
      2))
  (defun color-saturate-darken (c s d)
    "Saturate by S and darken by D a color C."
    (->> c
      hexrgb-hex-to-color-values
      (-map (-rpartial '/ 255.0))
      (funcall (-applify 'color-rgb-to-hsl))
      (funcall (-applify (-cut color-saturate-hsl <> <> <> s)))
      (funcall (-applify (-cut color-lighten-hsl <> <> <> (- d))))
      (funcall (-applify 'color-hsl-to-rgb))
      (funcall (-applify 'color-rgb-to-hex))))
  (defun modify-theme (theme)
    (let* ((custom--inhibit-theme-enable nil)
            (colors (symbol-value (intern (concat (symbol-name theme) "-theme-colors"))))
            (base00 (plist-get colors :base00))
            (base01 (plist-get colors :base01))
            (base08 (plist-get colors :base08))
            (base0A (plist-get colors :base0A))
            (base0B (plist-get colors :base0B))
            (base005 (color-blend base00 base01 0.5))
            (base08-highlight (color-saturate-darken base08 20 10))
            (base0A-highlight (color-saturate-darken base0A 20 10))
            (base0B-highlight (color-saturate-darken base0B 20 10)))
      (base16-theme-set-faces theme (symbol-value (intern (concat (symbol-name theme) "-theme-colors")))
        `( ;; Make it slightly different from highlighting
           (hl-line :background ,base005)
           ;; Minibuffer completion
           (completions-common-part :foreground base0C)
           ;; Ediff
           (ediff-current-diff-A :foreground base08 :inverse-video t)
           (ediff-current-diff-B :foreground base0B :inverse-video t)
           (ediff-current-diff-C :foreground base0A :inverse-video t)
           (ediff-even-diff-A :inverse-video t)
           (ediff-even-diff-B :inverse-video t)
           (ediff-even-diff-C :inverse-video t)
           (ediff-fine-diff-A :foreground ,base08-highlight :inverse-video t)
           (ediff-fine-diff-B :foreground ,base0B-highlight :inverse-video t)
           (ediff-fine-diff-C :foreground ,base0A-highlight :inverse-video t)
           (ediff-odd-diff-A :foreground base04 :inverse-video t)
           (ediff-odd-diff-B :foreground base04 :inverse-video t)
           (ediff-odd-diff-C :foreground base04 :inverse-video t)
           ;; Magit
           (magit-diff-base :foreground base0A :inverse-video t)
           (magit-diff-added :foreground base0B :inverse-video t)
           (magit-diff-removed :foreground base08 :inverse-video t)
           (magit-diff-base-highlight :foreground ,base0A-highlight :inverse-video t)
           (magit-diff-added-highlight :foreground ,base0B-highlight :inverse-video t)
           (magit-diff-removed-highlight :foreground ,base08-highlight :inverse-video t)
           ;; lsp-ui
           (lsp-ui-peek-peek :background ,base005)
           (lsp-ui-peek-list :background ,base005)
           (lsp-ui-peek-filename :foreground base09)
           (lsp-ui-peek-line-number :foreground base03)
           (lsp-ui-peek-highlight :box (:line-width -1 :color base08))
           (lsp-ui-peek-header :background base07 :foreground base00)
           (lsp-ui-peek-selection :background base07 :foreground base00)
           ;; Smerge
           (smerge-base :foreground base0A :inverse-video t)
           (smerge-upper :foreground base08 :inverse-video t)
           (smerge-lower :foreground base0B :inverse-video t)
           (smerge-refined-added :foreground ,base0B-highlight :inverse-video t)
           (smerge-refined-removed :foreground ,base08-highlight :inverse-video t)
           ;; Highlight foreground instead of background
           (show-paren-match :foreground base0D :background unspecified :weight extra-bold)
           (show-paren-mismatch :foreground base09 :background unspecified :weight extra-bold)
           ;; Make comments italic
           (font-lock-comment-face :foreground base03 :slant italic)
           ;; Apply string foreground for docstring and make it italic
           (font-lock-doc-face :foreground base0B :slant italic)))))
  (defun set-base16-theme (theme)
    (load-theme theme t)
    (modify-theme theme))
  (defvar base16-theme-chosen-themes '(base16-onedark base16-one-light))
  (if (or (daemonp) (display-graphic-p))
    (set-base16-theme (car base16-theme-chosen-themes))
    (add-hook 'tty-setup-hook (lambda () (set-base16-theme (car base16-theme-chosen-themes))) t))
  (defun toggle-base16-theme ()
    (interactive)
    (set-base16-theme (if (eq (car base16-theme-chosen-themes) (car custom-enabled-themes))
                        (cadr base16-theme-chosen-themes)
                        (car base16-theme-chosen-themes)))))

(use-package ligature
  :demand t
  :config
  (dolist (mode '(scala-mode scala-ts-mode))
    (ligature-set-ligatures mode '("<-" ;; for comprehension
                                    "=>" ;; pattern matching
                                    "???" ;; not implemented
                                    "##" ;; hashCode
                                    "=:=" ;; type equality
                                    "->" ;; tuple
                                    "==" "!=" "||" "&&" ">=" "<=" ;; boolean operators
                                    "<<" ">>" ;; binary operators
                                    "+=" "-=" "*=" "/=" "%=" "&=" "|=" "^=" "<<=" ">>=" "++=" "--=" "::=" ":::=" "+++=" ;; assign operators
                                    "++" "--" "::" ":::" "+++" ;; collections
                                    "//" "/*" "*/" "/**" ;; comments
                                    "<->" ;; cats IsEq
                                    "===" "=!=" ;; cats Eq
                                    "<*>" "*>" "<*" ;; cats Applicative
                                    ">>" ">>=" ;; cats FlatMap
                                    "***" ;; cats Arrow
                                    ":=" ;; scalatags attr
                                    )))
  (dolist (mode '(rust-mode rust-ts-mode))
    (ligature-set-ligatures mode '("->" ;; function
                                    "=>" ;; pattern matching
                                    "::" ;; path
                                    "==" "!=" "||" "&&" ">=" "<=" ;; boolean operators
                                    "+=" "-=" "*=" "/=" "%=" "&=" "|=" "^=" "<<=" ">>=" ;; assign operators
                                    ".." "..=" ;; range operators
                                    )))
  (global-ligature-mode t))

(use-package cl-macs
  :ensure nil
  :demand t
  :config
  ;; Fix hotkeys for Russian keyboard layout.
  (cl-loop
    for from across "йцукенгшщзхъфывапролджэячсмитьбю"
    for to   across "qwertyuiop[]asdfghjkl;'zxcvbnm,."
    do
    (eval `(define-key key-translation-map (kbd ,(concat "C-" (string from))) (kbd ,(concat "C-" (string to)))))
    (eval `(define-key key-translation-map (kbd ,(concat "M-" (string from))) (kbd ,(concat "M-" (string to)))))
    (eval `(define-key key-translation-map (kbd ,(concat "C-" (string (upcase from)))) (kbd ,(concat "C-S-" (string to)))))
    (eval `(define-key key-translation-map (kbd ,(concat "M-" (string (upcase from)))) (kbd ,(concat "M-S-" (string to)))))))

(use-package cua-base
  :ensure nil
  :demand t
  :bind (:map cua-global-keymap
          ("<C-return>"))
  :config
  (cua-mode t)
  (defun cua-macro-fix (orig-fun &rest args)
    (apply orig-fun args)
    (kmacro-edit-macro)
    (let ((endl "[[:space:]]*\\(;.*\\)?\n")
           (case-fold-search nil))
      ;; fix the C-c C-c
      (goto-char (point-min))
      (forward-line 7)
      (while (search-forward-regexp (concat "C-c C-c" endl "<timeout>") nil t)
        (replace-match "C-c <timeout>"))
      ;; fix the C-x C-x
      (goto-char (point-min))
      (forward-line 7)
      (while (search-forward-regexp (concat "C-x C-x" endl "<timeout>") nil t)
        (replace-match "C-x <timeout>"))
      ;; fix C-m is being confused with <return>
      (goto-char (point-min))
      (forward-line 7)
      (while (search-forward-regexp (concat "RET" endl) nil t)
        (replace-match "<return>")))
    (edmacro-finish-edit))
  (advice-add #'kmacro-end-macro :around #'cua-macro-fix))

(use-package time
  :ensure nil
  :demand t
  :custom
  (display-time-24hr-format t "24 hours time format.")
  :config
  (display-time-update))

(use-package comint
  :ensure nil
  :custom
  (comint-prompt-read-only t "Make the prompt read only."))

(use-package mb-depth
  :ensure nil
  :demand t
  :config
  (minibuffer-depth-indicate-mode t))

(use-package display-line-numbers
  :ensure nil
  :demand t
  :config
  (global-display-line-numbers-mode 1))

(use-package so-long
  :ensure nil
  :demand t
  :config
  (global-so-long-mode))

(use-package which-key
  :demand t
  :config
  (which-key-mode))

(use-package paren
  :ensure nil
  :demand t
  :custom
  (show-paren-style 'parenthesis)
  :config
  (show-paren-mode t))

(use-package elec-pair
  :ensure nil
  :demand t
  :config
  (electric-pair-mode 1))

(use-package puni
  :bind (("<delete>" . puni-forward-delete-char)
          ("C-M-<up>" . puni-beginning-of-sexp)
          ("C-M-<down>" . puni-end-of-sexp)))

(use-package hl-line
  :ensure nil
  :demand t
  :config
  (global-hl-line-mode 1))

(use-package pixel-scroll
  :ensure nil
  ;; overrides vertico
  :bind (:map pixel-scroll-precision-mode-map
          ("<prior>")
          ("<next>"))
  :demand t
  :config
  (pixel-scroll-precision-mode))

(use-package highlight-thing
  :demand t
  :custom
  (highlight-thing-what-thing 'symbol)
  :config
  (defun highlight-thing-should-highlight-p ()
    (and
      (not (minibufferp))
      (not (member major-mode highlight-thing-excluded-major-modes))
      (not (and
             (bound-and-true-p lsp-mode)
             (fboundp 'lsp--capability)
             (lsp--capability "documentHighlightProvider")))))
  (global-highlight-thing-mode))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'stack)
  :config
  ;; Display property might be deleted if a major mode defines
  ;; font-lock-extra-managed-props via font-lock-defaults.
  (add-hook 'after-change-major-mode-hook (lambda ()
                                            (when (derived-mode-p 'prog-mode)
                                              (add-to-list 'font-lock-extra-managed-props 'display)))))

(use-package vterm
  :bind (:map vterm-mode-map
          ("<f2>"))
  :custom
  (vterm-always-compile-module t)
  :config
  (add-hook 'vterm-mode-hook (lambda ()
                               (setq-local global-hl-line-mode nil)
                               (setq-local cua-mode nil)
                               (undo-tree-mode -1))))

(use-package multi-vterm)

(use-package flyspell
  :ensure nil
  :hook ((text-mode . flyspell-mode)
          (prog-mode . flyspell-prog-mode))
  :bind (:map flyspell-mode-map
          ("C-.")
          ("C-,"))
  :config
  ;; flyspell uses sit-for for delays which breaks things like
  ;; delete-selection-mode and company-mode. One possible solution is setting
  ;; flyspell-delay to nil but this will impact performance. Instead I disable
  ;; it completely for self-insert-command when it inserts anything besides
  ;; separators. See https://en.wikipedia.org/wiki/Unicode_character_property#General_Category
  ;; for Unicode properties.
  (advice-add #'flyspell-check-word-p :around (lambda (orig-fun &rest args)
                                                (if (eq this-command 'self-insert-command)
                                                  (memq (get-char-code-property (char-before) 'general-category) '(Zs Zl Zp))
                                                  (apply orig-fun args))))
  (advice-add #'uncomment-region :before (lambda (BEG END &optional _ARG)
                                           (flyspell-delete-region-overlays BEG END))))

(use-package pos-tip
  :autoload pos-tip-show)

(use-package langtool
  :ensure t
  :ensure pos-tip
  :commands langtool-check
  :custom
  (langtool-bin "languagetool-commandline")
  (langtool-default-language "en-US")
  (langtool-mother-tongue "ru-RU")
  (langtool-autoshow-message-function (lambda (overlays)
                                        (pos-tip-show (langtool-details-error-message overlays)))))

(use-package guess-language
  :hook (text-mode . guess-language-mode)
  :custom
  (guess-language-languages '(en ru))
  (guess-language-min-paragraph-length 15)
  :config
  (setcar (cdr (assoc 'ru guess-language-langcodes)) "ru")
  (defun guess-language-switch-langtool-function (lang _beginning _end)
    "Switch the voice used by langtool.

LANG is the ISO 639-1 code of the language (as a
symbol).  BEGINNING and END are the endpoints of the region in
which LANG was detected but these are ignored."
    (setq-local langtool-default-language lang))
  (with-eval-after-load "langtool"
    (add-hook 'guess-language-after-detection-functions #'guess-language-switch-langtool-function)))

(use-package minimap
  :commands minimap-mode
  :custom
  (minimap-width-fraction 0.1)
  (minimap-minimum-width 20)
  (minimap-window-location 'right))

(use-package fuzzy-matcher
  :ensure nil
  :demand t
  :init
  (load-library "libfuzzy_matcher_el.so")
  :config
  (defun fuzzy-matcher-without-tofu-char (string)
    (if (and
          (fboundp 'consult--tofu-p)
          (consult--tofu-p (aref string (- (length string) 1))))
      (substring string 0 (- (length string) 1))
      string))
  (defun fuzzy-matcher-propertize (pattern candidate)
    (let* ((score (fuzzy-matcher-skim-fuzzy-indices
                    (encode-coding-string pattern 'utf-8 t)
                    (encode-coding-string (fuzzy-matcher-without-tofu-char candidate) 'utf-8 t)))
            (candidate (copy-sequence candidate)))
      (unless (string-empty-p pattern)
        (put-text-property 0 1 'completion-score (- (* (or (car score) 0) 100) (length candidate)) candidate))
      (dolist (char (cdr score))
        (add-face-text-property char (1+ char) 'completions-common-part nil candidate))
      (when-let* ((char (last (cdr score)))
                   (char (car char)))
        (when (length> candidate (1+ char))
          (add-face-text-property (1+ char) (+ 2 char) 'completions-first-difference nil candidate)))
      candidate))
  (defun fuzzy-matcher-all-completions (string table pred point)
    (let* ((beforepoint (substring string 0 point))
            (afterpoint (substring string point))
            (bounds (completion-boundaries beforepoint table pred afterpoint))
            (infix (concat
                     (substring beforepoint (car bounds))
                     (substring afterpoint 0 (cdr bounds)))))
      (pcase-let ((`(,all ,_pattern ,prefix ,_suffix ,_carbounds)
                    (completion-substring--all-completions string table pred point #'completion-flex--make-flex-pattern)))
        (when all
          (nconc (mapcar (-partial #'fuzzy-matcher-propertize infix) all) (length prefix))))))
  (add-to-list 'completion-styles-alist '(fuzzy
                                           completion-flex-try-completion
                                           fuzzy-matcher-all-completions
                                           "Fuzzy completion with scoring."))
  (put 'fuzzy 'completion--adjust-metadata (lambda (metadata)
                                             ;; completion--flex-adjust-metadata has faulty check for the completion
                                             (if (let ((input (minibuffer-contents-no-properties)))
                                                   (or
                                                     (string-empty-p input)
                                                     (and
                                                       (eq (completion-metadata-get metadata 'category) 'file)
                                                       (string-suffix-p "/" input))))
                                               metadata
                                               (completion--flex-adjust-metadata metadata))))
  (setq completion-styles '(fuzzy)))

(use-package ido
  :ensure nil
  :commands (ido-completing-read
              ido-read-directory-name
              ido-read-file-name
              ido-read-buffer)
  :custom
  (ido-enable-flex-matching t)
  (ido-use-faces nil)
  :config
  ;; Show directories first.
  (defun ends-with-/ (s)
    (eq (aref s (1- (length s))) ?/))
  (defun ido-file-lessp (a b)
    (cond
      ((and (ends-with-/ a) (not (ends-with-/ b))) t)
      ((and (not (ends-with-/ a)) (ends-with-/ b)) nil)
      (t (string-lessp a b)))))

(use-package all-the-icons-nerd-fonts
  :demand t
  :after all-the-icons
  :config
  (all-the-icons-nerd-fonts-prefer))

(use-package vertico
  :demand t
  :bind (:map vertico-map
          ("TAB" . minibuffer-complete)
          ("M-TAB" . vertico-insert)
          ("<prior>" . vertico-scroll-down)
          ("<next>" . vertico-scroll-up)
          ("RET" . vertico-directory-enter)
          ("DEL" . vertico-directory-delete-char)
          ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :custom
  (vertico-resize nil)
  :config
  (vertico-mode))

(use-package vertico-multiform
  :demand t
  :ensure vertico
  :after vertico
  :custom
  (vertico-multiform-categories '((file (vertico-sort-function . sort-directories-first))))
  :config
  ;; Sort directories before files
  (defun sort-directories-first (files)
    (setq files (vertico-sort-history-alpha files))
    (nconc (seq-filter (-partial #'string-suffix-p "/") files)
      (seq-remove (-partial #'string-suffix-p "/") files)))
  (vertico-multiform-mode))

(use-package vertico-mouse
  :demand t
  :ensure vertico
  :after vertico
  :config
  (vertico-mouse-mode))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(use-package marginalia
  :demand t
  :config
  (defun marginalia-project-name (buffer)
    (let ((name (buffer-file-name buffer)))
      (if (and name (projectile-project-p name))
        (projectile-project-name (projectile-project-root name))
        "")))
  (el-patch-defun marginalia-annotate-buffer (cand)
    "Annotate buffer CAND with modification status, file name and major mode."
    (when-let (buffer (get-buffer cand))
      (marginalia--fields
        ((marginalia--buffer-status buffer))
        (el-patch-add ((marginalia-project-name buffer)
                        :truncate 0.2 :face 'marginalia-modified))
        ((marginalia--buffer-file buffer)
          :truncate -0.5 :face 'marginalia-file-name))))
  (marginalia-mode))

(use-package all-the-icons-completion
  :demand t
  :config
  (all-the-icons-completion-mode))

(use-package embark
  :commands kill-target-buffer
  :bind (:map minibuffer-local-map
          ("C-." . embark-act)
          ("C-;" . embark-dwim)
          ("C-e" . embark-export))
  :custom
  (embark-mixed-indicator-delay 0)
  (embark-mixed-indicator-both t)
  (embark-quit-after-action nil)
  :config
  (setq embark-pre-action-hooks
    (delete '(kill-buffer embark--confirm) embark-pre-action-hooks))
  (defun kill-target-buffer ()
    (interactive)
    (if-let ((buffer (seq-find
                       (lambda (target)
                         (eq (plist-get target :type) 'buffer))
                       (embark--targets))))
      (embark--act 'kill-buffer buffer)
      (user-error "No buffer target found"))))

(use-package projectile
  :demand t
  :bind (:map projectile-mode-map
          ("C-p f" . projectile-find-file)
          ("C-p o" . projectile-find-file)
          ("C-p C-p" . projectile-switch-project))
  :config
  (projectile-mode))

(use-package consult
  :bind (("<f2>" . consult-buffer)
          ([remap goto-line] . consult-goto-line)
          :map projectile-mode-map
          ("C-p g" . consult-ripgrep))
  :init
  (setq
    register-preview-function #'consult-register-format
    xref-show-xrefs-function #'consult-xref
    xref-show-definitions-function #'consult-xref)
  (advice-add #'register-preview :override #'consult-register-window)
  :custom
  (consult-project-function (lambda (_) (projectile-project-root)))
  (consult-async-min-input 2)
  :config
  (consult-customize consult-buffer
    :preview-key nil
    :keymap (let ((map (make-sparse-keymap)))
              (define-key map (kbd "<f2>") #'keyboard-escape-quit)
              (define-key map (kbd "C-k") #'kill-target-buffer)
              map)))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package helpful
  :bind (([remap describe-key] . helpful-key))
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
          ("C-e" . dired-toggle-read-only)))

(use-package wdired
  :ensure nil
  :bind (:map wdired-mode-map
          ("C-s" . wdired-finish-edit)))

(use-package ediff-wind
  :ensure nil
  :custom
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-split-window-function #'split-window-horizontally))

(use-package tramp
  :ensure nil
  :custom
  (tramp-default-method "ssh"))

(use-package ag
  :commands (ag
              ag-files
              ag-regexp
              ag-project
              ag-project-files
              ag-project-regexp
              ag-dired
              ag-dired-regexp
              ag-project-dired
              ag-project-dired-regexp))

(use-package anzu
  :demand t
  :bind (([remap query-replace] . anzu-query-replace)
          ([remap query-replace-regexp] . anzu-query-replace-regexp))
  :config
  (global-anzu-mode t))

(use-package doom-modeline
  :demand t
  :custom
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  :config
  (doom-modeline-mode))

(use-package centaur-tabs
  :demand
  :custom
  (centaur-tabs-set-icons t)
  :config
  (push 'treemacs-mode-hook centaur-tabs-hide-tabs-hooks)
  (advice-add #'centaur-tabs-hide-tab-cached :after-until (lambda (buffer)
                                                            (with-current-buffer buffer
                                                              (and
                                                                (boundp 'polymode-mode)
                                                                polymode-mode
                                                                (string-prefix-p " " (buffer-name (current-buffer)))))))
  (centaur-tabs-mode t)
  (centaur-tabs-group-by-projectile-project)
  (remove-hook 'kill-buffer-hook 'centaur-tabs-buffer-track-killed))

(use-package rg
  :after projectile
  :custom
  (rg-group-result t)
  (rg-command-line-flags '("--hidden"))
  :bind (:map projectile-mode-map
          ("C-p G" . rg-project)
          :map rg-mode-map
          ("C-b")
          ("C-f")
          ("F" . rg-forward-history)
          ("B" . rg-back-history)))

(use-package wgrep
  :bind (:map wgrep-mode-map
          ("C-s" . wgrep-finish-edit)
          :map embark-consult-rerun-map
          ("C-e" . wgrep-change-to-wgrep-mode))
  :custom
  (wgrep-auto-save-buffer t))

(use-package company
  :demand t
  :bind (:map company-mode-map
          ("TAB" . company-indent-or-complete-common))
  :custom
  (company-tooltip-align-annotations t)
  (company-require-match nil)
  (company-tooltip-maximum-width 140)
  :config
  (global-company-mode 1))

(use-package company-dabbrev
  :ensure company
  :after company
  :commands company-dabbrev
  :custom
  (company-dabbrev-downcase nil))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package multiple-cursors-core
  :ensure multiple-cursors
  :commands (mc/multiple-cursors-mode-when-num-cursors>1
              mc/quit-leaving-cursors)
  :bind (("C-b" . mc/mark-all-like-this)
          ("C-S-b" . mc/edit-lines)
          :map mc/keymap
          ("C-S-b" . mc/keyboard-quit))
  :config
  ;; Copy/paste.
  (with-no-warnings
    (defvar-local mc/clipboard nil))
  (defun mc/cut-copy-across-cursors (cut)
    (setq mc/clipboard nil)
    (mc/for-each-cursor-ordered
      (mc/restore-state-from-overlay cursor)
      (push
        (if (region-active-p)
          (buffer-substring
            (caar (region-bounds))
            (cdar (region-bounds)))
          "")
        mc/clipboard)
      (mc/execute-command-for-fake-cursor
        (lambda ()
          (interactive)
          (if (and cut (not buffer-read-only))
            (delete-region
              (caar (region-bounds))
              (cdar (region-bounds)))
            (deactivate-mark)))
        cursor)))
  (defun mc/copy-across-cursors ()
    (interactive)
    (mc/cut-copy-across-cursors nil))
  (defun mc/cut-across-cursors ()
    (interactive)
    (mc/cut-copy-across-cursors t))
  (defun mc/paste-across-cursors ()
    (interactive)
    (if mc/clipboard
      (let ((clipboard (reverse mc/clipboard)))
        (mc/for-each-cursor-ordered
          (when clipboard
            (mc/execute-command-for-fake-cursor
              (lambda ()
                (interactive)
                (insert (car clipboard)))
              cursor)
            (setq clipboard (cdr clipboard)))))
      (mc/execute-command-for-all-cursors #'cua-paste)))
  (add-to-list
    'emulation-mode-map-alists
    `((multiple-cursors-mode . ,(-doto (make-sparse-keymap)
                                  (define-key [remap yank] #'mc/paste-across-cursors)
                                  (define-key [remap clipboard-yank] #'mc/paste-across-cursors)
                                  (define-key [remap x-clipboard-yank] #'mc/paste-across-cursors)
                                  (define-key [remap copy-region-as-kill] #'mc/copy-across-cursors)
                                  (define-key [remap kill-region] #'mc/cut-across-cursors)
                                  (define-key [remap clipboard-kill-region] #'mc/cut-across-cursors)))))
  (defun mc/clear-clipboard ()
    (setq mc/clipboard nil))
  (add-hook 'multiple-cursors-mode-hook #'mc/clear-clipboard)
  ;; Fake cursors manipulation.
  (defun mc/multiple-cursors-mode-when-num-cursors>1 ()
    (interactive)
    (when (> (mc/num-cursors) 1)
      (multiple-cursors-mode 1)))
  (defun mc/quit-leaving-cursors ()
    (interactive)
    (cl-letf (((symbol-function 'mc/remove-fake-cursors)
                (lambda ())))
      (multiple-cursors-mode 0)))
  (defun mc/remove-fake-cursors-interactive ()
    (interactive)
    (mc/remove-fake-cursors))
  ;; Disable saving/loading commands.
  (defun mc/load-lists ())
  (defun mc/save-lists ())
  ;; Define commands.
  (setq mc/cmds-to-run-once '(hydra-multiple-cursors/body
                               hydra-multiple-cursors/nil
                               cua--prefix-override-handler
                               mc/toggle-fake-cursor
                               mc/copy-across-cursors
                               mc/cut-across-cursors
                               mc/paste-across-cursors))
  (setq mc/cmds-to-run-for-all '(back-to-indentation-or-beginning
                                  end-of-code-or-line
                                  indent-for-tab-command
                                  org-self-insert-command)))

(use-package mc-mark-more
  :ensure multiple-cursors
  :commands mc/toggle-fake-cursor
  :bind (("C-S-<mouse-1>" . mc/add-cursor-on-click))
  :config
  (defun mc/toggle-fake-cursor ()
    (interactive)
    (let ((existing (mc/fake-cursor-at-point)))
      (if existing
        (mc/remove-fake-cursor existing)
        (mc/create-fake-cursor-at-point)))))

(use-package hydra
  :bind (("<C-return>" . hydra-multiple-cursors/body)
          :map prog-mode-map
          ("C-(" . hydra-smartparens/body))
  :config
  (defhydra hydra-multiple-cursors (:foreign-keys run
                                     :body-pre (progn
                                                 (mc/quit-leaving-cursors)
                                                 (mc/toggle-fake-cursor))
                                     :post (mc/multiple-cursors-mode-when-num-cursors>1))
    "multiple-cursors"
    ("<C-return>" mc/toggle-fake-cursor "toggle")
    ("<return>" nil "apply")
    ("<escape>" mc/remove-fake-cursors-interactive "quit" :exit t))
  (defhydra hydra-smartparens (:hint nil)
    "
 Moving^^^^                                  Slurp & Barf^^   Wrapping^^            Sexp juggling^^^^               Destructive
-----------------------------------------------------------------------------------------------------------------------------------
 [_<home>_] beginning  [_<down>_] down       [_h_] bw slurp   [_R_]   rewrap        [_S_] split   [_t_] transpose   [_c_] change inner  [_w_] copy
 [_<end>_] end         [_S-<down>_] bw down  [_H_] bw barf    [_u_]   unwrap        [_s_] splice  [_A_] absorb      [_C_] change outer
 [_<right>_] forward   [_<up>_] up           [_l_] slurp      [_U_]   bw unwrap     [_r_] raise   [_E_] emit        [_k_] kill          [_<escape>_] quit
 [_<left>_] backward   [_S-<up>_] bw up      [_L_] barf       [_(__{__[_] wrap (){}[]   [_j_] join    [_o_] convolute   [_K_] bw kill       [_q_] quit"
    ;; Moving
    ("<home>" sp-beginning-of-sexp)
    ("<end>" sp-end-of-sexp)
    ("<right>" sp-forward-sexp)
    ("<left>" sp-backward-sexp)
    ("<down>" sp-down-sexp)
    ("S-<down>" sp-backward-down-sexp)
    ("<up>" sp-up-sexp)
    ("S-<up>" sp-backward-up-sexp)
    ;; Slurping & barfing
    ("h" sp-backward-slurp-sexp)
    ("H" sp-backward-barf-sexp)
    ("l" sp-forward-slurp-sexp)
    ("L" sp-forward-barf-sexp)
    ;; Wrapping
    ("R" sp-rewrap-sexp)
    ("u" sp-unwrap-sexp)
    ("U" sp-backward-unwrap-sexp)
    ("(" sp-wrap-round)
    ("{" sp-wrap-curly)
    ("[" sp-wrap-square)
    ;; Sexp juggling
    ("S" sp-split-sexp)
    ("s" sp-splice-sexp)
    ("r" sp-raise-sexp)
    ("j" sp-join-sexp)
    ("t" sp-transpose-sexp)
    ("A" sp-absorb-sexp)
    ("E" sp-emit-sexp)
    ("o" sp-convolute-sexp)
    ;; Destructive editing
    ("c" sp-change-inner :exit t)
    ("C" sp-change-enclosing :exit t)
    ("k" sp-kill-sexp)
    ("K" sp-backward-kill-sexp)
    ;; Other
    ("w" sp-copy-sexp)
    ("<escape>" nil)
    ("q" nil)))

(use-package undo-tree
  :demand t
  :bind (:map undo-tree-map
          ([remap undo] . undo-tree-undo)
          ([remap undo-only] . undo-tree-undo)
          ("C-S-z" . undo-tree-redo)
          ("C-y" . undo-tree-redo)
          ("C-w" . last-edit))
  :init
  (setq undo-tree-map (make-sparse-keymap))
  :custom
  (undo-tree-auto-save-history nil)
  :config
  (global-undo-tree-mode)
  (advice-add #'undo-tree-overridden-undo-bindings-p :override (lambda ()))
  (defun undo-tree-make-hashed-history-save-file-name (file)
    (f-mkdir (concat user-emacs-directory "undo/"))
    (concat
      user-emacs-directory
      "undo/"
      (let ((filename (file-name-nondirectory file)))
        (substring-no-properties filename 0 (min 64 (length filename))))
      "~"
      (secure-hash 'sha256 file)))
  (advice-add #'undo-tree-make-history-save-file-name :override #'undo-tree-make-hashed-history-save-file-name)
  (defun last-edit-next (undo tree)
    (when undo
      (if tree
        (undo-tree-node-previous undo)
        (cdr undo))))
  (defun last-edit (arg)
    "Go back to last add/delete edit."
    (interactive "^P")
    (unless arg
      (setq arg 1))
    (let ((undo buffer-undo-list)
           (tree))
      (unless (eq undo t)
        (while undo
          (pcase (if tree
                   (car (undo-tree-node-undo undo))
                   (car undo))
            (`(,beg . ,end) (let ((pos (cond
                                         ((and (integerp beg) (integerp end)) end)
                                         ((and (stringp beg) (integerp end)) (abs end))
                                         ((eq beg 'apply) (pcase end
                                                            (`(,delta ,beg ,end . ,_)
                                                              (when (and
                                                                      (integerp delta)
                                                                      (integerp beg)
                                                                      (integerp end))
                                                                end)))))))
                              (if pos
                                (progn
                                  (setq arg (1- arg))
                                  (if (<= arg 0)
                                    (progn
                                      (goto-char pos)
                                      (setq undo nil))
                                    (setq undo (last-edit-next undo tree))))
                                (setq undo (last-edit-next undo tree)))))
            (`undo-tree-canary (if tree
                                 (progn
                                   (error "Inner undo-tree-canary")
                                   (setq undo nil))
                                 (setq
                                   undo (undo-tree-current buffer-undo-tree)
                                   tree t)))
            (_ (setq undo (last-edit-next undo tree)))))))))

(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :bind (:map hs-minor-mode-map
          ("C-`" . hs-toggle-hiding)))

(use-package origami
  :bind (:map origami-mode-map
          ("C-`" . origami-toggle-node))
  :config
  (add-hook 'origami-mode-hook
    (lambda ()
      (hs-minor-mode -1))))

(use-package org
  :ensure nil
  :bind (:map org-mode-map
          ("M-TAB" . org-cycle)
          ("S-<down>")
          ("S-<up>")
          ("S-<left>")
          ("S-<right>")
          ("C-S-<down>")
          ("C-S-<up>")
          ("C-S-<left>")
          ("C-S-<right>"))
  :custom
  (org-support-shift-select t)
  :config
  (defmath lookupfst (val s-list r-list)
    (org-lookup-first val s-list r-list))
  (defmath lookuplst (val s-list r-list)
    (org-lookup-last val s-list r-list)))

(use-package org-modern
  :demand t
  :after org
  :config
  (global-org-modern-mode))

(use-package org-agenda
  :ensure nil
  :custom
  (org-agenda-file-regexp "\\`[^.].*\\.org\\\(\\.gpg\\\)?\\'")
  (org-agenda-start-on-weekday nil)
  (org-agenda-span 14)
  (org-agenda-files '("~/calendar.org")))

(use-package org-roam
  :custom
  (org-roam-directory (expand-file-name "~/roam/"))
  :config
  (org-roam-db-autosync-mode))

(use-package org-roam-capture
  :ensure org-roam
  :after org-roam
  :custom
  (org-roam-capture-templates '(("d" "default" plain "%?"
                                  :target (file+head "${slug}.org.gpg"
                                            "# -*- mode:org; epa-file-encrypt-to: (\"kurnevsky@gmail.com\") -*-\n#+title: ${title}\n")
                                  :unnarrowed t))))

(use-package org-ql)

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :bind (:map yas-keymap
          ("<return>" . yas-next-field-or-maybe-expand))
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :demand t
  :after yasnippet)

(use-package vc
  :ensure nil
  :custom
  (vc-follow-symlinks t))

(use-package magit
  :demand t
  :bind (("<C-m> <C-m>" . magit-status)
          ("<C-m> b" . magit-blame-addition)
          ("<C-m> s" . magit-show-commit)
          :map magit-blame-mode-map
          ("<C-m> b" . magit-blame-quit)
          :map magit-status-mode-map
          ("TAB" . magit-section-cycle))
  :config
  (add-hook 'magit-status-mode-hook
    (lambda () (company-mode -1))))

(use-package forge
  :demand t
  :after magit
  :config
  (let ((host "gitlab.evolution.com"))
    (add-to-list
      'forge-alist
      `(,host ,(concat host "/api/v4") ,host forge-gitlab-repository))))

(use-package pass
  :mode (".password-store/.*\\.gpg\\'" . pass-view-mode))

(use-package auth-source
  :ensure nil
  :custom
  (auth-source-save-behavior nil))

(use-package auth-source-pass
  :ensure nil
  :demand t
  :after auth-source
  :config
  (auth-source-pass-enable))

(use-package age
  :demand t
  :custom
  (age-program "rage")
  :config
  (age-file-enable))

(use-package git-modes)

(use-package diff-hl
  :demand t
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(use-package diff-hl-flydiff
  :demand t
  :ensure diff-hl
  :after diff-hl
  :config
  (diff-hl-flydiff-mode))

(use-package magit-delta
  :demand t
  :config
  (magit-delta-mode))

(use-package treemacs
  :bind (("<f8>" . treemacs)
          :map treemacs-mode-map
          ("<M-up>")
          ("<M-down>"))
  :custom
  (treemacs-collapse-dirs 3)
  (treemacs-position 'right)
  (treemacs-project-follow-cleanup t)
  (treemacs-show-cursor t)
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (add-hook 'treemacs-mode-hook (lambda () (display-line-numbers-mode -1))))

(use-package treemacs-projectile
  :bind ("S-<f8>" . treemacs-projectile))

(use-package flycheck
  :demand t
  :bind (("C-e" . flycheck-list-errors))
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  :config
  (global-flycheck-mode)
  (add-to-list 'display-buffer-alist
    `(,(rx bos "*Flycheck errors*" eos)
       (display-buffer-reuse-window display-buffer-below-selected)
       (reusable-frames . visible)
       (side            . bottom)
       (window-height   . 0.3))))

(use-package flycheck-package
  :after flycheck
  :demand t
  :config
  (flycheck-package-setup))

(use-package pdf-loader
  :ensure pdf-tools
  :demand t
  :config
  (pdf-loader-install))

(use-package pdf-view
  :ensure pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook (lambda ()
                                  (display-line-numbers-mode -1))))

(use-package adoc-mode
  :mode ("\\.adoc\\'" . adoc-mode))

(use-package polymode
  :config
  (advice-add #'flycheck-may-enable-mode :after-while (lambda ()
                                                        (not (and polymode-mode (buffer-base-buffer)))))
  ;; Doesn't work well with polymode.
  (add-hook 'polymode-init-inner-hook (lambda ()
                                        (set (make-local-variable 'highlight-indent-guides-responsive) nil)))
  ;; Fix highlight thing
  (add-hook 'polymode-before-switch-buffer-hook (lambda (_old _new)
                                                  (highlight-thing-remove-last))))

(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook (lambda () (setq show-trailing-whitespace t))))

(use-package poly-markdown)

(use-package poly-org)

(use-package poly-rst)

(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4))

(use-package treesit-auto
  :demand t
  :config
  (push
    (make-treesit-auto-recipe
      :lang 'scala
      :ts-mode 'scala-ts-mode
      :remap 'scala-mode
      :url "https://github.com/KaranAhlawat/scala-ts-mode")
    treesit-auto-recipe-list)
  (push 'scala treesit-auto-langs)
  (global-treesit-auto-mode))

(use-package conf-mode
  :ensure nil
  :mode ("/Cargo.lock\\'" . conf-toml-mode))

(use-package yaml-mode)

(use-package groovy-mode
  :mode ("/Jenkinsfile\\." . groovy-mode))

(use-package scala-mode
  :mode ("\\.sc\\'" . scala-mode)
  :interpreter ("scala" . scala-mode))

(use-package scala-ts-mode)

(use-package haskell-mode)

(use-package eldoc
  :ensure nil
  :commands (eldoc-mode turn-on-eldoc-mode)
  :init
  (add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode))

(use-package rust-mode)

(use-package matlab
  :ensure matlab-mode)

(use-package ess)

(use-package csv-mode)

(use-package json-mode)

(use-package dockerfile-mode)

(use-package systemd)

(use-package pkgbuild-mode)

(use-package go-mode)

(use-package typescript-mode)

(use-package fsharp-mode)

(use-package lua-mode)

(use-package nix-mode)

(use-package dhall-mode
  :custom
  (dhall-format-at-save nil))

(use-package mermaid-mode)

(use-package bats-mode)

(use-package terraform-mode)

(when (executable-find "agda-mode")
  (use-package agda2
    :ensure nil
    :load-path (lambda ()
                 (let ((coding-system-for-read 'utf-8))
                   (file-name-directory (shell-command-to-string "agda-mode locate"))))
    :mode ("\\.l?agda\\'" . agda2-mode)
    :interpreter ("agda -I" . agda2-mode)))

(use-package editorconfig
  :demand t
  :config
  (editorconfig-mode 1))

(use-package direnv
  :demand t
  :config
  (direnv-mode)
  (advice-add #'direnv-update-directory-environment :after (lambda (&rest _)
                                                             (when doom-modeline-env--command
                                                               (setq doom-modeline-env--command (executable-find (file-name-nondirectory doom-modeline-env--command)))))))

(use-package format-all)

(use-package dumb-jump
  :custom
  (dumb-jump-selector 'completing-read))

(use-package lsp-mode
  :init
  (setenv "LSP_USE_PLISTS" "true")
  (setq lsp-keymap-prefix "C-l")
  :custom
  (lsp-session-file "/tmp/.lsp-session")
  (lsp-auto-guess-root t)
  (lsp-prefer-flymake nil)
  (lsp-file-watch-threshold nil)
  (lsp-keep-workspace-alive nil)
  (lsp-lens-auto-enable t)
  (lsp-prefer-capf t)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-segments '(symbols))
  (lsp-semantic-tokens-enable t)
  (lsp-semantic-tokens-honor-refresh-requests t)
  (lsp-semantic-tokens-apply-modifiers nil)
  :config
  (lsp-enable-which-key-integration)
  (defun lsp-activate-if-already-activated (server-id)
    (when (lsp-find-workspace server-id (buffer-file-name))
      (lsp)))
  (add-hook 'rust-mode-hook (-partial #'lsp-activate-if-already-activated 'rust-analyzer))
  (add-hook 'rust-ts-mode-hook (-partial #'lsp-activate-if-already-activated 'rust-analyzer))
  (add-hook 'scala-mode-hook (-partial #'lsp-activate-if-already-activated 'metals))
  (add-hook 'scala-ts-mode-hook (-partial #'lsp-activate-if-already-activated 'metals))
  ;; Hack for metals to send ranges in hover request.
  (el-patch-defun lsp--text-document-position-params (&optional identifier position)
    "Make TextDocumentPositionParams for the current point in the current document.
If IDENTIFIER and POSITION are non-nil, they will be used as the document
identifier and the position respectively."
    (list :textDocument (or identifier (lsp--text-document-identifier))
      :position (or position (lsp--cur-position))
      (el-patch-add :range (when (use-region-p) (lsp--region-to-range (region-beginning) (region-end)))))))

(use-package lsp-ui
  :custom
  (lsp-ui-doc-alignment 'window)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-sideline-enable nil)
  :config
  (delete (list 'no-accept-focus) lsp-ui-doc-frame-parameters)
  (add-to-list 'lsp-ui-doc-frame-parameters '(no-accept-focus . t)))

(use-package lsp-origami
  :hook (lsp-after-open . lsp-origami-activate-when-supported)
  :config
  (defun lsp-origami-activate-when-supported ()
    (when (lsp--capability "foldingRangeProvider")
      (origami-mode t)
      (lsp-origami-mode t))))

(use-package lsp-treemacs
  :custom
  (lsp-treemacs-error-list-current-project-only t))

(use-package lsp-rust
  :ensure lsp-mode
  :after lsp-mode
  :demand t
  :custom
  (lsp-rust-clippy-preference "on")
  (lsp-rust-cfg-test t)
  (lsp-rust-build-on-save t)
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-cargo-watch-args ["--all"]))

(use-package lsp-java
  :after lsp-mode
  :demand t
  :config
  (defun lsp-java--ls-command ()
    (lsp-java--ensure-dir lsp-java-workspace-dir)
    `("jdt-language-server" "-data" ,(lsp-file-local-name lsp-java-workspace-dir)))
  (defun lsp-java--locate-server-jar ()
    t))

(use-package lsp-metals
  :after lsp-mode
  :demand t
  :init
  (setq lsp-metals-server-command "env")
  :custom
  (lsp-metals-server-args '("JAVA_TOOL_OPTIONS=-Dmetals.allow-multiline-string-formatting=off -Dmetals.icons=unicode" "metals"))
  (lsp-metals-super-method-lenses-enabled t)
  (lsp-metals-enable-semantic-highlighting t)
  (lsp-metals-treeview-views '("metalsPackages" "metalsBuild")))

(use-package lsp-haskell
  :after lsp-mode
  :demand t)

(use-package dap-mode
  :ensure t
  :ensure posframe
  :config
  (delete 'tooltip dap-auto-configure-features)
  (dap-mode t))

(use-package dap-ui
  :demand t
  :ensure dap-mode
  :after dap-mode
  :config
  (dap-ui-mode t))

(use-package sideline
  :hook (flycheck-mode . sideline-mode)
  :custom
  (sideline-backends-right '((sideline-lsp . up)
                              (sideline-flycheck . down))))

(use-package sideline-flycheck
  :hook (flycheck-mode . sideline-flycheck-setup))

(use-package sideline-lsp)

(use-package llama-cpp
  :custom
  (llama-cpp-chat-prompt "You are Echo, an advanced AI system.")
  (llama-cpp-port 8081))

(use-package khalel
  :commands (khalel-import-events khalel-run-vdirsyncer)
  :custom
  (khalel-import-org-file "~/calendar.org")
  :config
  (khalel-add-capture-template))

(use-package mu4e
  :commands mu4e
  :custom
  (mu4e-view-show-addresses t)
  (mu4e-headers-results-limit 1000)
  (mu4e-change-filenames-when-moving t)
  (mu4e-get-mail-command "mbsync --all")
  (mu4e-update-interval (* 15 60))
  :config
  ;; Remove padding so that content won't be shifted comparing to the header
  (dolist (hook '(mu4e-main-mode-hook mu4e-headers-mode-hook mu4e-view-mode-hook mu4e-compose-mode-hook))
    (add-hook hook (lambda ()
                     (display-line-numbers-mode -1)
                     (setq left-fringe-width 0)
                     (setq show-trailing-whitespace nil))))
  (defun mu4e-shr2text ()
    "Html to text using the shr engine."
    (interactive)
    (defvar shr-inhibit-images)
    (defvar shr-width)
    (let ((shr-inhibit-images t)
           (shr-width (- (window-body-width) 8)))
      (shr-render-region (point-min) (point-max))
      (goto-char (point-min))))
  (defvar mu4e-sent-folder-alternatives '("/[Gmail]/Sent Mail" ;; gmail
                                           "/Sent" ;; yandex
                                           "/Sent Items" ;; outlook
                                           ))
  (defvar mu4e-drafts-folder-alternatives '("/[Gmail]/Drafts" ;; gmail
                                             "/Drafts" ;; yandex, outlook
                                             ))
  (defvar mu4e-trash-folder-alternatives '("/[Gmail]/Trash" ;; gmail
                                            "/Trash" ;; yandex
                                            "/Deleted Items" ;; outlook
                                            ))
  (defvar mu4e-refile-folder-alternatives '("/[Gmail]/Archive" ;; gmail
                                             "/Archive" ;; yandex, outlook
                                             ))
  (defun choose-mu4e-alternative (name alternatives)
    (string-remove-prefix "~/Maildir"
      (seq-find #'file-directory-p
        (mapcar (lambda (value) (concat "~/Maildir/" name value))
          (symbol-value alternatives)))))
  (defun make-mu4e-context-generic (name)
    (make-mu4e-context
      :name name
      :enter-func `(lambda () (mu4e-message (concat "Entering " ,name " context")))
      :leave-func `(lambda () (mu4e-message (concat "Leaving " ,name " context")))
      :match-func `(lambda (msg) (when msg
                                   (string-prefix-p (concat "/" ,name) (mu4e-message-field msg :maildir))))
      :vars `((mu4e-sent-folder . ,(choose-mu4e-alternative name 'mu4e-sent-folder-alternatives))
               (mu4e-drafts-folder . ,(choose-mu4e-alternative name 'mu4e-drafts-folder-alternatives))
               (mu4e-trash-folder . ,(choose-mu4e-alternative name 'mu4e-trash-folder-alternatives))
               (mu4e-refile-folder . ,(choose-mu4e-alternative name 'mu4e-refile-folder-alternatives)))))
  (setq mu4e-contexts (mapcar #'make-mu4e-context-generic (directory-files "~/Maildir" nil "[^.]"))))

;; ========== Key bindings ==========

(defun new-empty-buffer ()
  "Create a new empty buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (switch-to-buffer buffer)
    (text-mode)
    (setq buffer-offer-save t)
    (setq-local new-untitled t)))
(defun move-cursor-next-pane ()
  "Move cursor to the next pane."
  (interactive)
  (other-window 1))
(defun move-cursor-previous-pane ()
  "Move cursor to the previous pane."
  (interactive)
  (other-window -1))
(defun isearch-forward-from-begin ()
  "Search from the beginning of document."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (isearch-forward)))
(defun back-to-indentation-or-beginning ()
  "Move point to the first non-whitespace character on this line.
If it's already there move it to the beginning of this line."
  (interactive "^")
  (when (= (point) (progn (back-to-indentation) (point)))
    (beginning-of-line)))
(defun end-of-code-or-line (arg)
  "Move point to the end of this line ignoring comments.
If it's already there move it to the end of this line.
With argument ARG not nil or 1, move forward ARG - 1 lines first.
Comments are recognized in any mode that sets `syntax-ppss'
properly."
  (interactive "^P")
  (let* ((start (point))
          (bol (save-excursion
                 (beginning-of-line)
                 (point)))
          (eol (progn
                 (move-end-of-line arg)
                 (point)))
          (syn (syntax-ppss))
          (boc (nth 8 syn)))
    (when (and
            boc
            (not (nth 3 syn))
            (> boc bol))
      (goto-char boc))
    (skip-chars-backward " \t")
    (when (or (= start (point)) (= bol (point)))
      (goto-char eol))))
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
      (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (forward-line)))
(defun which-active-modes ()
  "Gives a message of which minor modes are enabled in the current buffer."
  (interactive)
  (let ((active-modes))
    (mapc (lambda (mode) (condition-case nil
                           (if (and (symbolp mode) (symbol-value mode))
                             (add-to-list 'active-modes mode))
                           (error nil)))
      minor-mode-list)
    (message "Active modes are %s" active-modes)))
(defun tell-emacsclients-for-buffer-to-die ()
  "Sends error exit command to every client for the current buffer."
  (interactive)
  (dolist (proc server-buffer-clients)
    (server-send-string proc "-error die")))
(defun jq-region ()
  "Format json with jq in a region."
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "jq ." (buffer-name) t)))
(defun jq-buffer ()
  "Format json with jq in a buffer."
  (interactive)
  (save-excursion
    (shell-command-on-region (point-min) (point-max) "jq ." (buffer-name) t)))
(defun xmllint-region ()
  "Format xml with xmllint in a region."
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "xmllint --format -" (buffer-name) t)))
(defun xmllint-buffer ()
  "Format xml with xmllint in a buffer."
  (interactive)
  (save-excursion
    (shell-command-on-region (point-min) (point-max) "xmllint --format -" (buffer-name) t)))
(defun xmlstarlet-region ()
  "Format xml with xmlstarlet in a region."
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "xmlstarlet format" (buffer-name) t)))
(defun xmlstarlet-buffer ()
  "Format xml with xmlstarlet in a buffer."
  (interactive)
  (save-excursion
    (shell-command-on-region (point-min) (point-max) "xmlstarlet format" (buffer-name) t)))
(defun save-as (filename)
  "Save current buffer to FILENAME."
  (interactive "F")
  (save-restriction
    (widen)
    (write-region (point-min) (point-max) filename)
    (find-file filename)))
(defun scroll-right-2()
  "Scroll right by 2 columns."
  (interactive)
  (when-let ((window (window-at (cadr (mouse-position))
                       (cddr (mouse-position))
                       (car (mouse-position)))))
    (with-selected-window window
      (scroll-right 2))))
(defun scroll-left-2()
  "Scroll left by 2 columns."
  (interactive)
  (when-let ((window (window-at (cadr (mouse-position))
                       (cddr (mouse-position))
                       (car (mouse-position)))))
    (with-selected-window window
      (scroll-left 2))))
(global-set-key (kbd "<escape>") #'keyboard-escape-quit)
(global-set-key (kbd "C-f") #'isearch-forward)
(define-key isearch-mode-map (kbd "C-f") #'isearch-repeat-forward)
(global-set-key (kbd "C-S-f") #'isearch-backward)
(define-key isearch-mode-map (kbd "C-S-f") #'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "C-v") #'isearch-yank-kill)
(define-key isearch-mode-map (kbd "<escape>") #'isearch-abort)
(global-set-key (kbd "C-r") #'query-replace)
(global-set-key (kbd "C-n") #'new-empty-buffer)
(global-set-key (kbd "C-o") #'find-file)
(global-set-key (kbd "C-s") #'save-buffer)
(global-set-key (kbd "C-S-s") #'save-as)
(global-set-key (kbd "C-a") #'mark-whole-buffer)
(global-set-key (kbd "C-=") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)
(global-set-key (kbd "C-+") #'global-text-scale-adjust)
(global-set-key (kbd "C-_") (lambda () (interactive) (global-text-scale-adjust -1)))
(global-set-key (kbd "C-,") #'move-cursor-previous-pane)
(global-set-key (kbd "C-.") #'move-cursor-next-pane)
(global-set-key (kbd "<home>") #'back-to-indentation-or-beginning)
(global-set-key (kbd "<end>") #'end-of-code-or-line)
(global-set-key (kbd "C-/") #'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-k C-k") #'kill-current-buffer)
(global-set-key (kbd "C-k w") #'delete-window)
(global-set-key (kbd "C-|") #'split-window-horizontally)
(global-set-key (kbd "C-\\") #'split-window-vertically)
(global-set-key (kbd "C-x C-M-c") #'tell-emacsclients-for-buffer-to-die)
(global-set-key (kbd "<S-f2>") #'list-buffers)
(global-set-key (kbd "S-C-M-<left>") #'shrink-window-horizontally)
(global-set-key (kbd "S-C-M-<right>") #'enlarge-window-horizontally)
(global-set-key (kbd "S-C-M-<down>") #'shrink-window)
(global-set-key (kbd "S-C-M-<up>") #'enlarge-window)
(global-set-key (kbd "<f7>") #'pop-global-mark)
(global-set-key (kbd "S-<f4>") (lambda ()
                                 (interactive)
                                 (kmacro-call-macro 0)))
(global-set-key (kbd "<f12>") (lambda ()
                                (interactive)
                                (when-let (value (completing-read "Kill ring: " (lambda (string pred action)
                                                                                  (if (eq action 'metadata)
                                                                                    `(metadata (display-sort-function . ,#'identity))
                                                                                    (complete-with-action action kill-ring string pred)))))
                                  (kill-new value))))
(global-set-key (kbd "<wheel-left>") #'scroll-left-2)
(global-set-key (kbd "<double-wheel-left>") #'scroll-left-2)
(global-set-key (kbd "<triple-wheel-left>") #'scroll-left-2)
(global-set-key (kbd "<wheel-right>") #'scroll-right-2)
(global-set-key (kbd "<double-wheel-right>") #'scroll-right-2)
(global-set-key (kbd "<triple-wheel-right>") #'scroll-right-2)

(provide 'init)
;;; init.el ends here

;; Local Variables:
;; lisp-indent-offset: 2
;; End:
