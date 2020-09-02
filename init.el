;;; init.el --- My custom Emacs configuration file. -*- lexical-binding: t; -*-

;;; Commentary:

;;================================================================================
;;
;; Jessie Hildebrandt's
;; Run-Anywhere Emacs Config
;;
;; This configuration file was designed to work with Emacs 27, but should work
;; with Emacs 26.
;;
;; Generic keybinds are documented at the top of the "Key Bindings" subsection,
;; and package-specific keybinds are documented above each "use-package" statement
;; in the "EXTERNAL PACKAGES" section.
;;
;;================================================================================

;;; Code:

;;========================================
;;
;; EMACS CONFIGURATION
;;
;;========================================

;;====================
;; Garbage Collector
;;====================

;; Set the default garbage collection parameters. (~32MB)
(defconst init-file/gc-cons-threshold 32000000 "Preferred garbage collection threshold value.")
(defconst init-file/gc-cons-percentage 0.1 "Preferred garbage collection percentage value.")

;; Define some functions for deferring and restoring Emacs' garbage collection facilities.
(defun defer-garbage-collection ()
  "Set the garbage collection threshold to the highest possible for collection avoidance."
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6))
(defun restore-garbage-collection ()
  "Restore the garbage collection threshold parameters in a deferred fashion."
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold init-file/gc-cons-threshold
                          gc-cons-percentage init-file/gc-cons-percentage))))

;; Defer garbage collection while Emacs is starting and restore the threshold to 8MB when we're done.
(defer-garbage-collection)
(add-hook 'emacs-startup-hook #'restore-garbage-collection)

;; Similarly raise and restore the garbage collection threshold for minibuffer commands.
(add-hook 'minibuffer-setup-hook #'defer-garbage-collection)
(add-hook 'minibuffer-exit-hook #'restore-garbage-collection)

;; Collect all garbage whenever the focus changes to/from Emacs.
(if (>= emacs-major-version 27)
    (add-function :after after-focus-change-function #'garbage-collect)
  (add-hook 'focus-out-hook #'garbage-collect))

;;====================
;; Variables/Basic Config.
;;====================

;; Basic variable configuration.
(setq-default
 initial-scratch-message ""          ; Remove initial message
 frame-title-format '("Emacs - %b")  ; Window title formatting
 truncate-lines t                    ; Truncate lines instead of wrapping
 message-truncate-lines t            ; Truncate messages in the echo area
 inhibit-startup-screen t            ; Don't show startup screen
 inhibit-splash-screen t             ; Don't show splash screen
 x-gtk-use-system-tooltips nil       ; Don't use system tooltips
 mouse-wheel-progressive-speed nil   ; Don't accelerate mouse scrolling
 scroll-preserve-screen-position 1   ; Don't move cursor while scrolling
 scroll-conservatively 101           ; Only scroll one line at a time
 scroll-margin 5                     ; Maintain a margin of 5 lines while scrolling
 )

;; Set backup behavior.
(setq-default
 backup-by-copying t    ; Don't delink hardlinks
 version-control t      ; Use version numbers on backups
 delete-old-versions t  ; Do not keep old backups
 kept-new-versions 5    ; Keep 5 new versions
 kept-old-versions 3    ; Keep 3 old versions
 )

;; Configure user directory and file locations.
(defconst custom-backup-dir (concat user-emacs-directory "backups") "Preferred backup directory.")
(setq-default
 custom-file (concat user-emacs-directory "custom.el")          ; Use separate custom-vars file
 backup-directory-alist `((".*" . ,custom-backup-dir))          ; Set backup file directory
 auto-save-file-name-transforms `((".*" ,custom-backup-dir t))  ; Set autosave file directory
 )

;; Enable uniquify for better unique buffer names.
(require 'uniquify)
(setq
 uniquify-buffer-name-style 'forward  ; Show directory name before buffer name
 uniquify-separator "/"               ; Use a forward slash separator
 uniquify-after-kill-buffer-p t       ; Update buffer names after killing
 uniquify-ignore-buffers-re "^\\*"    ; Ignore special buffers
 )

;; Set the default styling rules to use.
(setq-default
 indent-tabs-mode nil   ; Don't use tabs for indentation
 tab-width 4            ; Use 4 spaces for default by indentation
 c-basic-offset 4       ; Same as above, but for C-like languages
 c-default-style "bsd"  ; Use BSD-style default styling rules for C-like languages
 )

;; (OS-specific) Set the default working directory.
(if (eq system-type 'windows-nt)
    (setq default-directory (getenv "USERPROFILE"))
  (setq default-directory "~/"))

;; Disable some unnecessary byte compilation warnings.
(setq byte-compile-warnings '(not
                              free-vars
                              unresolved
                              noruntime
                              lexical
                              make-local))

;; Set default buffer grouping in ibuffer
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Emacs" (or
                         (name . "^\\*.*\\*.*$")
                         (name . "^magit.*:.*$")))))))
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-switch-to-saved-filter-groups "default")))

;; Temporarily disable file handler checking during startup to save time.
(defvar temp--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook (lambda () (setq file-name-handler-alist temp--file-name-handler-alist)))

;; Add a hook to trailing whitespaces before saving a file.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;====================
;; Misc. Mode Config
;;====================

;; Enable Modes
(mapc (lambda (mode) (funcall mode 1))
      '(
        global-subword-mode      ; Treats camel-case names as multiple words
        global-auto-revert-mode  ; Automatically revert buffers on file changes
        global-hl-line-mode      ; Highlight the currently-selected line
        ido-mode                 ; Better directory and buffer completion
        column-number-mode       ; Show column number in the mode line
        show-paren-mode          ; Highlight matching parenthesis
        size-indication-mode     ; Show buffer size in the mode line
        ))

;; Disable Modes
(mapc (lambda (mode) (funcall mode 0))
      `(
        menu-bar-mode  ; Disable the menu bar
        ))

;; Hooked Modes
(add-hook 'prog-mode-hook 'electric-pair-local-mode)  ; Automatic delimiter pairing

;; Mode Configuration
(setq
 ido-max-prospects 8              ; (ido-mode) - Limit max on-screen matches to 8 items
 ido-max-window-height 10         ; (ido-mode) - Limit max minibuffer height to 10 lines
 show-paren-delay 0.0             ; (show-paren-mode) - Parenthesis highlighting delay
 visual-line-fringe-indicators t  ; (visual-line-mode) - Shows fringe indicators for wrapping
 )

;;====================
;; New Frame Config
;;====================

;; Define a function that will configure new Emacs frames.
(defvar init-file/theme-loaded nil "Whether or not the theme has been loaded yet.")
(defun configure-new-frame (frame)
  "Configure new frame FRAME."
  (when (display-graphic-p frame)
    (select-frame frame)
    (let ((winid (frame-parameter frame 'outer-window-id)))
      (call-process-shell-command
       (concat "xprop -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT dark -id " winid)))
    (tool-bar-mode 0)
    (scroll-bar-mode 0)
    (when (and
           (daemonp)
           (not init-file/theme-loaded))
      (load-theme 'mood-one t)
      (setq init-file/theme-loaded t))))

;; Define a function that will inhibit startup messages for new Emacs frames.
(defun inhibit-new-frame-message ()
  "Inhibit startup messages in new frames."
  (setq inhibit-message t)
  (run-with-idle-timer 0 nil (lambda () (setq inhibit-message nil))))

;; Hook the frame configuration function into all newly-created frames.
(add-hook 'after-make-frame-functions #'configure-new-frame)

;; Hook the startup message inhibition function into all newly-created frames.
(add-hook 'server-after-make-frame-hook #'inhibit-new-frame-message)

;; Run for any already-existing frames
(mapc 'configure-new-frame (frame-list))

;;====================
;; Package Manager
;;====================

;; Temporary fix for bugged package archive retrieval in Emacs 26.2.
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Configure the package manager.
(eval-and-compile

  ;; Configure the package manager and use-package settings.
  (setq load-prefer-newer t
        package-user-dir (concat user-emacs-directory "elpa")
        package-check-signature nil
        package-enable-at-startup nil
        package--init-file-ensured t
        use-package-always-ensure t
        use-package-always-defer t)

  ;; Make sure that the package directory exists to prevent errors.
  (unless (file-directory-p package-user-dir)
    (make-directory package-user-dir t))

  ;; Manually assemble the load-path during startup to save time.
  (setq load-path (append load-path (directory-files package-user-dir t "^[^.]" t))))

;; Initialize the package management system (only at compile time).
(eval-when-compile

  ;; Require the package manager.
  (require 'package)

  ;; Enable the MELPA repository.
  (unless (assoc-default "melpa" package-archives)
    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))

  ;; Initialize the package manager.
  (package-initialize)
  (package-refresh-contents)

  ;; Check for use-package. Install it if not already present.
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))
  (require 'use-package))

;;====================
;; Key Bindings
;;====================

;; Custom Bindings:
;; [ F6 ]               -> Toggle line-wrapping
;; [ F7 ]               -> Toggle linum-mode/display-line-numbers-mode
;; [ F10 ]              -> (Overwritten) Open the menubar in a minibuffer
;; [ C-z ]              -> (Overwritten) Undo (and don't suspend, thanks)
;; [ C-c o ]            -> Focus on minibuffer window
;; [ C-c r ]            -> Revert buffer without confirmation
;; [ C-c s ]            -> Open scratch buffer in the current window
;; [ C-c RET ]          -> Open terminal in the current window
;; [ C-x C-b ]          -> (Overwritten) Invoke ibuffer
;; [ M-n / M-p ]        -> Scroll up/down by one line
;; [ C-M-n / C-M-p ]    -> Move forward/back by one paragraph
;; [ C-c C-<i,j,k,l> ]  -> Focus on the window in <direction>

;; Require bind-key. (Bundled with use-package)
(require 'bind-key)

(defun switch-to-minibuffer-window ()
  "Switch to the minibuffer window (if active)."
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(defun revert-buffer-no-confirm ()
  "Revert the current buffer without confirmation."
  (interactive)
  (if (not (buffer-modified-p))
      (revert-buffer :ignore-auto :noconfirm)
    (when (yes-or-no-p "The contents of this buffer have been modified. Really revert? ")
      (revert-buffer :ignore-auto :noconfirm))))

(defun open-scratch-buffer ()
  "Open the scratch buffer, (re)creating it if not present."
  (interactive)
  (if (get-buffer "*scratch*")
      (switch-to-buffer "*scratch*")
    (progn
      (switch-to-buffer (get-buffer-create "*scratch*"))
      (lisp-interaction-mode))))

(defun open-default-shell ()
  "Opens the default system shell in a terminal emulator."
  (interactive)
  (term shell-file-name))

;; Set up all custom bindings (non-package-specific)
(bind-keys ([f6] . visual-line-mode)
           ([f7] . display-line-numbers-mode)
           ([f10] . tmm-menubar)
           ("C-z" . undo)
           ("C-c o" . switch-to-minibuffer-window)
           ("C-c r" . revert-buffer-no-confirm)
           ("C-c s" . open-scratch-buffer)
           ("C-c RET" . open-default-shell)
           ("C-x C-b" . ibuffer)
           ("M-n" . scroll-up-line)
           ("M-p" . scroll-down-line)
           ("C-M-n" . forward-paragraph)
           ("C-M-p" . backward-paragraph))
(bind-keys* ("C-c C-i" . windmove-up)
            ("C-c C-k" . windmove-down)
            ("C-c C-j" . windmove-left)
            ("C-c C-l" . windmove-right))

;;====================
;; Init File
;;====================

(defun init-file/open-init-file ()
  "Opens the init file in a buffer."
  (interactive)
  (find-file user-init-file))

(defun init-file/download-latest-init-file ()
  "Download the latest init file from jessieh.net."
  (interactive)
  (when (yes-or-no-p "Download latest init file from jessieh.net? ")
    (message "Updating init file...")
    (url-copy-file "https://jessieh.net/emacs" (concat user-emacs-directory "init.el") t)
    (init-file/byte-compile-init-file)))

(defun init-file/byte-compile-init-file ()
  "Byte compile the init file."
  (interactive)
  (save-restriction
    (message "Byte-compiling init file...")
    (byte-compile-file (concat user-emacs-directory "init.el"))))

(defun init-file/refresh-packages ()
  "Refresh all packages that have been configured for use in the init file."
  (interactive)
  (when (yes-or-no-p "Redownload and refresh packages? ")
    (message "Refreshing packages...")
    (delete-directory package-user-dir t)
    (init-file/byte-compile-init-file)))

;; Make sure that this init file is byte-compiled whenever it changes.
(if (file-newer-than-file-p
     (concat user-emacs-directory "init.el")
     (concat user-emacs-directory "init.elc"))
    (add-hook 'emacs-startup-hook 'init-file/byte-compile-init-file))

;;========================================
;;
;; THEME CONFIGURATION
;;
;;========================================

;;====================
;; Default Face
;;====================

;; Set up styling for the default face.
(set-face-attribute 'default nil
                    :family "Roboto Mono"
                    :height 90)

;;====================
;; Theme
;;====================

;; Load "mood-one"
(use-package mood-one-theme
  :after
  (solaire-mode)
  :demand
  t
  :config
  (setq diff-hl-fringe-bmp-function #'mood-one-theme-diff-hl-fringe-bmp-function)
  (eval-after-load 'flycheck #'mood-one-theme-flycheck-fringe-bmp-enable)
  (eval-after-load 'flymake #'mood-one-theme-flymake-fringe-bmp-enable)
  (eval-after-load 'neotree #'mood-one-theme-neotree-configuration-enable)
  (load-theme 'mood-one t))

;;====================
;; Mode-Line
;;====================

;; Load "mood-line"
(use-package mood-line
  :demand
  t
  :config
  (mood-line-mode))

;;====================
;; Solaire-Mode
;;====================

;; Load "solaire-mode"
(use-package solaire-mode
  :demand
  t
  :custom
  (solaire-mode-themes-to-face-swap '("mood-one"))
  (solaire-mode-remap-modeline nil)
  (solaire-mode-real-buffer-fn
   (lambda ()
     (and (not (string-equal major-mode "neotree-mode"))
          (buffer-name (buffer-base-buffer))
          (not (string-match "\*Echo Area" (buffer-name (buffer-base-buffer)))))))
  :hook
  (change-major-mode-hook . turn-on-solaire-mode)
  (term-mode . (lambda () (set-face-background 'term (face-background 'solaire-default-face))))
  :config
  (solaire-global-mode t))

;;========================================
;;
;; EXTERNAL PACKAGE CONFIGURATION
;;
;;========================================

;;====================
;; Language Modes
;;====================

;; Currently Supported:
;; Lua, PHP, Rust, Fish, C#, Dart, Kotlin, GDScript

;; Load Lua Mode
;; (Associated files: .lua)
(use-package lua-mode
  :mode
  ("\\.lua\\'" . lua-mode))

;; Load Web Mode
;; (Associated files: .php, .html)
;; (In js-mode: .ts)
;; (In js-jsx-mode: .jsx, .tsx)
(use-package web-mode
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-enable-auto-quoting nil)
  :mode
  (("\\.php\\'" . web-mode)
   ("\\.html\\'" . web-mode)
   ("\\.ts\\'" . js-mode)
   ("\\.[jt]sx\\'" . js-jsx-mode)))

;; Load JSON Mode
;; (Associated files: .json)
(use-package json-mode
  :mode
  ("\\.json\\'" . json-mode))

;; Load Rust Mode
;; (Associated files: .rs)
(use-package rust-mode
  :mode
  ("\\.rs\\'" . rust-mode))

;; Load Fish Mode
;; (Associated files: .fish)
(use-package fish-mode
  :mode
  ("\\.fish\\'" . fish-mode))

;; Load C Sharp Mode
;; (Associated files: .cs)
(use-package csharp-mode
  :mode
  ("\\.cs\\'" . csharp-mode))

;; Load Dart Mode
;; (Associated files: .dart)
(use-package dart-mode
  :mode
  ("\\.dart\\'" . dart-mode))

;; Load Kotlin Mode
;; (Associated files: .kt)
(use-package kotlin-mode
  :mode
  ("\\.kt\\'" . kotlin-mode))

;; Load GDScript Mode
;; (Associated files: .gd, .tscn)
(use-package gdscript-mode
  :mode
  ("\\.gd\\'" . gdscript-mode)
  ("\\.tscn\\'" . gdscript-mode))

;;====================
;; LSP Mode (Language Server Support)
;;====================

;; Load LSP Mode
;; Associated languages: HTML, JavaScript, JSON, GDScript, Rust
(use-package lsp-mode
  :hook
  (web-mode . lsp-deferred)
  (js-mode . lsp-deferred)
  (css-mode . lsp-deferred)
  (json-mode . lsp-deferred)
  (gdscript-mode . lsp-deferred)
  (rust-mode . lsp-deferred)
  :custom
  (lsp-signature-auto-activate nil)
  (lsp-signature-render-documentation nil)
  (lsp-eldoc-hook nil)
  :commands
  (lsp
   lsp-deferred))

;; Load LSP UI
(use-package lsp-ui
  :custom
  (lsp-ui-imenu-enable nil)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-always-show t)
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-max-width 50)
  (lsp-ui-doc-max-height 10)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-alignment 'window)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  :config
  (setq lsp-ui-doc-border (face-background 'lsp-ui-doc-background))
  :bind
  (:map lsp-ui-mode-map
   ("M-," . lsp-ui-doc-mode)
   ("M-." . lsp-ui-peek-find-definitions)
   ("M-?" . lsp-ui-peek-find-references)))

;;====================
;; Package-Lint (Elisp Package Linter)
;;====================

;; Load Package-Lint
(use-package package-lint
  :commands
  (package-lint-current-buffer))

;;====================
;; FlyCheck (Syntax Checker)
;;====================

;; Bindings
;; [ C-c e ] -> Open error list window

;; Load FlyCheck
(use-package flycheck
  :hook
  (prog-mode . flycheck-mode)
  (c++-mode-hook . (lambda () (setq flycheck-clang-standard "c++17")))
  :config
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))
  :custom
  (flycheck-python-flake8-executable "flake8")
  (flycheck-python-pylint-executable "pylint")
  (flycheck-python-mypy-executable "mypy")
  :bind
  ("C-c e" . flycheck-list-errors))

;; [Elisp Packages]
;; Load FlyCheck-Package
(use-package flycheck-package
  :after
  (flycheck)
  :hook
  (flycheck-mode-hook . flycheck-package-setup))

;;====================
;; Company (Autocompletion)
;;====================

;; Load Company
(use-package company
  :defer
  2
  :config
  (global-company-mode)
  :custom
  (company-idle-delay 0.3)
  (company-dabbrev-downcase nil))

;; [Lua]
;; Load Company-Lua
(use-package company-lua
  :after
  (company)
  :hook
  (lua-mode my-lua-mode-company-init))

;; [PHP]
;; Load Company-PHP
(use-package company-php
  :after
  (company))

;;====================
;; yasnippet (Snippet Insertion/Completion)
;;====================

;; Load yasnippet
(use-package yasnippet
  :demand
  t
  :custom
  (yas-also-auto-indent-first-line t)
  (yas-also-indent-empty-lines t)
  :config
  (yas-global-mode t))

;;====================
;; Projectile (Project Interaction)
;;====================

;; Bindings:
;; [ C-c p ... ] -> Projectile key map prefix

;; Load Projectile
(use-package projectile
  :demand
  t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode t))

;;====================
;; Smex (M-x Autocompletion)
;;====================

;; Bindings:
;; [ M-x ] (Overwritten) -> Open Smex

;; Load Smex
(use-package smex
  :config
  (smex-initialize)
  :custom
  (smex-prompt-string "Cmd: ")
  :bind
  ("M-x" . smex))

;;====================
;; Undo Tree
;;====================

;; Bindings:
;; [ C-x u ] (Overwritten) -> Open Undo Tree

;; Load Undo Tree
(use-package undo-tree
  :demand
  t
  :config
  (global-undo-tree-mode))

;;====================
;; Diff-HL (VCS Diff Highlighting)
;;====================

;; Load Diff-HL
(use-package diff-hl
  :demand
  t
  :hook
  (diff-hl-mode-hook . diff-hl-flydiff-mode)
  :config
  (global-diff-hl-mode))

;;====================
;; Neotree (File Browser)
;;====================

;; Bindings:
;; [ C-c w ] -> (Overwritten) Do nothing (neotree-mode)
;; [ F5 ]    -> (Overwritten) Do nothing (neotree-mode)
;; [ F6 ]    -> (Overwritten) Do nothing (neotree-mode)
;; [ F7 ]    -> (Overwritten) Do nothing (neotree-mode)
;; [ F8 ]    -> Toggle Neotree window
;; [ C-c t ] -> Switch to Neotree window

;; Load Neotree
(use-package neotree
  :custom
  (neo-confirm-create-file #'off-p)
  (neo-confirm-create-directory #'off-p)
  (neo-show-updir-line nil)
  (neo-smart-open t)
  (neo-window-width 30)
  (neo-theme 'nerd)
  :config
  (defun neotree--setup (&rest _)
    (make-local-variable 'auto-hscroll-mode)
    (setq mode-line-format nil
          line-spacing 3
          truncate-lines t
          word-wrap nil
          auto-hscroll-mode nil))
  (add-hook 'neo-after-create-hook #'neotree--setup)
  :bind
  (([f8] . neotree-toggle)
   ("C-c t" . neotree)
   :map neotree-mode-map
   ("C-c w" . (lambda () (interactive) nil))
   ([f5] . (lambda () (interactive) nil))
   ([f6] . (lambda () (interactive) nil))
   ([f7] . (lambda () (interactive) nil))))

;;====================
;; Resize-Window (Resizing)
;;====================

;; Bindings:
;; [ C-c w ] -> Toggle window sizing mode

;; Load Resize-Window
(use-package resize-window
  :bind
  ("C-c w" . resize-window))

;;====================
;; Writeroom-Mode
;;====================

;; Bindings:
;; [ F5 ] -> Toggle
;; [ F6 ] -> (Overwritten) Decrease writeroom width (writeroom-mode)
;; [ F7 ] -> (Overwritten) Increase writeroom width (writeroom-mode)

;; Load Writeroom-Mode
(use-package writeroom-mode
  :custom
  (writeroom-global-effects nil)
  (writeroom-maximize-window nil)
  (writeroom-fringes-outside-margins nil)
  :bind
  (([f5] . writeroom-mode)
   :map writeroom-mode-map
   ([f6] . writeroom-decrease-width)
   ([f7] . writeroom-increase-width)))

;;====================
;; Multiple-Cursors
;;====================

;; Bindings:
;; [ C-> ] -> Mark next like this
;; [ C-< ] -> Mark previos like this
;; [ C-c C-> || C-c C-< ] -> Mark all like this

;; Load Multiple-Cursors
(use-package multiple-cursors
  :custom
  (mc/always-run-for-all nil)
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C->" . mc/mark-all-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

;;====================
;; Rainbow-Delimiters (Delimiter Highlighting)
;;====================

;; Load Rainbow-Delimiters
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;;====================
;; HL-Todo (TODO/FIXME Highlighting)
;;====================

;; Load HL-Todo
(use-package hl-todo
  :custom
  (hl-todo-keyword-faces
   '(("TODO" . ,(face-foreground 'hl-todo))
     ("DEBUG" . ,(face-foreground 'hl-todo))
     ("FIXME" . ,(face-foreground 'hl-todo))
     ("HACK" . ,(face-foreground 'hl-todo))))
  :hook
  (prog-mode . hl-todo-mode))

;;====================
;; Magit (Git Interface)
;;====================

;; Bindings:
;; [ C-c g ] -> Open Magit window

;; Load Magit
(use-package magit
  :bind
  ("C-c g" . magit-status)
  :custom
  (magit-completing-read-function 'magit-ido-completing-read))

;; [TODO Listing]
;; Load Magit-Todos
(use-package magit-todos
  :hook
  (magit-mode-hook))

;;====================
;; Crm-Custom (Additional Ido Compat.)
;;====================

;; Load Crm-Custom
(use-package crm-custom
  :demand
  t
  :config
  (crm-custom-mode 1))

;;====================
;; Ido-Completing-Read+ (Ido Everywhere)
;;====================

;; Load Ido-Completing-Read+
(use-package ido-completing-read+
  :demand
  t
  :config
  (ido-ubiquitous-mode t))

;;====================
;; Ido-Vertical-Mode (Vertical Completions)
;;====================

;; Load Ido-Vertical-Mode
(use-package ido-vertical-mode
  :demand
  t
  :config
  (ido-vertical-mode t)
  :custom
  (ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
  (ido-vertical-indicator " →"))

;;====================
;; Flx-Ido (Ido Fuzzy Matching)
;;====================

;; Load Flx-Ido
(use-package flx-ido
  :demand
  t
  :config
  (flx-ido-mode t))

;;====================
;; Swiper (Inline Search w/ Overview)
;;====================

(use-package swiper
  :demand
  t
  :custom
  (ivy-dynamic-exhibit-delay-ms 250)
  (ivy-count-format "")
  (swiper-include-line-number-in-search t)
  :bind
  ("C-s" . swiper)
  ("C-r" . swiper-backward))

;;====================
;; Anzu (Search Mode Info Display)
;;====================

;; Load Anzu
(use-package anzu
  :demand
  t
  :custom
  (anzu-cons-mode-line-p nil)
  :config
  (global-anzu-mode t)
  :bind
  (("<remap> <query-replace>" . 'anzu-query-replace)
   ("<remap> <query-replace-regexp>" . 'anzu-query-replace-regexp)))

;;========================================
;;
;; END OF INIT FILE
;;
;;========================================

;;; init.el ends here
