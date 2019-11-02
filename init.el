;;; init.el --- My custom Emacs configuration file. -*- lexical-binding: t; -*-

;;================================================================================
;;
;; Jessie Hildebrandt's
;; Run-Anywhere Emacs Config
;;
;;================================================================================


;;========================================
;;
;; EMACS CONFIGURATION
;;
;;========================================

;;====================
;; Garbage Collector
;;====================

;; Increase the garbage collection threshold to 100MB for a faster startup time.
(setq-default gc-cons-threshold 100000000
              gc-cons-percentage 0.6)

;; Restore it to 8MB after initialization is finished.
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold 8000000
                                               gc-cons-percentage 0.1)))

;; Collect all garbage whenever Emacs loses focus.
(add-hook 'focus-out-hook #'garbage-collect)

;;====================
;; Variables/Basic Config.
;;====================

;; Basic variable configuration.
(setq-default
 initial-scratch-message ""          ; Remove initial message
 frame-title-format '("Emacs - %b")  ; Window title formatting
 truncate-lines 1                    ; Truncate lines instead of wrapping
 inhibit-startup-screen t            ; Don't show startup screen
 inhibit-splash-screen t             ; Don't show splash screen
 x-gtk-use-system-tooltips nil       ; Don't use system tooltips
 mouse-wheel-progressive-speed nil   ; Don't accelerate mouse scrolling
 scroll-preserve-screen-position 1   ; Don't move cursor while scrolling
 custom-file "~/.emacs.d/custom.el"  ; Use separate custom-vars file
 )

;; Set backup behavior.
(setq-default
 backup-directory-alist '(("." . "~/.emacs.d/backup"))  ; Set backup file directory
 backup-by-copying t                                    ; Don't delink hardlinks
 version-control t                                      ; Use version numbers on backups
 delete-old-versions t                                  ; Do not keep old backups
 kept-new-versions 5                                    ; Keep 5 new versions
 kept-old-versions 3                                    ; Keep 3 old versions
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
 indent-tabs-mode nil
 tab-width 4
 c-basic-offset 4
 c-default-style "bsd"
 )

;; (OS-specific) Set the default working directory.
(if (or (eq system-type 'windows-nt)
        (eq system-type 'ms-dos))
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
 show-paren-delay 0.0             ; (show-paren-mode) - Parenthesis highlighting delay
 visual-line-fringe-indicators t  ; (visual-line-mode) - Shows fringe indicators for wrapping
 )

;;====================
;; Graphical Mode Config
;;====================

;; Define a function that will configure graphical Emacs frames.
(defun configure-graphic-frame (frame)
  "Set up graphical Emacs frame FRAME."
  (when (display-graphic-p)
    (select-frame frame)
    (setq winid (frame-parameter frame 'outer-window-id))
    (call-process-shell-command
     (concat "xprop -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT dark -id " winid))
    (tool-bar-mode 0)
    (scroll-bar-mode 0)))

;; Hook the frame configuration function into all newly-created graphical frames.
(when (display-graphic-p)
  (progn
    (add-hook 'window-setup-hook (lambda () (configure-graphic-frame (selected-frame))))
    (add-hook 'after-make-frame-functions #'configure-graphic-frame)
    (configure-graphic-frame (selected-frame))))

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
;; [ C-c o ]          -> Focus on minibuffer window
;; [ C-c r ]          -> Revert buffer without confirmation
;; [ C-x RET ]          -> Open eshell in the current buffer
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

;; Set up all custom bindings (non-package-specific)
(bind-keys ([f6] . visual-line-mode)
           ([f7] . display-line-numbers-mode)
           ([f10] . tmm-menubar)
           ("C-z" . undo)
           ("C-c o" . switch-to-minibuffer-window)
           ("C-c r" . revert-buffer-no-confirm)
           ("C-x RET" . eshell)
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
                    :family "Hack"
                    :height 90)

;;====================
;; Theme
;;====================

;; Load "mood-one"
(use-package mood-one-theme
  :demand
  t
  :config
  (setq diff-hl-fringe-bmp-function #'mood-one-theme-diff-hl-fringe-bmp-function)
  (eval-after-load 'flycheck #'mood-one-theme-flycheck-fringe-bmp-enable)
  (eval-after-load 'flymake #'mood-one-theme-flymake-fringe-bmp-enable)
  (eval-after-load 'neotree #'mood-one-theme-neotree-configuration-enable))

;;====================
;; Mode-Line
;;====================

;; Load "mood-line"
(use-package mood-line
  :demand
  t
  :config
  (mood-line-mode))


;;========================================
;;
;; EXTERNAL PACKAGE CONFIGURATION
;;
;;========================================

;;====================
;; Language Modes
;;====================

;; Currently Supported:
;; Lua, PHP, Rust, Fish

;; Load Lua Mode
;; (Associated files: .lua)
(use-package lua-mode
  :mode
  ("\\.lua\\'" . lua-mode))

;; Load PHP Mode
;; (Associated files: .php (HTML-Mode), .inc)
(use-package php-mode
  :mode
  (("\\.php\\'" . html-mode)
   ("\\.inc\\'" . php-mode)))

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
  ("\\.cs=='" . csharp-mode))

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
  :custom
  (flycheck-python-flake8-executable "flake8")
  (flycheck-python-pylint-executable "pylint")
  (flycheck-python-mypy-executable "mypy")
  :bind
  ("C-c e" . flycheck-list-errors))

;; [Rust]
;; Load FlyCheck-Rust
(use-package flycheck-rust
  :after
  (flycheck)
  :hook
  (flycheck-mode-hook . flycheck-rust-setup))

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
  (company-idle-delay 0.3))

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
;; Smart-Tabs-Mode (Superior Indentation Method)
;;====================

;; Load Smart-Tabs-Mode
(use-package smart-tabs-mode
  :demand
  t
  :config
  (smart-tabs-insinuate 'c 'c++ 'java
                        'javascript 'cperl
                        'ruby 'nxml))

;;====================
;; Smex (M-x Autocompletion)
;;====================

;; Bindings:
;; [ M-x ] (Overwritten) -> Open Smex

;; Load Smex
(use-package smex
  :config
  (smex-initialize)
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
;; [ F5 ]    -> (Overwritten) Do nothing (neotree-mode)
;; [ F6 ]    -> (Overwritten) Do nothing (neotree-mode)
;; [ F7 ]    -> (Overwritten) Do nothing (neotree-mode)
;; [ F8 ]    -> Toggle Neotree window
;; [ C-c t ] -> Switch to Neotree window

;; Load Neotree
(use-package neotree
  :custom
  (neo-show-updir-line nil)
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
;; Rainbow Mode (Colorize Colors)
;;====================

;; Load Rainbow-Mode
(use-package rainbow-mode
  :hook
  ((html-mode css-mode) . rainbow-mode))

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
  (ido-vertical-define-keys 'C-n-C-p-up-down-left-right))

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

;;====================
;; Smooth Scrolling
;;====================

;; Load Smooth-Scrolling
(use-package smooth-scrolling
  :demand
  t
  :custom
  (smooth-scroll-margin 6)
  :config
  (smooth-scrolling-mode t))


;;========================================
;;
;; END OF INIT FILE
;;
;;========================================

;;; init.el ends here
