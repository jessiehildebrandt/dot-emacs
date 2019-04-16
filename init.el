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

;; Disable some unnecessary byte-compiler warnings.
(setq byte-compile-warnings '(not
                              free-vars
                              unresolved
                              noruntime
                              lexical
                              make-local))

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
;; [ F6 ]                -> Toggle line-wrapping
;; [ F7 ]                -> Toggle linum-mode/display-line-numbers-mode
;; [ F10 ]               -> (Overwritten) Open the menubar in a minibuffer
;; [ C-z ]               -> (Overwritten) Undo
;; [ C-x C-b ]           -> (Overwritten) Invoke ibuffer
;; [ C-c C-o ]           -> Focus on minibuffer window
;; [ C-c C-r ]           -> Revert buffer without confirmation
;; [ C-x RET ]           -> Open eshell in the current buffer
;; [ C-c C-<i,j,k,l> ]   -> Focus on the window in <direction>
;; [ M-n / M-p ]         -> Scroll up/down by one line
;; [ C-M-n / C-M-p ]     -> Move forward/back by one paragraph

;; Require bind-key. (Bundled with use-package)
(require 'bind-key)

;; Bind a key to toggle line wrapping behavior.
(bind-key [f6] 'visual-line-mode)

;; Bind a key to show line numbers.
(bind-key [f7] 'linum-mode)
(when (>= emacs-major-version 26)
  (bind-key [f7] 'display-line-numbers-mode))

;; Replace the menu-bar-open keybind with a tmm-menubar keybind.
(bind-key [f10] 'tmm-menubar)

;; Replace the suspend-emacs keybind with a (much less annoying) undo keybind.
(bind-key "C-z" 'undo)

;; Replace the list-buffers keybind with an ibuffer keybind.
(bind-key "C-x C-b" 'ibuffer)

;; Bind a key to switch to the minibuffer.
(defun switch-to-minibuffer-window ()
  "Switch to the minibuffer window (if active)."
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))
(bind-key "C-c C-o" 'switch-to-minibuffer-window)

;; Bind a key to revert the buffer without confirmation.
(defun revert-buffer-no-confirm ()
  "Revert the current buffer without confirmation."
  (interactive)
  (if (not (buffer-modified-p))
      (revert-buffer :ignore-auto :noconfirm)
    (when (yes-or-no-p "The contents of this buffer have been modified. Really revert? ")
      (revert-buffer :ignore-auto :noconfirm))))
(bind-key "C-c C-r" 'revert-buffer-no-confirm)

;; Bind a key to open up eshell.
(bind-key "C-x RET" 'eshell)

;; Bind keys to switch windows easier.
(bind-key* "C-c C-i" 'windmove-up)
(bind-key* "C-c C-k" 'windmove-down)
(bind-key* "C-c C-j" 'windmove-left)
(bind-key* "C-c C-l" 'windmove-right)

;; Bind keys to scroll up/down by one line
(bind-key "M-n" 'scroll-up-line)
(bind-key "M-p" 'scroll-down-line)

;; Bind keys to move forward/back by one paragraph
(bind-key "C-M-n" 'forward-paragraph)
(bind-key "C-M-p" 'backward-paragraph)

;;====================
;; Init File
;;====================

(defun update-init-file ()
  "Download the latest init file from jessieh.net."
  (interactive)
  (when (yes-or-no-p "Download latest init file from jessieh.net? ")
    (message "Updating init file...")
    (url-copy-file "https://jessieh.net/emacs" (concat user-emacs-directory "init.el") t)))

(defun byte-compile-init-file ()
  "Byte compile the init file."
  (interactive)

  (save-restriction
    (message "Byte-compiling init file...")
    (byte-compile-file (concat user-emacs-directory "init.el"))))

(defun refresh-init-file-packages ()
  "Redownload all packages that have been configured in the init file."
  (interactive)
  (when (yes-or-no-p "Redownload and reconfigure packages? ")
    (message "Refreshing packages...")
    (delete-directory package-user-dir t)
    (byte-compile-init-file)))

;; Make sure that this init file is byte-compiled whenever it changes.
(if (file-newer-than-file-p
     (concat user-emacs-directory "init.el")
     (concat user-emacs-directory "init.elc"))
    (add-hook 'emacs-startup-hook 'byte-compile-init-file))


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
   (mood-one-theme-enable-fringe-bmps))

;;====================
;; Mode-Line
;;====================

;; Load "mood-line"
(use-package mood-line
  :demand
  t
  :config
  (mood-line-activate))


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

;;====================
;; Exec-Path-From-Shell (PATH Setting)
;;====================

;; Load Exec-Path-From-Shell (Non-Windows systems only)
(unless (string-equal system-type "windows-nt")
  (use-package exec-path-from-shell
    :demand
    t
    :custom
    (exec-path-from-shell-arguments '("-l"))
    :config
    (add-to-list 'exec-path-from-shell-variables '"RUST_SRC_PATH")
    (exec-path-from-shell-initialize)))

;;====================
;; Racer (Rust Completion)
;;====================

;; Load Racer
(use-package racer
  :custom
  (racer-rust-src-path nil)
  :hook
  (rust-mode . racer-mode))

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
  :demand
  t
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

;; [Rust]
;; Load Company-Racer
(use-package company-racer
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
                        'javascript
                        'cperl 'python
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
          tab-width 1
          truncate-lines t
          word-wrap nil
          auto-hscroll-mode nil)
    )
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
  ("C-c g" . magit-status))

;; [TODO Listing]
;; Load Magit-Todos
(use-package magit-todos
  :hook
  (magit-mode-hook))

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
