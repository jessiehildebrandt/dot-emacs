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
;; Custom Keybinds
;;====================

;; Custom Bindings:
;; [ F6 ]              -> Toggle line-wrapping
;; [ F7 ]              -> Toggle linum-mode
;; [ C-x C-b ]         -> (Overwritten) Invoke ibuffer
;; [ C-x RET ]         -> Open eshell in the current buffer
;; [ C-c <direction> ] -> Focus on the window in <direction>
;; [ M-n / M-p]        -> Scroll up/down by one line

;; Default but Useful:
;; [ M-g M-g || M-g g ] -> Go to line number
;; [ M-. ]              -> Go to definition
;; [ C-/ ]              -> Alt. undo binding

;; Bind a key to toggle line wrapping behavior.
(global-set-key [f6] 'toggle-truncate-lines)

;; Bind a key to show line numbers.
(global-set-key [f7] 'linum-mode)

;; Replace the list-buffers keybind to invoke ibuffer.
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Bind a key to open up eshell.
(global-set-key (kbd "C-x RET") 'eshell)

;; Bind keys to switch windows easier.
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)

;; Bind keys to scroll up/down by one line
(defun scroll-up-line-nm ()
  (interactive)
  (setq scroll-margin 0)
  (scroll-up-line)
  (setq scroll-margin 6))
(defun scroll-down-line-nm ()
  (interactive)
  (setq scroll-margin 0)
  (scroll-down-line)
  (setq scroll-margin 6))
(global-set-key (kbd "M-n") 'scroll-up-line-nm)
(global-set-key (kbd "M-p") 'scroll-down-line-nm)

;;====================
;; Variables/Basic Config.
;;====================

;; Basic variable configuration.
(setq
 frame-title-format '("Emacs@" system-name " - %b")  ; Window title formatting
 indicate-empty-lines t                              ; Highlight empty lines
 inhibit-startup-screen t                            ; Don't show startup screen
 inhibit-splash-screen t                             ; Don't show splash screen
 x-gtk-use-system-tooltips nil                       ; Don't use system tooltips
 mouse-wheel-progressive-speed nil                   ; Don't accelerate mouse scrolling
 custom-file "~/.emacs.d/custom.el"                  ; Use separate custom-vars file
 scroll-preserve-screen-position 1                   ; Don't move cursor while scrolling
 scroll-conservatively 100000                        ; Scroll one line at a time
 scroll-margin 6                                     ; Maintain a margin while scrolling
 )

;; Set backup behavior.
(setq
 backup-directory-alist '(("." . "~/.emacs.d/backup"))  ; Set backup file directory
 backup-by-copying t                                    ; Don't delink hardlinks
 version-control t                                      ; Use version numbers on backups
 delete-old-versions t                                  ; Do not keep old backups
 kept-new-versions 20                                   ; Keep 20 new versions
 kept-old-versions 5                                    ; Keep 5 old versions
 )

;; Enable uniquify for better unique buffer names.
(require 'uniquify)
(setq
 uniquify-buffer-name-style 'forward  ; Show directory name before buffer name
 uniquify-separator "/"               ; Use a forward slash separator
 uniquify-after-kill-buffer-p t       ; Update buffer names after killing
 uniquify-ignore-buffers-re "^\\*"    ; Ignore special buffers
 )

;; Set up face styling for the default face.
(set-face-attribute 'default nil
					:family "Terminus (TTF)"
					:height 90)

;; Set the default styling rules to use.
(setq-default
 tab-width 4
 c-basic-offset 4
 c-default-style "bsd"
 )

;; Add a hook to trailing whitespaces before saving a file.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;====================
;; Misc. Mode Config
;;====================

;; Enable Modes
(mapc (lambda (mode) (funcall mode 1))
      '(
		global-subword-mode      ; Treats camel-case names as multiple words
		global-visual-line-mode  ; Enables whole-word line wrapping
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

;; Mode Configuration
(setq
 show-paren-delay 0.0             ; (show-paren-mode) - Parenthesis highlighting delay
 visual-line-fringe-indicators t  ; (visual-line-mode) - Shows fringe indicators for wrapping
 )

;;====================
;; Graphical/Term Mode Config
;;====================

;; Turn off the toolbar and scroll bar when in graphical mode.
(if (display-graphic-p)
	(progn
	  (tool-bar-mode 0)
	  (scroll-bar-mode 0)
	  ))

;;====================
;; Package Manager
;;====================

;; Require the package manager.
(require 'package)
(setq package-enable-at-startup nil)
(setq package-check-signature nil)

;; Enable the MELPA repository.
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Initialize the package manager.
(package-initialize)

;; Check for use-package. Install it if not already present.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Require use-package.
(eval-when-compile
  (require 'use-package))

;;====================
;; Garbage Collector
;;====================

;; Increase the garbage collection threshold to 100MB for a faster startup time.
(setq-default gc-cons-threshold 100000000)

;; Restore it to 20MB after 5 seconds.
(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold 20000000)))


;;========================================
;;
;; EXTERNAL PACKAGE CONFIGURATION
;;
;;========================================

;;====================
;; Themes
;;====================

;; Load "Tomorrow Night"
(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (load-theme `sanityinc-tomorrow-night t))

;;====================
;; Language Modes
;;====================

;; Currently Supported:
;; Lua, PHP, Rust, Fish, OCaml

;; Load Lua Mode
;; (Associated files: .lua)
(use-package lua-mode
  :ensure t
  :mode
  ("\\.lua\\'" . lua-mode))

;; Load PHP Mode
;; (Associated files: .php (HTML-Mode), .inc)
(use-package php-mode
  :ensure t
  :mode
  (("\\.php\\'" . html-mode)
   ("\\.inc\\'" . php-mode)))

;; Load Rust Mode
;; (Associated files: .rs)
(use-package rust-mode
  :ensure t
  :mode
  ("\\.rs\\'" . rust-mode)
  :config
  (set-face-attribute 'rust-string-interpolation-face nil
					  :slant 'normal
					  :foreground "red2"))

;; Load Fish Mode
;; (Associated files: .fish)
(use-package fish-mode
  :ensure t
  :mode
  ("\\.fish\\'" . fish-mode))

;; Load Tuareg (OCaml Mode)
(use-package tuareg
  :ensure t
  :mode
  ("\\.ml\\'" . tuareg-mode))

;;====================
;; Auto-Package-Update
;;====================

;; Interactive Functions:
;; (update-packages) -> Automatically update all packages

;; Load Auto-Package-Update
(use-package auto-package-update
  :ensure t
  :config
  (defun update-packages ()
    "Automatically update all installed packages."
    (interactive)
    (message "Updating packages...")
    (auto-package-update-now)))

;;====================
;; Exec-Path-From-Shell (PATH Setting)
;;====================

;; Bindings:
;; None

;; Load Exec-Path-From-Shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (progn
    (setq exec-path-from-shell-arguments '("-l"))
    (add-to-list 'exec-path-from-shell-variables '"RUST_SRC_PATH")
    (exec-path-from-shell-initialize)))

;;====================
;; Racer (Rust Completion)
;; ===================

;; Bindings:
;; None

;; Load Racer
(use-package racer
  :ensure t
  :config
  (progn
    (setq racer-rust-src-path nil)
    (add-hook 'rust-mode-hook #'racer-mode)))

;;====================
;; FlyCheck (Syntax Checker)
;;====================

;; Bindings
;; [ C-c e ] -> Open error list window

;; Load FlyCheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))
  :bind
  ("C-c e" . flycheck-list-errors))

;; [Rust]
;; Load FlyCheck-Rust
(use-package flycheck-rust
  :ensure t
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;;====================
;; Company (Autocompletion)
;;====================

;; Bindings:
;; None

;; Load Company
(use-package company
  :ensure t
  :init (global-company-mode)
  :config (setq company-idle-delay 0.1))

;; [Lua]
;; Load Company-Lua
(use-package company-lua
  :ensure t
  :config (add-hook 'lua-mode-hook #'my-lua-mode-company-init))

;; [Rust]
;; Load Company-Racer
(use-package company-racer
  :ensure t)

;;====================
;; Smex (M-x Autocompletion)
;;====================

;; Bindings:
;; [ M-x ] (Overwritten) -> Open Smex

;; Load Smex
(use-package smex
  :ensure t
  :config (smex-initialize)
  :bind
  ("M-x" . smex))

;;====================
;; Undo Tree
;;====================

;; Load Undo Tree
(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode))

;;====================
;; Treemacs (File Browser)
;;====================

;; Bindings:
;; [ F8 ]    -> Toggle Treemacs window
;; [ C-c t ] -> Switch to Treemacs window

;; Load Treemacs
(use-package treemacs
  :ensure t
  :config
  (progn
    (setq
     treemacs-is-never-other-window t
     treemacs-git-integration t
     treemacs-show-hidden-files nil
     )
    (treemacs-follow-mode t)
	(set-face-attribute 'treemacs-root-face nil
						:height 1.0
						:underline nil))
  :bind
  (([f8] . treemacs)
   ("C-c t" . treemacs-select-window)))

;;====================
;; Resize-Window (Resizing)
;;====================

;; Bindings:
;; [ C-c w ] -> Toggle window sizing mode

;; Load Resize-Window
(use-package resize-window
  :ensure t
  :bind
  ("C-c w" . resize-window))

;;====================
;; Multiple-Cursors
;;====================

;; Bindings:
;; [ C-> ] -> Mark next like this
;; [ C-< ] -> Mark previos like this
;; [ C-c C-> || C-c C-< ] -> Mark all like this

;; Load Multiple-Cursors
(use-package multiple-cursors
  :ensure t
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
  :ensure t
  :config
  (progn
	(add-hook 'html-mode-hook #'rainbow-mode)
	(add-hook 'css-mode-hook #'rainbow-mode)))

;;====================
;; Magit (Git Interface)
;;====================

;; Bindings:
;; [ C-c g ] -> Open Magit window

;; Load Magit
(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status))
