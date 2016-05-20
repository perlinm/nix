;; -------------------------------------------------------------------------------------
;; Package declaration and configuration
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-refresh-contents)
(package-initialize)

(use-package color-theme
  :config
  (color-theme-initialize)
  (color-theme-hober)
  :ensure t)
(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)
  :ensure t)
(use-package linum-relative
  :config
  (linum-relative-global-mode t)
  :ensure t)

(use-package helm
  :config
  (helm-mode t)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (helm-autoresize-mode 1)
  :ensure t)

(use-package tex-site
  :ensure auctex)
(use-package haskell-mode
  :ensure t)
(use-package rust-mode
  :ensure t)
(use-package cargo
  :ensure t)
(use-package markdown-mode
  :ensure t)

(use-package f
  :ensure t)
(use-package company
  :config
  (global-company-mode)
  (setq company-tooltip-align-annotations t)
  (global-set-key (kbd "<C-tab>") 'company-indent-or-complete-common)
  :ensure t)
(use-package company-ycmd
  :config
  (company-ycmd-setup)
  :ensure t)
(use-package ycmd
  :config
  (add-hook 'after-init-hook #'global-ycmd-mode)
  (set-variable 'ycmd-server-command
                '("python2" "/usr/share/vim/vimfiles/third_party/ycmd/ycmd"))
  (set-variable 'ycmd-global-config "~/.ycmd_settings.json")
 :ensure t)

;; -------------------------------------------------------------------------------------
;; Config options

;; aesthetics
(set-frame-parameter (selected-frame) 'alpha '(75 75)) ;; background transparency
(add-to-list 'default-frame-alist '(alpha 75 75)) ;; background transparency
(add-to-list 'default-frame-alist '(foreground-color . "grey85")) ;; font color

;; emacs window/client modifications
(setq line-number-mode t) ;; show line number at cursor
(setq column-number-mode t) ;; show column number at cursor
(tool-bar-mode -1) ;; disable tool bar
(menu-bar-mode -1) ;; disable menu bar
(scroll-bar-mode -1) ;; disable scroll bar
(setq inhibit-startup-message t) ;; inhibit startup messages
(global-auto-revert-mode) ;; refresh buffers when files are modified
(setq x-select-enable-clipboard t) ;; make emacs cooperate with system clipboard

;; default to text mode with spell checking
(setq-default major-mode 'text-mode)
(dolist (hook '(text-mode-hook))
	(add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
	(add-hook hook (lambda () (flyspell-mode -1))))

;; cursor options
(blink-cursor-mode 0)
(setq visible-bell t)
(setq-default fill-column 90)
(setq-default show-trailing-whitespace t)

;; indentation
(setq-default indent-tabs-mode nil)
(setq-default electric-indet nil)
(setq indent-line-function 'insert-tab)
(setq-default tab-width 2)
(setq cperl-indent-level 2)
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; scrolling settings
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
(setq mouse-wheel-progressive-speed nil)

;; highlight parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)
(require 'paren)
(set-face-background 'show-paren-match-face "#333333")
(set-face-foreground 'show-paren-match-face "#ffffff")
(set-face-attribute 'show-paren-match-face nil :weight 'black)

;; show offscreen parenthesis in minibuffer
(defadvice show-paren-function
  (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
         (matching-text (and cb
                             (char-equal (char-syntax cb) ?\) )
                             (blink-matching-open))))
    (when matching-text (message matching-text))))

;; -------------------------------------------------------------------------------------
;; TeX options

;; general latex hooks
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook (lambda () (set-fill-column 70)))
(add-hook 'bibtex-mode-hook (lambda () (set-fill-column 70)))

; enable auto-fill mode; nice for text formatting
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; compile documents via latexmk
(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
      :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'LaTeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

;; compile latex documents to pdf when not using latexmk
(setq TeX-PDF-mode t)

;; use okular as the pdf viewer
(setq TeX-view-program-selection
 '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
 '(("PDF Viewer" "okular %o#src:%n%b")))

;; run emacs in server mode to use emacsclient with Okular
(add-hook 'LaTeX-mode-hook 'server-start)

;; display errors properly when editing a multi-file tex project using auctex
(defadvice TeX-parse-reset (after make-master-file-default () activate)
  (push (concat (substring (buffer-name) 1 (- (length (buffer-name)) 8))
                "." TeX-default-extension) TeX-error-file)
  (push nil TeX-error-offset))

;; syntax highlighting in auctex
(custom-set-variables
 '(TeX-insert-braces nil)
 '(custom-safe-themes (quote ("843a82ff3b91bec5430f9acdd11de03fc0f7874b15c1b6fbb965116b4c7bf830" "b90d367096824d5b69b59e606c6260be55d378b58e0d03ff8866e0b3d0da1c1b" "c3b86220873ba8ec54e0988673b87ea7d11301799eca74ccf7e84cce286ec9cd" "fa14373656d9c9e86f15dcced71f42b0cd99ea13e12a66cf9eb2625097c75d02" default)))
 '(font-latex-math-environments (quote ("display" "displaymath" "equation" "eqnarray" "gather" "multline" "align" "alignat" "xalignat" "dmath"))))

;; suppress automatic labelling of new environments
(eval-after-load "latex" '(progn (defun LaTeX-label (env))))

;; -------------------------------------------------------------------------------------
;; File modes

;; systemd
(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.timer\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.target\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.mount\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.automount\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.slice\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.socket\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.path\\'" . conf-unix-mode))

;; markdown
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; python - scons and sage
(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))
(add-to-list 'auto-mode-alist '("\\.sage\\'" . python-mode))

;; -------------------------------------------------------------------------------------
;; Miscellaneous functions and commands

;; kill all buffers other than the current one
(defun only-current-buffer ()
  (interactive)
    (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))
(put 'upcase-region 'disabled nil)

;; exactly what it counds like
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

;; -------------------------------------------------------------------------------------
;; Key bindings

(cua-mode t) ;; use standard copy/paste commands
(global-unset-key (kbd "C-x C-q")) ;; unset key binding for read-only mode

;; define custom key bindings map
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

(define-key my-keys-minor-mode-map (kbd "M-n") 'backward-char)
(define-key my-keys-minor-mode-map (kbd "M-i") 'forward-char)
(define-key my-keys-minor-mode-map (kbd "M-u") 'previous-line)
(define-key my-keys-minor-mode-map (kbd "M-e") 'next-line)

(define-key my-keys-minor-mode-map (kbd "M-C-n") 'left-word)
(define-key my-keys-minor-mode-map (kbd "M-C-i") 'right-word)
(define-key my-keys-minor-mode-map (kbd "M-C-u") 'backward-paragraph)
(define-key my-keys-minor-mode-map (kbd "M-C-e") 'forward-paragraph)

(define-key my-keys-minor-mode-map (kbd "M-l") 'move-beginning-of-line)
(define-key my-keys-minor-mode-map (kbd "M-y") 'move-end-of-line)

(define-key my-keys-minor-mode-map (kbd "C-u") 'scroll-down-command)
(define-key my-keys-minor-mode-map (kbd "C-e") 'scroll-up-command)
(define-key my-keys-minor-mode-map (kbd "C-l") 'beginning-of-buffer)
(define-key my-keys-minor-mode-map (kbd "C-y") 'end-of-buffer)

(define-key my-keys-minor-mode-map (kbd "M-m") 'recenter)
(define-key my-keys-minor-mode-map (kbd "M-C-m") 'recenter)

(define-key my-keys-minor-mode-map (kbd "C-x i") 'next-buffer)
(define-key my-keys-minor-mode-map (kbd "C-x n") 'previous-buffer)

(define-key my-keys-minor-mode-map (kbd "M-r") 'comment-or-uncomment-region-or-line)

(define-key my-keys-minor-mode-map (kbd "C-f C-f") (lambda ()
                                                     (interactive) (revert-buffer t t t)
                                                     (message "buffer reverted")))

;; define minor mode which enables my custom key bindings
(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

;; enable minor mode with custom key bindings
(my-keys-minor-mode 1)

  ;; unset conflicting flyspell keybinding
(eval-after-load "flyspell" '(define-key flyspell-mode-map (kbd "M-C-i") nil))

;; make custom key bindings work in the minibuffer
(defun my-minibuffer-setup-hook ()
  (my-keys-minor-mode 1))
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)
