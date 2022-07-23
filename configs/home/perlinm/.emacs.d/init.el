;; -------------------------------------------------------------------------------------
;; Package declaration and configuration
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-refresh-contents)
(package-initialize)

(leaf recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)
  :ensure t)
(leaf linum-relative
  :config
  (linum-relative-global-mode t)
  (setq linum-relative-current-symbol "")
  :ensure t)

(leaf helm
  :config
  (helm-mode t)
  (helm-autoresize-mode 1)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  :ensure t)
(leaf helm-ls-git
  :config
  (global-set-key (kbd "C-x C-d") 'helm-browse-project)
  :ensure t)

(leaf tex-site
  :ensure auctex)
(leaf haskell-mode
  :ensure t)
(leaf rust-mode
  :ensure t)
(leaf cargo
  :ensure t)
(leaf markdown-mode
  :ensure t)
(leaf multiple-cursors
  :ensure t)

(leaf adaptive-wrap
  :ensure t)

(leaf leaf-key)

(leaf color-theme-modern
  :ensure t)
(load-theme 'hober t t)
(enable-theme 'hober)

;; clean up litter such as "#*", ".#*", and "*~" files
(leaf no-littering
  :ensure t)
(require 'no-littering)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
(add-to-list 'recentf-exclude no-littering-var-directory)
(add-to-list 'recentf-exclude no-littering-etc-directory)

;; language server protocol
;; (leaf lsp-mode
;;   :ensure t)
;; (leaf helm-lsp
;;   :commands helm-lsp-workspace-symbol
;;   :ensure t)
;; (leaf python-mode
;;   :ensure t)
;; (add-hook 'python-mode-hook 'lsp)

;; -------------------------------------------------------------------------------------
;; Config options

;; mouse usage
(xterm-mouse-mode t)

;; aesthetics
(set-frame-parameter (selected-frame) 'alpha '(75 75)) ;; background transparency
(add-to-list 'default-frame-alist '(alpha 75 75)) ;; background transparency
(add-to-list 'default-frame-alist '(font . "Consolas-13")) ;; font
(add-to-list 'default-frame-alist '(foreground-color . "grey90")) ;; font color

;; highlight parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)
(require 'paren)
(set-face-background 'show-paren-match "#333333")
(set-face-foreground 'show-paren-match "#ffffff")
(set-face-attribute 'show-paren-match nil :weight 'black)

;; emacs window/client modifications
(setq line-number-mode t) ;; show line number at cursor
(setq column-number-mode t) ;; show column number at cursor
(tool-bar-mode -1) ;; disable tool bar
(menu-bar-mode -1) ;; disable menu bar
(scroll-bar-mode 1) ;; disable scroll bar
(setq inhibit-startup-message t) ;; inhibit startup messages
(global-auto-revert-mode) ;; refresh buffers when files are modified
(setq x-select-enable-clipboard t) ;; make emacs cooperate with system clipboard

;; default to text mode with spell checking
(setq-default major-mode 'text-mode)
(dolist (hook '(text-mode-hook))
	(add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
	(add-hook hook (lambda () (flyspell-mode -1))))
(add-hook 'text-mode-hook 'electric-indent-mode)

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

;; disable electric indent mode for ordinarly text
(defun remove-electric-indent-mode ()
  (electric-indent-local-mode -1))
(add-hook 'text-mode-hook 'remove-electric-indent-mode)

;; scrolling settings
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
(setq mouse-wheel-progressive-speed nil)

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

;; follow symlinks by default
(setq vc-follow-symlinks t)

;; fuzzy whitespace matching
(setq isearch-lax-whitespace t)
(setq isearch-regexp-lax-whitespace t)
(setq search-whitespace-regexp "[ \t\r\n]+")

;; enable visual line mode always by default
(global-visual-line-mode t)

;; enable adaptive line wrapping together with visual-line-mode
(when (fboundp 'adaptive-wrap-prefix-mode)
  (defun my-activate-adaptive-wrap-prefix-mode ()
    "Toggle `visual-line-mode' and `adaptive-wrap-prefix-mode' simultaneously."
    (adaptive-wrap-prefix-mode (if visual-line-mode 1 -1)))
  (add-hook 'visual-line-mode-hook 'my-activate-adaptive-wrap-prefix-mode))

;; -------------------------------------------------------------------------------------
;; TeX options

;; general latex hooks
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook (lambda () (set-fill-column 70)))
(add-hook 'bibtex-mode-hook (lambda () (set-fill-column 70)))
(setq reftex-plug-into-AUCTeX t) ;; make reftex work with auctex

;; compile documents via latexmk
(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
      :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'LaTeX-mode-hook (lambda () (setq TeX-command-default "latexmk")))

;; query for master file
(setq-default TeX-master nil)

;; compile latex documents to pdf when not using latexmk
(setq TeX-PDF-mode t)

;; use qpdfview as the pdf viewer
(setq TeX-view-program-selection
 '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
 '(("PDF Viewer" "qpdfview %o")))

;; display errors properly when editing a multi-file tex project using auctex
(defadvice TeX-parse-reset (after make-master-file-default () activate)
  (push (concat (substring (buffer-name) 1 (- (length (buffer-name)) 8))
                "." TeX-default-extension) TeX-error-file)
  (push nil TeX-error-offset))

;; suppress automatic labelling of new environments
(eval-after-load "latex" '(progn (defun LaTeX-label (env))))

;; syntax highlighting in auctex
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-indent-environment-list
   '(("verbatim" current-indentation)
     ("verbatim*" current-indentation)
     ("tabular")
     ("tabular*")
     ("align")
     ("align*")
     ("array")
     ("eqnarray")
     ("eqnarray*")
     ("displaymath")
     ("equation")
     ("equation*")
     ("picture")
     ("tabbing")
     ("tikzpicture")))
 '(TeX-insert-braces nil)
 '(custom-safe-themes
   '("45482e7ddf47ab1f30fe05f75e5f2d2118635f5797687e88571842ff6f18b4d5" "7de92d9e450585f9f435f2d9b265f34218cb235541c3d0d42c154bbbfe44d4dd" "57db540d6a8cc20d2e2f20bd63dc3af4eb9e4bbfa7252a0ee877c99b577996c4" "9ac11c78f208abf58e5b313a33147cbf209ad9dc9cb169bf82464b043b45ad7a" "20ad8133a73088c0ce4f26d106c3e70cae4e10c7e613c9b9e17a962eb194a24f" "10551f0a24d0ac97a109f02178e9e34b448ee12a52357911cf3362a6b249cae6" "843a82ff3b91bec5430f9acdd11de03fc0f7874b15c1b6fbb965116b4c7bf830" "b90d367096824d5b69b59e606c6260be55d378b58e0d03ff8866e0b3d0da1c1b" "c3b86220873ba8ec54e0988673b87ea7d11301799eca74ccf7e84cce286ec9cd" "fa14373656d9c9e86f15dcced71f42b0cd99ea13e12a66cf9eb2625097c75d02" default))
 '(font-latex-math-environments
   '("display" "displaymath" "equation" "eqnarray" "gather" "multline" "align" "alignat" "xalignat" "dmath"))
 '(package-selected-packages
   '(leaf ein auctex-latexmk markdown-mode linum-relative helm-ls-git haskell-mode f company-ycmd color-theme cargo auctex)))

;; only change sectioninig color, not font
(setq font-latex-fontify-sectioning 'color)

;; avoid larger fonts in latex
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-latex-sectioning-0-face ((t (:inherit font-latex-sectioning-1-face))))
 '(font-latex-sectioning-1-face ((t (:inherit font-latex-sectioning-2-face))))
 '(font-latex-sectioning-2-face ((t (:inherit font-latex-sectioning-3-face))))
 '(font-latex-sectioning-3-face ((t (:inherit font-latex-sectioning-4-face))))
 '(font-latex-sectioning-4-face ((t (:inherit font-latex-sectioning-5-face))))
 '(font-latex-slide-title-face ((t (:inherit (variable-pitch font-lock-type-face) :weight bold :height 1.0))))
 '(font-latex-verbatim-face ((t (:inherit nil :foreground "burlywood"))))
 '(variable-pitch ((t nil))))

;; fill paragraph with one sentence per line
;; taken from https://abizjak.github.io/emacs/2016/03/06/latex-fill-paragraph.html
(defun ales/fill-paragraph (&optional P)
  "When called with prefix argument call `fill-paragraph'.
Otherwise split the current paragraph into one sentence per line."
  (interactive "P")
  (if (not P)
      (save-excursion
        (let ((fill-column 12345678)) ;; relies on dynamic binding
          (fill-paragraph) ;; this will not work correctly if the paragraph is
                           ;; longer than 12345678 characters (in which case the
                           ;; file must be at least 12MB long. This is unlikely.)
          (let ((end (save-excursion
                       (forward-paragraph 1)
                       (backward-sentence)
                       (point-marker))))  ;; remember where to stop
            (beginning-of-line)
            (while (progn (forward-sentence)
                          (<= (point) (marker-position end)))
              (just-one-space) ;; leaves only one space, point is after it
              (delete-char -1) ;; delete the space
              (newline)        ;; and insert a newline
              (LaTeX-indent-line) ;; I only use this in combination with late, so this makes sense
              ))))
    ;; otherwise do ordinary fill paragraph
    (fill-paragraph P)))
(defun my-latex-hook ()
  (define-key LaTeX-mode-map (kbd "M-q") 'ales/fill-paragraph) )
(add-hook 'LaTeX-mode-hook 'my-latex-hook)

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

;; "Emacs speaks statistics" -- mode for R language
;; (setq load-path (cons "/usr/share/emacs/site-lisp/ess" load-path))
;;(require 'ess-site)

;; -------------------------------------------------------------------------------------
;; Miscellaneous functions and commands

(defun only-current-buffer ()
    "Kill all buffers other than the one which is currently active."
  (interactive)
    (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))
(put 'upcase-region 'disabled nil)

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region, or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

(defun my--back-to-indentation ()
  "Move to indentation respecting `visual-line-mode'."
  (if visual-line-mode
      (cl-letf (((symbol-function 'beginning-of-line) (lambda (arg) (beginning-of-visual-line arg))))
        (back-to-indentation))
    (back-to-indentation)))

(defun my--move-beginning-of-line (&optional arg)
  "Move to beginning of line respecting `visual-line-mode'."
  (cond
   ((eq major-mode 'org-mode)
    (org-beginning-of-line arg))
   (visual-line-mode
    (beginning-of-visual-line arg))
   (t (move-beginning-of-line arg))))

(defun my--move-end-of-line (&optional arg)
  "Move to end of line respecting `visual-line-mode'."
  (cond
   ((eq major-mode 'org-mode)
    (org-end-of-line arg))
   (visual-line-mode
    (end-of-visual-line arg))
   (t (move-end-of-line arg))))

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (my--back-to-indentation)
    (when (= orig-point (point))
      (my--move-beginning-of-line 1))))

;; -------------------------------------------------------------------------------------
;; Key bindings

(cua-mode t) ;; use "standard" copy/paste commands
(setq mouse-yank-at-point t) ;; paste at cursor position

;; use helm commands
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; custom key bindings for cursor navigation

(leaf-key* "M-n" 'backward-char)
(leaf-key* "M-i" 'forward-char)
;; (leaf-key* "M-u" 'helm-previous-line)
;; (leaf-key* "M-e" 'helm-next-line)
(global-set-key (kbd "M-u") 'previous-line) ;; leaf-key will change helm bindings (undesired)
(global-set-key (kbd "M-e") 'next-line)

(leaf-key* "M-C-n" 'left-word)
(leaf-key* "M-C-i" 'right-word)
(leaf-key* "M-C-u" 'backward-paragraph)
(leaf-key* "M-C-e" 'forward-paragraph)

(leaf-key* "M-l" 'smarter-move-beginning-of-line)
(leaf-key* "M-y" 'move-end-of-line)
(leaf-key* "M-C-l" 'beginning-of-buffer)
(leaf-key* "M-C-y" 'end-of-buffer)

(leaf-key* "M-;" 'scroll-down)
(leaf-key* "M-o" 'scroll-up)

(leaf-key* "M-m" 'recenter)

(leaf-key* "C-x i" 'next-buffer)
(leaf-key* "C-x n" 'previous-buffer)

(leaf-key* "M-r" 'comment-or-uncomment-region-or-line)

;; unset undesired key bindings
(global-unset-key (kbd "C-x C-q"))
(global-unset-key (kbd "M-C-o"))
(global-unset-key (kbd "M-C-;"))

;; set some keybindings for helm
(setq helm-mode-fuzzy-match t)
(setq helm-completion-in-region-fuzzy-match t)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "M-/") 'helm-execute-persistent-action)
;; (define-key helm-map (kbd "M-/") 'helm-select-action)
(define-key helm-map (kbd "M-u") 'helm-previous-line)
(define-key helm-map (kbd "M-e") 'helm-next-line)
(define-key helm-find-files-map (kbd "M-u") 'helm-previous-line)
(define-key helm-find-files-map (kbd "M-e") 'helm-next-line)
(define-key helm-read-file-map (kbd "M-u") 'helm-previous-line)
(define-key helm-read-file-map (kbd "M-e") 'helm-next-line)

;; add custom keybindings for emacs ipython notebook
(defun my-ein-hook ()
  (define-key ein:notebook-mode-map (kbd "C-c C-u") 'ein:worksheet-goto-prev-input-km)
  (define-key ein:notebook-mode-map (kbd "C-c C-e") 'ein:worksheet-goto-next-input-km)
  (define-key ein:notebook-mode-map (kbd "C-c C-k") 'ein:worksheet-toggle-output-km)
  (define-key ein:notebook-mode-map (kbd "C-c q") 'ein:worksheet-kill-cell-km) )
(add-hook 'ein:notebook-mode-hook 'my-ein-hook)

;; for making multiple cursors
(leaf-key* "M-C-q" 'set-rectangular-region-anchor)
(global-unset-key (kbd "C-<down-mouse-1>"))

;; reload buffer
(leaf-key* "C-f C-f" (lambda () (interactive) (revert-buffer t t t) (message "buffer reverted")))
