;; .emacs -- mathiasx's emacs configuration

;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please
(setq inhibit-startup-screen t)

;;;; Load packages from Marmalade and Melpa
(require 'package)

(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (if (file-directory-p default-directory)
      (normal-top-level-add-subdirs-to-load-path)))

(add-to-list 'package-archives
	     '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; use-package like http://cestlaz.github.io
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (if (version< emacs-version "24.4")
    (require 'cl)
    (require 'cl-lib))
  (require 'use-package))

(require 'bind-key)

;; Always try to install packages (rather than having to pass :ensure t to every package:
(setq use-package-always-ensure t)

(use-package ag
  :config
  ;; color highlights in search
  (setq ag-highlight-search t))

(use-package auto-complete
  :init
  (ac-config-default)
  (global-auto-complete-mode t))

(use-package cider
  :config
   ;; hide special buffers when using Cider
  (setq nrepl-hide-special-buffers t))

(use-package clojure-mode
  :defer t
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
    ;; recognize boot script files using shebang:
    (add-to-list 'magic-mode-alist '(".* boot" . clojure-mode))
    ;; Hoplon dev
    (add-to-list 'auto-mode-alist '("\\cljs.hl\\'" . clojure-mode))))

(use-package coffee-mode
  :defer t)

(use-package counsel)

(use-package cyberpunk-theme
  :defer nil
  :init
  (progn
    (load-theme 'cyberpunk t)
    (enable-theme 'cyberpunk)))

(use-package elixir-mode :defer t)

(use-package alchemist :defer t)

(use-package elm-mode :defer t)

(use-package git-link :defer t)

(use-package highlight-symbol :defer t)

(use-package htmlize
  :defer t
  :mode ("\\.org\\'" . org-mode))

(use-package ivy
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy))

(use-package keyfreq
  :defer nil
  :config
  (progn
    (if (not (fboundp 'reduce))
	(defalias 'reduce 'cl-reduce))
    (keyfreq-mode 1)
    (keyfreq-autosave-mode 1)))

(use-package magit
  :defer nil)

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))
  :config
  (progn
    (visual-line-mode t)
    (flyspell-mode t)))

;; highlight matching parentheses
(use-package mic-paren
  :defer nil
  :config
  (progn
    (setq paren-highlight-offscreen t)
    (setq paren-match-face 'highlight)
    (paren-activate)))

(use-package org
  :defer t
  :mode ("\\.org\\'" . org-mode)
  :config
  (progn
    (visual-line-mode t)
    (flyspell-mode t)
    (setq org-drill-maximum-duration 25)))

(use-package org-journal
  :defer t
  :mode ("\\.org\\'" . org-mode))

(use-package org-plus-contrib
  :defer t
  :mode ("\\.org\\'" . org-mode))

(use-package org-pomodoro
  :defer t
  :mode ("\\.org\\'" . org-mode)
  :config (setq org-pomodoro-play-sounds nil))

(use-package paredit
  :defer t
  :init
  (progn
    (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
    (add-hook 'clojure-mode-hook          #'enable-paredit-mode)
    (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
    (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
    (add-hook 'scheme-mode-hook           #'enable-paredit-mode))
  :bind (:map paredit-mode-map
	      ("M-)" . paredit-forward-slurp-sexp)
	      ("M-(" . paredit-wrap-round)))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package slamhound
  :defer t
  :mode ("\\.clj\\'" . clojure-mode))

(use-package slime
  :defer t
  :config
  (progn
    (require 'slime-autoloads)
    (setq inferior-lisp-program "/usr/local/bin/sbcl")
    (setq slime-contribs '(slime-fancy))))

(use-package smex
  :defer t
  :config
  (progn
    ;; Can be omitted. This might cause a (minimal) delay when Smex is auto-initialized on its first run:
    (smex-initialize)

    ;; remap M-x
    (global-set-key (kbd "M-x") 'smex)
    (global-set-key (kbd "M-X") 'smex-major-mode-commands)
    ;; This is your old M-x.
    (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)))

(use-package swiper
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)
	 ("C-c C-r" . ivy-resume)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("C-c k" . counsel-ag))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))

(use-package try)

(use-package which-key
  :config
  (which-key-mode))

(use-package writegood-mode
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.markdown\\'" . writegood-mode))
    (add-to-list 'auto-mode-alist '("\\.md\\'" . writegood-mode))
    (add-to-list 'auto-mode-alist '("\\.txt\\'" . writegood-mode)))
  :bind (("C-c g" . writegood-mode)))

;;;; setup emacs itself

(setq user-full-name "Matt Gauger")
(setq user-mail-address "matt.gauger@gmail.com")

(setq custom-file "~/.emacs.d/custom.el")

;; Autorevert files (mandatory if working with VCS like git)
(global-auto-revert-mode t)

;; Do not save autosave/swap files in current directory
;; Save all tempfiles in $TMPDIR/emacs$UID/ instead
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))

(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Monaco")
  (set-face-attribute 'default nil :height 200))

;; change font size, interactively
(global-set-key (kbd "s-+") 'text-scale-increase)
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)

;;;; global buffer settings
(column-number-mode)
(line-number-mode)

;; Set locale to UTF8
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; set initial frame size to always be maximized
(modify-all-frames-parameters '((fullscreen . maximized)))

;;;; functions


;;;; spell checking
(setq ispell-program-name "aspell")
(add-to-list 'exec-path "/usr/local/bin")

;;;; Whitespace highlighting
(setq-default show-trailing-whitespace t)

;;;; Whitespace cleanup
(global-set-key (kbd "C-c w") 'whitespace-cleanup)

;;;; prolog dev
(add-to-list 'auto-mode-alist '("\\.pro\\'" . prolog-mode))

;;;; boot dev
(add-to-list 'auto-mode-alist '("\\html.hl\\'" . html-mode))

;; .emacs file
(add-to-list 'auto-mode-alist '("\\\.emacs\\'" . emacs-lisp-mode))

;; Octave files
(add-to-list 'auto-mode-alist '("\\\.m\\'" . octave-mode))

;;; apropos settings
(setq apropos-sort-by-scores t)

(setq org-directory "~/dev/org")
;; Set to the name of the file where new capture notes will be stored
(setq org-default-notes-file (expand-file-name (concat org-directory "/notes.org")))
(define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates
      '(("n" "Note"
	 entry (file 'org-custom-notes-file)
	 "* %?\n\n  %i\n\n  From: %a"
	 :empty-lines 1
	 :prepend 1)
	("j" "Journal Entry"
	 entry (file (lambda () (get-journal-file-today)))
	 "* Entry: %?\n\n  %i\n\n  From: %a"
	 :empty-lines 1)
	("t" "TODO"
	 entry (file 'org-custom-notes-file)
	 "* TODO %?\n\n %i \n\n From: %a"
	 :empty-lines 1
	 :prepend 1)
	("f" "Fact to drill/study - ML plan"
	 entry (file+headline (lambda () "~/dev/org/drill.org") "Facts")
	 "** Fact:        :drill:\n:PROPERTIES:\n:DATE_ADDED: %u\n:FROM: %l\n:END:\n\n%i%?\n\n"
	 :empty-lines 1)
	("a" "annotate.mattgauger.com link"
	 entry (file (lambda () "~/dev/mathias/annotate.mattgauger.com/index.org"))
	 "* %? %U\n\n%i"
	 :empty-lines 1
	 :prepend 1)))

;; bind Org agendas view
(global-set-key "\C-ca" 'org-agenda)

;; org-journal setup
(setq org-journal-dir (concat org-directory "/journal"))

(defun get-journal-file-today ()
  "Return filename for today's journal entry."
  (let ((daily-name (format-time-string "%Y%m%d.org")))
    (expand-file-name (concat org-journal-dir "/" daily-name))))

(defun journal-file-today ()
  "Create and load a journal file based on today's date."
  (interactive)
  (find-file (get-journal-file-today)))

(global-set-key (kbd "C-c f j") 'journal-file-today)

(defun journal-file-insert ()
  "Insert's the journal heading based on the file's name."
  (interactive)
  (insert (format-time-string
	   "#+TITLE: Journal Entry - %Y-%b-%d (%A)\n\n")))

(add-hook 'find-file-hook 'auto-insert)
(define-auto-insert "/[0-9]\\{8\\}\\.org$" [journal-file-insert])

;; org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((lisp . t)
   (python . t)
   (emacs-lisp . t)))

;; org TODO list items clocking
(setq org-log-done 'time)


;; Window movement commands
;; from: Writing GNU Emacs Extensions by Bob Glickstein

(defun other-window-backward (&optional n)
  "Select Nth previous window."
  (interactive "p")
  (other-window (- (or n 1))))

(global-set-key "\C-x\C-n" `other-window)
(global-set-key "\C-x\C-p" `other-window-backward)

;; From mastering emacs, quick window switch (faster than C-x o)
(global-set-key (kbd "M-o") 'other-window)

;; Octave mode settings
(add-hook 'octave-mode-hook
	  (lambda ()
	    (abbrev-mode 1)
	    (auto-fill-mode 1)
	    (if (eq window-system 'x)
		(font-lock-mode 1))))

;; imenu
(global-set-key (kbd "M-i") 'imenu)

;; TRAMP mode config
(setq tramp-default-method "ssh")

(defun sudo ()
  "Use TRAMP to `sudo' the current buffer"
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
	     buffer-file-name))))

;; dired config
(require 'dired-x)

;; commenting lines
;; from http://ergoemacs.org/misc/emacs_comment-line_vs_comment-dwim.html
(defun my-comment-dwim ()
  "like `comment-dwim', but toggle comment if cursor is not at end of line."
  (interactive)
  (if (region-active-p)
      (comment-dwim nil)
    (if (eq (point) (line-end-position))
	(comment-dwim nil)
      (comment-region (line-beginning-position) (line-end-position)))))

(global-set-key (kbd "M-;") 'my-comment-dwim)

;; ido setup
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; elixir mode setup
;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))


(defalias 'list-buffers 'ibuffer-other-window)
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
