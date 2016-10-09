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
 	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(mic-paren . "melpa") t)
(add-to-list 'package-pinned-packages '(keyfreq . "melpa") t)
(add-to-list 'package-pinned-packages '(git-link . "melpa") t)

(add-to-list 'package-pinned-packages '(elm-mode . "melpa-stable") t)

;; package list
(dolist (p '(ag
	     cider
	     cl-lib
	     clojure-mode
	     coffee-mode
	     cyberpunk-theme
	     elixir-mode
	     elm-mode
	     git-link
	     highlight-symbol
	     keyfreq
	     magit
	     markdown-mode
	     mic-paren
	     org
	     org-journal
	     org-plus-contrib
	     paredit
	     rainbow-delimiters
	     slamhound
	     slime
	     smex))
  (when (not (package-installed-p p))
    (package-install p)))

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

;;;; functions

; borrowed from https://github.com/losingkeys/dotfiles/blob/master/.emacs
(defun add-hooks-to-modes (modes hooks)
  "Adds the specified hooks to the specified modes"
  (dolist (m modes)
    (let ((mode (intern (concat (symbol-name m) "-mode-hook"))))
      (dolist (hook hooks)
       (add-hook mode hook)))))

;;;; markdown mode

(autoload 'markdown-mode "markdown-mode"
	  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;;; paredit mode
(setq paredit-and-eldoc-modes
      '(cider
	clojure
	coffee
	emacs-lisp
	ielm
	kibit-mode
	lisp
	lisp-interaction
	scheme
	smex))

(defun mathiasx-paredit-mode-maps ()
  (interactive)
  (paredit-mode +1)
  (define-key paredit-mode-map (kbd "M-)")
    'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-(")
    'paredit-wrap-round))

(add-hooks-to-modes paredit-and-eldoc-modes
		    '((lambda ()
			(turn-on-eldoc-mode)
			(mathiasx-paredit-mode-maps))))

;;;; magit mode
(require 'magit)

;;;; spell checking
(setq ispell-program-name "aspell")
(add-to-list 'exec-path "/usr/local/bin")

;;;; Whitespace higlighting
(setq-default show-trailing-whitespace t)

;;;; Whitespace cleanup
(global-set-key (kbd "C-c w") 'whitespace-cleanup)

;;;; theme
(load-theme 'cyberpunk t)

;;;; prolog dev
(add-to-list 'auto-mode-alist '("\\.pro\\'" . prolog-mode))

;;;; boot dev
(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
;;;; recognize boot script files using shebang:
(add-to-list 'magic-mode-alist '(".* boot" . clojure-mode))

;;;; Hoplon dev
(add-to-list 'auto-mode-alist '("\\cljs.hl\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\html.hl\\'" . html-mode))

;; .emacs file
(add-to-list 'auto-mode-alist '("\\\.emacs\\'" . emacs-lisp-mode))

;; Octave files
(add-to-list 'auto-mode-alist '("\\\.m\\'" . octave-mode))


;;;; Slime for Common Lisp REPL:
(require 'slime-autoloads)

(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;;;; Clojure dev
(setq nrepl-hide-special-buffers t) ;; hide special buffers when using Cider

;;;; Slamhound for Clojure
(require 'slamhound)

;;;; mic-paren
(require 'mic-paren)
(setq paren-highlight-offscreen t)
(setq paren-match-face 'highlight)
(paren-activate)

;;;; rainbow delimiters
(require 'rainbow-delimiters)
;(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;; smex settings
(require 'smex)
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
		  ; when Smex is auto-initialized on its first run.

;; remap M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;; apropos settings
(setq apropos-sort-by-scores t)

;;; ag settings
(setq ag-highlight-search t) ;; color highlights in search

;;; org mode settings
(add-hooks-to-modes '(org markdown)
		    '((lambda ()
			(visual-line-mode t)
			(flyspell-mode t))))

(setq org-directory "~/dev/org")
;; Set to the name of the file where new capture notes will be stored
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates
      '(("n" "Note"
	 entry (file 'org-default-notes-file)
	 "* %?\n\n  %i\n\n  From: %a"
	 :empty-lines 1
	 :prepend 1)
	("j" "Journal Entry"
	 entry (file (get-journal-file-today))
	 "* Event: %?\n\n  %i\n\n  From: %a"
	 :empty-lines 1)
	("t" "TODO"
	 entry (file 'org-default-notes-file)
	 "* TODO %?\n\n %i \n\n From: %a"
	 :empty-lines 1
	 :prepend 1)))

;; bind Org agendas view
(global-set-key "\C-ca" 'org-agenda)

;; org-journal setup
(setq org-journal-dir (concat org-directory "/journal"))
(require 'org-journal)

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
  (when (string-match "\\(20[0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)"
                      (buffer-name))
    (let ((year  (string-to-number (match-string 1 (buffer-name))))
          (month (string-to-number (match-string 2 (buffer-name))))
          (day   (string-to-number (match-string 3 (buffer-name))))
          (datim nil))
      (setq datim (encode-time 0 0 0 day month year))
      (insert (format-time-string
	       "#+TITLE: Journal Entry - %Y-%b-%d (%A)\n\n" datim)))))

(add-hook 'find-file-hook 'auto-insert)
(add-to-list 'auto-insert-alist '(".*/[0-9]{8}*.org$" . journal-file-insert))

;; org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((lisp . t)))

;; Set up keyfreq (record key/command frequency)
(if (not (fboundp 'reduce))
    (defalias 'reduce 'cl-reduce))
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;; Window movement commands
;; from: Writing GNU Emacs Extensions by Bob Glickstein

(defun other-window-backward (&optional n)
  "Select Nth previous window."
  (interactive "p")
  (other-window (- (or n 1))))

(global-set-key "\C-x\C-n" `other-window)
(global-set-key "\C-x\C-p" `other-window-backward)

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
