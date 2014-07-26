;; .emacs -- mathiasx's emacs configuration

;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please
(setq inhibit-startup-screen t)


;;;; Load packages from Marmalade and Melpa
(require 'package)

(add-to-list 'load-path "~/.emacs.d/")

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; package list
(dolist (p '(ag
	     clojure-mode
	     coffee-mode
	     cider
	     cyberpunk-theme
	     markdown-mode
	     mic-paren
	     midje-mode
	     paredit 
	     org
	     org-plus-contrib
	     rainbow-delimiters
	     slamhound
	     slime))
  (when (not (package-installed-p p))
    (package-install p)))

;;;; macros

;; (defmacro after (mode &rest body)
;;   "`eval-after-load' MODE evaluate BODY."
;;   (declare (indent defun))
;;   `(eval-after-load ,mode
;;      '(progn ,@body)))

;;;; setup emacs itself

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  ;;'(custom-enabled-themes (quote (cyberpunk-theme)))
  '(text-scale-mode-step 1.2)
  '(visible-bell t))

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

(dolist (m paredit-and-eldoc-modes)
  (add-hook (intern (concat (symbol-name m) "-mode-hook"))
            'turn-on-eldoc-mode))

(defun mathiasx-paredit-mode-maps ()
  (interactive)
  (paredit-mode +1)
  (define-key paredit-mode-map (kbd "M-)")
    'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-(")
    'paredit-wrap-round))

(dolist (m paredit-and-eldoc-modes)
  (add-hook (intern (concat (symbol-name m) "-mode-hook"))
	    'mathiasx-paredit-mode-maps))

;;;; spell checking
(setq ispell-program-name "aspell")
(add-to-list 'exec-path "/usr/local/bin")

;;;; theme
(load-theme 'cyberpunk t)

;;;; prolog dev
(add-to-list 'auto-mode-alist '("\\.pro\\'" . prolog-mode))

;;;; Hoplon dev
(add-to-list 'auto-mode-alist '("\\cljs.hl\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\html.hl\\'" . html-mode))

;;;; Slime for Common Lisp REPL:
;; (setq slime-lisp-implementations
;;       '((sbcl ("sbcl" "--core" "sbcl.core-for-slime"))))

(require 'slime-autoloads)

(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;;;; Slamhound for Clojure
(require 'slamhound)

;;;; mic-paren
(require 'mic-paren)
(paren-activate)

;;;; rainbow delimiters
(require 'rainbow-delimiters)
;(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
