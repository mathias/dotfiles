;; .emacs -- mathiasx's emacs configuration

;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please
(setq inhibit-startup-screen t)

;;;; packages
;; Load packages from Marmalade and Melpa
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
	     cyberpunk-theme
	     clojure-mode
	     clojurescript-mode
	     coffee-mode
	     ido-better-flex
	     nrepl
	     paredit 
	     org
	     org-plus-contrib))
  (when (not (package-installed-p p))
    (package-install p)))

;;;; setup

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  ;;'(custom-enabled-themes (quote (cyberpunk-theme)))
  '(line-number-mode nil)
  '(text-scale-mode-step 1.2)
  '(visible-bell t))


(when (eq system-type 'darwin)
  ;; default Latin font (e.g. Consolas)
  (set-face-attribute 'default nil :family "Monaco")
  ;; default font size (point * 10)
  ;;
  ;; WARNING!  Depending on the default font,
  ;; if the size is not supported very well, the frame will be clipped
  ;; so that the beginning of the buffer may not be visible correctly.
  (set-face-attribute 'default nil :height 200)
  ;; use specific font for Korean charset.
  ;; if you want to use different font size for specific charset,
  ;; add :size POINT-SIZE in the font-spec.
  ;; (set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))
  ;; you may want to add different for other charset in this way.
)



;; Be able to zoom font
(defun zoom-in ()
  "Increase font size by 10 points"
  (interactive)
  (set-face-attribute 'default nil
                      :height
                      (+ (face-attribute 'default :height)
                         10)))

(defun zoom-out ()
  "Decrease font size by 10 points"
  (interactive)
  (set-face-attribute 'default nil
                      :height
                      (- (face-attribute 'default :height)
                         10)))

;; change font size, interactively
(global-set-key (kbd "s-+") 'zoom-in)
(global-set-key (kbd "s-=") 'zoom-in)
(global-set-key (kbd "s--") 'zoom-out)

;; Use markdown mode for markdown files
(autoload 'markdown-mode "markdown-mode"
          "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; paredit
(setq paredit-and-eldoc-modes
      '(clojure
         coffee
         emacs-lisp
         ielm
         lisp
         lisp-interaction
         nrepl
         scheme))

(dolist (m paredit-and-eldoc-modes)
  (add-hook (intern (concat (symbol-name m) "-mode-hook"))
            'turn-on-eldoc-mode))

(dolist (m paredit-and-eldoc-modes)
  (add-hook (intern (concat (symbol-name m) "-mode-hook"))
            (lambda ()
              (paredit-mode +1)
              (define-key paredit-mode-map (kbd "M-)")
                          'paredit-forward-slurp-sexp)
              (define-key paredit-mode-map (kbd "M-(")
                          'paredit-wrap-round))))

;; spell checking
(setq ispell-program-name "aspell")
(add-to-list 'exec-path "/usr/local/bin")

;; theme
(load-theme 'cyberpunk t)
