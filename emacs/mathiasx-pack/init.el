;; User pack init file
;;
;; Use this file to initiate the pack configuration.
;; See README for more information.


;; Load keybindings config
(live-load-config-file "bindings.el")
(live-load-config-file "fonts.el")

;; Load MobileOrg org mode settings
(live-load-config-file "orgmode.el")

;; Slime for Common Lisp REPL:
(setq slime-lisp-implementations
      '((sbcl ("sbcl" "--core" "sbcl.core-for-slime"))))
(live-add-pack-lib "slime")
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(add-to-list 'load-path "~/slime")
(require 'slime)
(slime-setup)

;; Slamhound for Clojure
(live-add-pack-lib "slamhound")
(require 'slamhound)

;; Prolog dev
(add-to-list 'auto-mode-alist '("\\.pro\\'" . prolog-mode))

;; Hoplon dev
(add-to-list 'auto-mode-alist '("\\cljs.hl\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\html.hl\\'" . html-mode))

;; Do not save autosave/swap files in current directory
;; Save all tempfiles in $TMPDIR/emacs$UID/ instead
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))

(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)
