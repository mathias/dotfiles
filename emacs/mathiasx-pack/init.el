;; User pack init file
;;
;; Use this file to initiate the pack configuration.
;; See README for more information.

'(text-scale-mode-step 1.2)

;; Load keybindings config
(live-load-config-file "bindings.el")

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
