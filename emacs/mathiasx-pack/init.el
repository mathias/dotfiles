;; User pack init file
;;
;; Use this file to initiate the pack configuration.
;; See README for more information.

'(text-scale-mode-step 1.2)

;; Load keybindings config
(live-load-config-file "bindings.el")

;; Load MobileOrg org mode settings
(live-load-config-file "orgmode.el")

;; Slamhound for Clojure
(live-add-pack-lib "slamhound")
(require 'slamhound)
;; (live-load-config-file "slamhound.el")
