;; Where do org mode files live?
(setq org-agenda-files (quote ("~/dev/org")))

;; Set up org mode key commands
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key (kbd "C-S-p") 'org-mobile-push)
(global-set-key (kbd "C-S-l") 'org-mobile-pull)
