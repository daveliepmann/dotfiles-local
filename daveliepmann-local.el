;; Processing! via https://github.com/ptrv/processing2-emacs
(setq processing-location "/usr/bin/processing-java")

;; Multiple cursors magic:
(global-set-key (kbd "C-,") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

; I don't want to type <yes> or <no> followed by <Enter>; when faced
; with such existential question I want to merely press <y> or <n>
(fset 'yes-or-no-p 'y-or-n-p)

;;;; ORG-MODE ===============================================
;; Let Shift-<arrow> select text instead of org-mode's custom
;; functionality. This is a config option.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-support-shift-select t))
(custom-set-faces)

;; I have no use for M-<up/down arrow> moving my sections.
;; Re-set keyboard navigation to up/down/sideways by logical unit.
(eval-after-load 'org-mode
  (dolist (binding (list (kbd "M-<up>") (kbd "M-<down>") (kbd "M-<left>") (kbd "M-<right>") (kbd "S-<up>") (kbd "S-<down>") (kbd "M-S-<left>") (kbd "M-S-<right>")))
    (define-key org-mode-map binding nil)))

;; Org-mode configuration, necessary for MobileOrg access.
;;  see http://mobileorg.ncogni.to/doc/getting-started/using-dropbox/
;;;;;;;;;;;;;; CURRENTLY NONFUNCTIONAL
;;;; Set to the location of your Org files on your local system
(setq org-directory "~/Documents/Projects")
;;;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Documents/Projects/flagged.org")
;;;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

;;;; END ORG-MODE ===============================================

;;;; TODO I want an HTML5 boilerplate function.
