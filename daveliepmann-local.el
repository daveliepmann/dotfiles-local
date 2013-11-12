;; ;; Processing! via https://github.com/ptrv/processing2-emacs
(setq processing-location "/usr/bin/processing-java")

;; ;; Multiple cursors magic:
(global-set-key (kbd "C-,") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; ; I don't want to type <yes> or <no> followed by <Enter>; when faced
;; ; with such existential question I want to merely press <y> or <n>
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

;; ;; Org-mode configuration, necessary for MobileOrg access.
;; ;;  see http://mobileorg.ncogni.to/doc/getting-started/using-dropbox/
;; ;;;;;;;;;;;;;; CURRENTLY NONFUNCTIONAL
;; ;;;; Set to the location of your Org files on your local system
;; (setq org-directory "~/Documents/Projects")
;; ;;;; Set to the name of the file where new notes will be stored
;; (setq org-mobile-inbox-for-pull "~/Documents/Projects/flagged.org")
;; ;;;; Set to <your Dropbox root directory>/MobileOrg.
;; (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

;; Nobody puts in-file navigation in a corner.
;; Re-set keyboard navigation to up/down/sideways by logical unit.
(require 'org)
(eval-after-load 'org-mode
  (dolist (binding (list (kbd "M-<up>") (kbd "M-<down>") (kbd "M-<left>") (kbd "M-<right>") (kbd "S-<up>") (kbd "S-<down>") (kbd "M-S-<left>") (kbd "M-S-<right>") (kbd "M-S-<down>") (kbd "M-S-<up>")))
    (define-key org-mode-map binding nil)))

;;;; END ORG-MODE ===============================================

;; TODO Add word to dictionary shortcut

;; TODO HTML5 boilerplate function.
;; no me gusta yasnippet, but autoinsert might help:
;; http://www.opensource.apple.com/source/emacs/emacs-51/emacs/lisp/autoinsert.el

;; TODO frame management:
;;      - automatic multi-frame setup
;;      - show how many columns are allocated when resizing

;; Floobits integration
(load "~/.emacs.d/floobits/floobits.el")

;; Unbind the right alt/option key from emacs meta so that it can be used for special character entry
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

;; TODO execute fill-paragraph on current/previous paragraph every time I create a newline (or copy/paste, or space) in org-mode. Manual M-q is for the birds.

;; TODO flash line that cursor is on when using C-l (same color as evaling)

;; Enable inline coloring of color codes for development:
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'clojure-mode-hook 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-mode)
(add-hook 'processing-mode-hook 'rainbow-mode)

;; Full-screen (from old master config)
(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullscreen)))

;; OS X Lion fullscreen mode command-return
(global-set-key (kbd "<C-return>") 'toggle-fullscreen)

;; Undo and redo window layout changes with C-c <left/right>
(winner-mode 1)

;; Open .info files using the info command (Info-mode is insufficient)
(defun info-mode ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (kill-buffer (current-buffer))
    (info file-name)))

(add-to-list 'auto-mode-alist '("\\.info\\'" . info-mode))

;; TODO Preview markdown (.md) files using grip
;;;; from terminal: grip --gfm <filename>
;;;; or terminal: grip --gfm, then browser: http://localhost:5000/<filename>

;; Scheme using guile:
(setq geiser-active-implementations '(racket))
(setq geiser-racket-binary "/Applications/Racket v5.3.6/bin/racket")

;; Prevent magit from opening new emacs window
;; https://github.com/magit/magit/issues/862#issuecomment-25950323
(set-variable 'magit-emacsclient-executable "/usr/local/Cellar/emacs/HEAD/bin/emacsclient")
