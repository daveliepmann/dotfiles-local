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
(add-hook 'prog-mode-hook 'rainbow-mode)

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

;; Open .php files with html-mode
(add-to-list 'auto-mode-alist '("\\.php\\'" . html-mode))

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

;; Un-fill region (for exporting, e.g. to tumblr)
;; from http://ergoemacs.org/emacs/modernization_fill-paragraph.html
(defun compact-uncompact-block ()
  "Remove or add line ending chars on current paragraph.
This command is similar to a toggle of `fill-paragraph'.
When there is a text selection, act on the region."
  (interactive)
  ;; This command symbol has a property “'stateIsCompact-p”.
  (let (currentStateIsCompact (bigFillColumnVal 90002000) (deactivate-mark nil))
    ;; 90002000 is just random. you can use `most-positive-fixnum'
    (save-excursion
      ;; Determine whether the text is currently compact.
      (setq currentStateIsCompact
            (if (eq last-command this-command)
                (get this-command 'stateIsCompact-p)
              (if (> (- (line-end-position) (line-beginning-position)) fill-column) t nil)))
      (if (region-active-p)
          (if currentStateIsCompact
              (fill-region (region-beginning) (region-end))
            (let ((fill-column bigFillColumnVal))
              (fill-region (region-beginning) (region-end))))
        (if currentStateIsCompact
            (fill-paragraph nil)
          (let ((fill-column bigFillColumnVal))
            (fill-paragraph nil))))
      (put this-command 'stateIsCompact-p (if currentStateIsCompact nil t)))))

;; Distinguish multiple buffers with identical filenames.
;; See http://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/")

;; Git color defaults are broken. These fixes are cribbed from magnars
;; https://github.com/magnars/.emacs.d/blob/master/setup-magit.el
(require 'magit)
(set-face-background 'magit-item-highlight "#333333")
(set-face-background 'diff-file-header "#555555")
(set-face-foreground 'diff-context "#777777")
(set-face-foreground 'diff-added "#00cc33")
(set-face-foreground 'diff-removed "#ff0000")

;; Set frame to 80 columns
;; from http://nullprogram.com/blog/2010/10/06/
;; see also C-x +
(defun set-window-width (n)
  "Set the selected window's width."
  (adjust-window-trailing-edge (selected-window) (- n (window-width)) t))

(defun set-80-columns ()
  "Set the selected window to 80 columns."
  (interactive)
  (set-window-width 80))

(global-set-key "\C-x~" 'set-80-columns)

;; Toggle highlighting on column 80
(defun highlight-col-80 ()
  (interactive) (column-marker-1 80))

;; ;; Highlight current line
;; (global-hl-line-mode 1)
;; ;; ...as unobtrusively as possible
;; (set-face-background 'hl-line "#112211")
;; ;; TODO doesn't play well with rainbow-mode. May be fixed in trunk 115615 of emacs.

;; I want diffs to show side-by-side
(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

(require 'cider)

(load "facts.el")

;;;; Python config
;; use IPython
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
;; use the wx backend, for both mayavi and matplotlib
(setq py-python-command-args
      '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
(setq py-force-py-shell-name-p t)

;; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)
;; don't split windows
(setq py-split-windows-on-execute-p nil)
;; try to automagically figure out indentation
(setq py-smart-indentation t)

;; Add FIXME (used by lein) to master list of highlighted words TODO/XXX/BUG
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil '(("\\<\\(FIXME\\)" 1 font-lock-warning-face prepend)))))

;; I always want to close CIDER. See cider-quit in
;; cider-interaction.el for original w/y-or-n-p
(defun really-quit-cider ()
  (interactive)
  (dolist (connection nrepl-connection-list)
    (when connection
      (nrepl-close connection)))
  (message "All active nREPL connections were closed")
  (cider-close-ancilliary-buffers))

(defalias 'cider-quit 'really-quit-cider)

;; web-mode handles JS within HTML:
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

(defun csv-to-sexp (csv)
  "Parses the string `csv` as a comma-separated-value file,
returning a sexp containing the values with strings converted to
numbers where appropriate."
  (-map (lambda (s) (-map 'reformat-field (s-split "," s))) (s-lines csv)))

;; Faster bold/italic keybindings for markdown-mode
(add-hook 'markdown-mode-hook
          (lambda () (local-set-key (kbd "C-c b") #'markdown-insert-bold)))
(add-hook 'markdown-mode-hook
          (lambda () (local-set-key (kbd "C-c i") #'markdown-insert-italic)))
