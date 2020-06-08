;; setup package stuffs
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize) ;;

;; set emacs path to user path. helps get commands working
;; initially added bc pdf2dsc used in latex-preview could not be found even though it was installed
(defun set-exec-path-from-shell-PATH ()
  "Sets the exec-path to the same value used by the user shell"
  (let ((path-from-shell
         (replace-regexp-in-string
          "[[:space:]\n]*$" ""
          (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; call function now
(set-exec-path-from-shell-PATH)

;; set location of elisp code I want to use (minor modes)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (kaolin-ocean)))
 '(custom-safe-themes
   (quote
    ("53993d7dc1db7619da530eb121aaae11c57eaf2a2d6476df4652e6f0bd1df740" "a9d67f7c030b3fa6e58e4580438759942185951e9438dd45f2c668c8d7ab2caf" "ff829b1ac22bbb7cee5274391bc5c9b3ddb478e0ca0b94d97e23e8ae1a3f0c3e" "11e0bc5e71825b88527e973b80a84483a2cfa1568592230a32aedac2a32426c1" "51043b04c31d7a62ae10466da95a37725638310a38c471cc2e9772891146ee52" "030346c2470ddfdaca479610c56a9c2aa3e93d5de3a9696f335fd46417d8d3e4" "7d4340a89c1f576d1b5dec57635ab93cdc006524bda486b66d01a6f70cffb08e" "886fe9a7e4f5194f1c9b1438955a9776ff849f9e2f2bbb4fa7ed8879cdca0631" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "3629b62a41f2e5f84006ff14a2247e679745896b5eaa1d5bcfbc904a3441b0cd" "6ee6f99dc6219b65f67e04149c79ea316ca4bcd769a9e904030d38908fd7ccf9" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "c567c85efdb584afa78a1e45a6ca475f5b55f642dfcd6277050043a568d1ac6f" "39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "cab317d0125d7aab145bc7ee03a1e16804d5abdfa2aa8738198ac30dc5f7b569" "1a53efc62256480d5632c057d9e726b2e64714d871e23e43816735e1b85c144c" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "345f8f92edc3508574c61850b98a2e0a7a3f5ba3bb9ed03a50f6e41546fe2de0" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(debug-on-error t)
 '(fci-rule-color "#073642")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#49483E" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#49483E" . 100))))
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f"))))
 '(inhibit-startup-screen t)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(magit-diff-use-overlays nil)
 '(org-agenda-files
   (quote
    ("~/Sync/Org/mylife.org" "~/Sync/Org/inbox.org" "~/Sync/Org/work.org" "~/Sync/Org/school.org")))
 '(org-log-into-drawer t)
 '(package-selected-packages
   (quote
    (magit counsel-tramp counsel-projectile org-projectile projectile origami sublimity avy smex helm-org-rifle flx flyspell-correct-ivy ivy-omni-org ivy-rich ivy-todo helm-swoop expand-region multiple-cursors latex-math-preview latex-pretty-symbols latex-preview-pane kaolin-themes doom-themes org-bullets spacemacs-theme ess auto-complete autopair color-theme color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow color-theme-solarized darkokai-theme electric-operator monokai-theme rfringe auctex)))
 '(pdf-view-midnight-colors (quote ("#b2b2b2" . "#292b2e")))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(recentf-max-menu-items 50)
 '(save-place-mode t nil (saveplace))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#cb4b16")
     (60 . "#b58900")
     (80 . "#859900")
     (100 . "#2aa198")
     (120 . "#268bd2")
     (140 . "#d33682")
     (160 . "#6c71c4")
     (180 . "#dc322f")
     (200 . "#cb4b16")
     (220 . "#b58900")
     (240 . "#859900")
     (260 . "#2aa198")
     (280 . "#268bd2")
     (300 . "#d33682")
     (320 . "#6c71c4")
     (340 . "#dc322f")
     (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#49483E" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#14141e" :foreground "#e6e6e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "nil" :family "Iosevka"))))
 '(comint-highlight-input ((t (:weight thin))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#5F8DAD" :slant normal))))
 '(font-lock-comment-face ((t (:foreground "#5F8DAD" :slant normal))))
 '(scroll-bar ((t (:foreground "dark slate gray"))))
 '(show-paren-match ((t (:background "cyan" :inverse-video nil)))))

;; turn on auto complete. woo!!
(require 'auto-complete)
(global-auto-complete-mode t)

;; turn on matching pairs mode
(electric-pair-mode 1)

;; show cursor position within line
(column-number-mode 1)

;; since electris-pair is a global minor mode, it's always active and
;; this can cause problems with other modes (e.g. autocomplete of
;; \letf( in auxtex).
;; so, from here:
;; we inhibit electric-pair in all modes not explicitly specified here:
(defvar my-electic-pair-modes '(ess-mode org-mode))
(defun my-inhibit-electric-pair-mode (char)
  (not (member major-mode my-electic-pair-modes)))
(setq electric-pair-inhibit-predicate #'my-inhibit-electric-pair-mode)

;; turn on good spacing around operators
(require 'electric-operator)
(add-hook 'ess-mode-hook #'electric-operator-mode)
(setq electric-operator-R-named-argument-style '(unspaced))

;; add in custom spacing rules
(electric-operator-add-rules-for-mode 'ess-mode
  (cons ":=" " := "))

;; change ess indenting to depth of 2
(defun myindent-ess-hook ()
  (setq ess-indent-level 2))
(add-hook 'ess-mode-hook 'myindent-ess-hook)

;; to avoid continued indenting (esp. with ggplot)
(add-hook 'ess-mode-hook
          (lambda ()
            (setq ess-first-continued-statement-offset 2)
            (setq ess-continued-statement-offset 0)))

;; turn off underscore to <-
;; turn on ; to <-
(require 'ess-site)
(global-set-key (kbd "C-;")  (lambda () (interactive) (insert " <- ")))
;; ;; leave my underscore key alone! need it twice ...
(ess-toggle-underscore nil)
(ess-toggle-underscore nil)

;; Set default R version, (i.e. the one launched by typing M-x R <RET>)
(setq inferior-R-program-name "/usr/local/bin/R")

;; use RStudio styling
(setq ess-default-style 'RStudio)
;; which sets:
;; >>   (RStudio
;; >>    (ess-indent-offset . 2)
;; >>    (ess-offset-arguments . open-delim)
;; >>    (ess-offset-arguments-newline . prev-line)
;; >>    (ess-offset-block . prev-line)
;; >>    (ess-offset-continued . straight)
;; >>    (ess-align-nested-calls)
;; >>    (ess-align-arguments-in-calls "function[     ]*(")
;; >>    (ess-align-continuations-in-calls)
;; >>    (ess-align-blocks)
;; >>    (ess-indent-from-lhs arguments)
;; >>    (ess-indent-from-chain-start . t)
;; >>    (ess-indent-with-fancy-comments))

;; fix indenting with only 1 # (it defaults to space 40!)
(add-hook 'ess-mode-hook
	  (lambda()
	    ;; don't indent comments
            (setq ess-indent-with-fancy-comments nil)
            ;; turn on outline mode
            (setq-local outline-regexp "[# ]+")
	    (outline-minor-mode t)))



;; fast switch between windows
;; C-x o isn't so bad... and C-tab is used in org and magit...
;; (global-set-key [C-tab] 'other-window)

;; fast commenting/uncommenting
;; (global-set-key (kbd "M-\-") 'comment-dwim)

;; ;; Control and up/down arrow keys to search history with matching what you've already typed
;; (define-key comint-mode-map [C-up] 'comint-previous-matching-input-from-input)
;; (define-key comint-mode-map [C-down] 'comint-next-matching-input-from-input)

;; turn on recent files with C-x C-r
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; THIS COMMENTED PART WORKED ON WINDOWS 10
;; turn off underscore to <-
;; turn on ; to <-
;; (require 'ess-site)
;; (setq ess-smart-S-assign-key (kbd ";"))
;; (ess-toggle-S-assign-key t) ; enable above key definition
;; ;; leave my underscore key alone! need it twice ...
;; (ess-toggle-underscore nil)
;; (ess-toggle-underscore nil)

;; turn on TODO/FIXME highlighting
;; I want this ALWAYS turned on so I turn it into a global minor mode
(define-globalized-minor-mode my-global-fic-mode fic-mode
  (lambda () (fic-mode 1)))
;; turn on global-fic-mode
(my-global-fic-mode 1)

;; turn on fixme to highlight TODOs, FIXMEs, ...
;; this one has fringe highlighting!
;; AOZ customized this and the modified minor-mode should sit in ~/.emacs.d/elisp/
;;(require 'myfixme)
;;(defun turn-on-myfixme-mode ()
;;   (myfixme-mode 1))
;;(add-hook 'ess-mode-hook 'turn-on-myfixme-mode)

;; AOZ: this next one is no longer used and I use myfixme instead
;; turn on fixme highlighting
;;(require 'fic-mode)
;;(add-hook 'ess-mode-hook 'turn-on-fic-mode)

;; set directory
(cd "~/Documents/GitRepos/")

;; set replace-string keyboard binding
(global-set-key (kbd "M-r") 'replace-string)

;;;;;;;;;;;;
;; AucTeX ;;
;;;;;;;;;;;;

;; set PATH for mac auctex to work
(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin/"))  
(setq exec-path (append exec-path '("/Library/TeX/texbin/")))

;; load auctex
(require 'auto-complete-auctex)
(load "auctex.el" nil t t)

;; set path to gs for preview
(setq preview-gs-command "/usr/local/bin/gs")

;; set path to pdf2dsc
(add-to-list 'exec-path "/usr/local/bin/pdf2dsc")

;; autocomplete \left( with \right)
(setq LaTeX-electric-left-right-brace t)

;; some settings for aucetex
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)


;; tell emacs where to find ispell (for use in flyspell)
(setq ispell-program-name "/usr/local/bin/ispell")

;; Set transparency of selected and unselected frames
;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
 ;;(set-frame-parameter (selected-frame) 'alpha <both>)
 (set-frame-parameter (selected-frame) 'alpha '(95 . 93))
 (add-to-list 'default-frame-alist '(alpha . (95 . 93)))


;; set up base GIT use in emacs
;; TODO: make this work on mac
;;(add-to-list 'exec-path "C:/Users/azimmer/AppData/Local/GitHub/PortableGit_d76a6a98c9315931ec4927243517bc09e9b731a0/cmd/")
;;(add-to-list 'load-path "C:/Users/azimmer/Documents/HOME/git_emacs")
;;  (require 'git)
;;  (require 'git-blame)

;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~~~~;;
;; settings for magit ;;
;;~~~~~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; magit status
(global-set-key (kbd "C-x g") 'magit-status)

;; magit prefix/help buffer from non-magit buffers/files
(global-set-key (kbd "C-x M-g") 'magit-dispatch)

;; https://magit.vc/manual/magit/How-to-install-the-gitman-info-manual_003f.html
(setq magit-view-git-manual-method 'man)


;; setup isend so we can easily copy code from a buffer and paste it into a terminal buffer!
;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/"))
(put 'downcase-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ~~~~~~~~~~~~~~~~~~~~~~ ;;
;; Org mode configuration ;;
;; ~~~~~~~~~~~~~~~~~~~~~~ ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable Org mode
(require 'org)
;; Make Org mode work with files ending in .org
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; The above is the default in recent emacs

;; make hidden edits not hidden
;; (hidden undo/redo in collapsed headings led me to look into this)
(setq-default org-catch-invisible-edits 'smart) ;; can also set to 'error, 'show, 'show-and-error

;; use the org-bullets to get pretty org bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; clocking
;; set clock to log into CLOCK drawer
;; remove zero time clocked entries
;; clock out when task is marked as done
(setq org-clock-into-drawer "CLOCK"
      org-clock-out-remove-zero-time-clocks t
      org-clock-out-when-done t)

;; set custom workflow words
;; items with a ! get the time logged when that state is entered
;; items with a @ also get a note
(setq org-todo-keywords 
      '((sequence "TODO(t)" "NEXT(n)" "PROJ(p)" "!URGENT!(u)" "SITF(s)" "WAIT(w@/!)" "FLUP(f@/!)" "|"
		  "DONE(d!)" "CANCELLED(c@/!)")))

;; set frequent tags (note: @*s are mutually exclusive)
(setq org-tag-alist '((:startgroup . nil)
                      ("@home" . ?h)  ("@school" . ?s) ("@work" . ?w) ("@errands" . ?e) ("@travel" . ?t)
                      (:endgroup . nil)
                      ("logan" . ?l) ("computer" . ?c) ("phone" . ?p) ("reading" . ?r)))

;; org-mode global key bindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; set org-capture templates
;; use with C-c c
(setq org-capture-templates '(

			       ("i" "Item [inbox]" entry
                               (file+headline "~/Sync/Org/inbox.org" "To File")
                               "* %i%?")

			      ("l" "(my)life Templates")

			      ("li" "Item [my life]" entry
                               (file+headline "~/Sync/Org/mylife.org" "Capture")
                               "* TODO %i%?")
			      ;; ("lI" "Item [inbox]" entry
                              ;;  (file+headline "~/Sync/Org/inbox.org" "To File")
                              ;;  "* TODO %i%?")
                              ("lt" "Tickler" entry
                               (file+headline "~/Sync/Org/tickler_mylife.org" "Tickler")
                               "* %i%? \n %U")
			      ("lj" "Journal" entry
			       (file+datetree "~/Sync/Org/journal_mylife.org")
			       "* %?\nEntered on %U\n  %i\n  %a")
			      ;; ("lr" "Weekly Review" entry
			      ;;  (file+datetree "~/Sync/Org/journal_mylife.org")
			      ;;  (file "~/Sync/Org/CaptureTemplates/tpl-review.txt")
			      ;;  "* %?\nEntered on %U\n  %i\n  %a")

			      ("s" "PhD/School Templates")

			      ("si" "Item [School]" entry
                               (file+headline "~/Sync/Org/school.org" "Capture")
                               "* TODO %i%?")
			      ;; ("sI" "Item [inbox]" entry
                              ;;  (file+headline "~/Sync/Org/inbox.org" "To File")
                              ;;  "* TODO %i%?")
                              ("st" "Tickler" entry
                               (file+headline "~/Sync/Org/tickler_school.org" "Tickler")
                               "* %i%? \n %U")
			      ("sj" "Journal" entry
			       (file+datetree "~/Sync/Org/journal_school.org")
			       "* %?\nEntered on %U\n  %i\n  %a")
			      ;; ("sr" "Weekly Review" entry
			      ;;  (file+datetree "~/Sync/Org/journal_school.org")
			      ;;  (file "~/Sync/Org/CaptureTemplates/tpl-review.txt")
			      ;;  "* %?\nEntered on %U\n  %i\n  %a")

			      ("w" "Work Templates")

			      ("wi" "Item [work]" entry
                               (file+headline "~/Sync/Org/work.org" "Capture")
                               "* TODO %i%?")
			      ;; ("wI" "Item [inbox]" entry
                              ;;  (file+headline "~/Sync/Org/inbox.org" "To File")
                              ;;  "* TODO %i%?")
                              ("wt" "Tickler" entry
                               (file+headline "~/Sync/Org/tickler_work.org" "Tickler")
                               "* %i%? \n %U")
			      ("wj" "Journal" entry
			       (file+datetree "~/Sync/Org/journal_work.org")
			       "* %?\nEntered on %U\n  %i\n  %a")
			      ;; ("wr" "Weekly Review" entry
			      ;;  (file+datetree "~/Sync/Org/journal_work.org")
			      ;;  (file "~/Sync/Org/CaptureTemplates/tpl-review.txt")
			      ;;  "* %?\nEntered on %U\n  %i\n  %a")
			      ;; )

			      ("r" "Weekly Review" entry
			       (file+datetree "~/Sync/Org/journal_mylife.org")
			       (file "~/Sync/Org/CaptureTemplates/tpl-review.txt")
			       "* %?\nEntered on %U\n  %i\n  %a")

			      )
      )

;; set refile targets
;; use with C-c C-w
(setq org-refile-targets '(("~/Sync/Org/mylife.org" :maxlevel . 3)
			   ("~/Sync/Org/school.org" :maxlevel . 2)
			   ("~/Sync/Org/work.org" :maxlevel . 2)
			   ("~/Sync/Org/private_someday.org" :level . 1)
                           ("~/Sync/Org/private_tickler.org" :maxlevel . 2)
                           ("~/Sync/Org/school_someday.org" :level . 1)
                           ("~/Sync/Org/school_tickler.org" :maxlevel . 2)
			   ("~/Sync/Org/work_someday.org" :level . 1)
                           ("~/Sync/Org/work_tickler.org" :maxlevel . 2)))

;; IDs
;; first, add unique property id to all headlines (if it doesn't exist)
;; ids are linked across agenda and archive files!
;; and can be linked to other files specified in: ~/.emacs.d/.org-id-locations
;; then set a shortcut to copy the id of the header the cursor is on

;; (defun my/org-add-ids-to-headlines-in-file ()
;;   "Add ID properties to all headlines in the current file which
;; do not already have one."
;;   (interactive)
;;   (org-map-entries 'org-id-get-create))

;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (add-hook 'before-save-hook 'my/org-add-ids-to-headlines-in-file nil 'local)))

;; copy id of headline
(defun my/copy-id-to-clipboard()
  "Copy the ID property value to killring,
if no ID is there then create a new unique ID.  This function
works only in org-mode buffers.

The purpose of this function is to easily construct id:-links to
org-mode items. If its assigned to a key it saves you marking the
text and copying to the killring."
  (interactive)
  (when (eq major-mode 'org-mode) ; do this only in org-mode buffers
    (setq mytmpid (funcall 'org-id-get-create))
    (kill-new mytmpid)
    (message "Copied %s to killring (clipboard)" mytmpid)
    ))
;; set id copy to C-c i
(global-set-key (kbd "C-c i") 'my/copy-id-to-clipboard)


;; Archiving
;; setup to keep ancestors and tags when archiving
;; https://fuco1.github.io/2017-04-20-Archive-subtrees-under-the-same-hierarchy-as-original-in-the-archive-files.html
(defadvice org-archive-subtree (around fix-hierarchy activate)
  (let* ((fix-archive-p (and (not current-prefix-arg)
                             (not (use-region-p))))
         (afile (org-extract-archive-file (org-get-local-archive-location)))
         (buffer (or (find-buffer-visiting afile) (find-file-noselect afile))))
    ad-do-it
    (when fix-archive-p
      (with-current-buffer buffer
        (goto-char (point-max))
        (while (org-up-heading-safe))
        (let* ((olpath (org-entry-get (point) "ARCHIVE_OLPATH"))
               (path (and olpath (split-string olpath "/")))
               (level 1)
               tree-text)
          (when olpath
            (org-mark-subtree)
            (setq tree-text (buffer-substring (region-beginning) (region-end)))
            (let (this-command) (org-cut-subtree))
            (goto-char (point-min))
            (save-restriction
              (widen)
              (-each path
                (lambda (heading)
                  (if (re-search-forward
                       (rx-to-string
                        `(: bol (repeat ,level "*") (1+ " ") ,heading)) nil t)
                      (org-narrow-to-subtree)
                    (goto-char (point-max))
                    (unless (looking-at "^")
                      (insert "\n"))
                    (insert (make-string level ?*)
                            " "
                            heading
                            "\n"))
                  (cl-incf level)))
              (widen)
              (org-end-of-subtree t t)
              (org-paste-subtree level tree-text))))))))

;; function to narrow to current heading and show parents
;; taken from https://stackoverflow.com/questions/25161792/emacs-org-mode-how-can-i-fold-everything-but-the-current-headline
;; other options would take many keystrokes, eg
;; C-x n s      # org-narrow-to-subtree (bound in org-mode)
;; C-x n d      # narrow-to-defun
;; C-x n w      # widen
(defun org-show-current-heading-tidily ()
  (interactive)  ;Inteactive
  "Show next entry, keeping other entries closed."
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-show-entry) (show-children))
    (outline-back-to-heading)
    (unless (and (bolp) (org-on-heading-p))
      (org-up-heading-safe)
      (hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (show-children)))
(global-set-key "\M-=" 'org-show-current-heading-tidily)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple cursors                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(multiple-cursors-mode 1)

(global-set-key (kbd "C-c m c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;;;;;;;;;;;;;;;;;;
;; expand region ;;
;;;;;;;;;;;;;;;;;;;
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(put 'narrow-to-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; line-highliting for legibility ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-hl-line-mode +1)

;;;;;;;;;;;;;;;;;;;
;; all the icons ;;
;;;;;;;;;;;;;;;;;;;

;; for dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; for neotre
;; see the neotree section

;; for ivy
(all-the-icons-ivy-setup)

;; for gnus buffers
(require 'all-the-icons-gnus)
(all-the-icons-gnus-setup)

;;;;;;;;;;;;;
;; neotree ;;
;;;;;;;;;;;;;
;; nice directory veiwew/navigator

(require 'neotree)
(global-set-key (kbd "C-x n t") 'neotree-toggle) ;; C-x (n)eo-(t)ree

;; icons for windows systems and arrows for terminal
;; requires installing all-the-icons
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;;;;;;;;;;;;;;;;
;; projectile ;;
;;;;;;;;;;;;;;;;
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;;;;;;;;;;;;;;;
;; sublimity ;;
;;;;;;;;;;;;;;;
;; smooth scrolling and minimap

;; (require 'sublimity)
;; (require 'sublimity-scroll)
;; ;; (require 'sublimity-map) ;; experimental
;; ;; (require 'sublimity-attractive)
;; (sublimity-mode 1)

;;;;;;;;;;;
;; tramp ;;
;;;;;;;;;;;
(require 'tramp)
(setq tramp-default-method "ssh")
(setq tramp-default-user "azimmer")
;; see counsel-tramp for more

;;;;;;;;;;;;;
;; origami ;;
;;;;;;;;;;;;;
;; text folding minor mode
(require 'origami)
(origami-mode 1)
(global-set-key (kbd "C-c o") 'origami-toggle-node)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; one command to shrink whitespace ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xah-delete-blank-lines ()
  "Delete all newline around cursor.

URL `http://ergoemacs.org/emacs/emacs_shrink_whitespace.html'
Version 2018-04-02"
  (interactive)
  (let ($p3 $p4)
          (skip-chars-backward "\n")
          (setq $p3 (point))
          (skip-chars-forward "\n")
          (setq $p4 (point))
          (delete-region $p3 $p4)))

(defun xah-shrink-whitespaces ()
  "Remove whitespaces around cursor to just one, or none.

Shrink any neighboring space tab newline characters to 1 or none.
If cursor neighbor has space/tab, toggle between 1 or 0 space.
If cursor neighbor are newline, shrink them to just 1.
If already has just 1 whitespace, delete it.

URL `http://ergoemacs.org/emacs/emacs_shrink_whitespace.html'
Version 2018-04-02T14:38:04-07:00"
  (interactive)
  (let* (
         ($eol-count 0)
         ($p0 (point))
         $p1 ; whitespace begin
         $p2 ; whitespace end
         ($charBefore (char-before))
         ($charAfter (char-after ))
         ($space-neighbor-p (or (eq $charBefore 32) (eq $charBefore 9) (eq $charAfter 32) (eq $charAfter 9)))
         $just-1-space-p
         )
    (skip-chars-backward " \n\t")
    (setq $p1 (point))
    (goto-char $p0)
    (skip-chars-forward " \n\t")
    (setq $p2 (point))
    (goto-char $p1)
    (while (search-forward "\n" $p2 t )
      (setq $eol-count (1+ $eol-count)))
    (setq $just-1-space-p (eq (- $p2 $p1) 1))
    (goto-char $p0)
    (cond
     ((eq $eol-count 0)
      (if $just-1-space-p
          (delete-horizontal-space)
        (progn (delete-horizontal-space)
               (insert " "))))
     ((eq $eol-count 1)
      (if $space-neighbor-p
          (delete-horizontal-space)
        (progn (xah-delete-blank-lines) (insert " "))))
     ((eq $eol-count 2)
      (if $space-neighbor-p
          (delete-horizontal-space)
        (progn
          (xah-delete-blank-lines)
          (insert "\n"))))
     ((> $eol-count 2)
      (if $space-neighbor-p
          (delete-horizontal-space)
        (progn
          (goto-char $p2)
          (search-backward "\n" )
          (delete-region $p1 (point))
          (insert "\n"))))
     (t (progn
          (message "nothing done. logic error 40873. shouldn't reach here" ))))))
(global-set-key (kbd "C-c s") 'xah-shrink-whitespaces) ; Ctrl+c s

;;;;;;;;;;
;; helm ;;
;;;;;;;;;;

;; helm global bindings
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)

;; helm config options

;; enable helm config
;; (require 'helm-config)
;; helm through the buffer list
;; (global-set-key (kbd "C-x b") 'helm-buffers-list)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ivy, swiper, counsel ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; from https://sam217pa.github.io/2016/09/13/from-helm-to-ivy/
;; and https://truthseekers.io/lessons/how-to-use-ivy-swiper-counsel-in-emacs-for-noobs/

;; turn on ivy-mode globally
(ivy-mode 1)
;; add 'recentf-mode' and bookmarks to 'ivy-switch-buffer'
(setq ivy-use-virtual-buffers t)
;; no idea, but recommended by project maintainer
(setq enable-recursive-minibuffers t)
;; number of result lines to display
(setq ivy-height 10)
;; does not count candidates
(setq ivy-count-format "%d/%d ")
;; set tab to keep completing (default behavior is to complete and then RETURN on second TAB)
;; (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial)
;; configure regexp engine
(setq ivy-re-builders-alist
      ;; allow input not in order
      '((t   . ivy--regex-ignore-order)))
;; set better fuzzy matching with flx
(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))
;; ditch ^ default at start of ivy to go full fuzz
(setq ivy-initial-inputs-alist nil)


;; turn on ivy-rich
(require 'ivy-rich)
(ivy-rich-mode 1)
;; no idea, reccomeded by maintainer
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
;; set ivy-rich to show abbrev (as opposed to default relative) paths
(setq ivy-rich-path-style 'abbrev)

;; turn on flyspell-correct-ivy
(require 'flyspell-correct-ivy) 
(define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-wrapper)

;; replace isearch with swiper
(global-set-key "\C-s" 'swiper)

;; Gives M-x command counsel features
;; NOTE: if smex is installed, counsel-M-x will automatically use it.
(global-set-key (kbd "M-x") 'counsel-M-x)
;; gives C-x C-f counsel features
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

;; setup counsel-tramp
;; (global-set-key (kbd "C-c t") 'counsel-tramp)

;; as per https://github.com/masasam/emacs-anything-tramp, these should speed tramp up:
;; (add-hook 'counsel-tramp-pre-command-hook '(lambda () (global-aggressive-indent-mode 0)
;; 				     (projectile-mode 0)
;; 				     (editorconfig-mode 0)))
;; (add-hook 'counsel-tramp-quit-hook '(lambda () (global-aggressive-indent-mode 1)
;; 			      (projectile-mode 1)
;; 			      (editorconfig-mode 1)))

;; customize actions during counsel-*
;; ;;set action options during execution of counsel-find-file
;; ;; replace "frame" with window to open in new window
;; (ivy-set-actions
;;  'counsel-find-file
;;  '(("j" find-file-other-frame "other frame")
;;    ("b" counsel-find-file-cd-bookmark-action "cd bookmark")
;;    ("x" counsel-find-file-extern "open externally")
;;    ("d" delete-file "delete")
;;    ("r" counsel-find-file-as-root "open as root")))
 
;; ;; set actions when running C-x b
;; ;; replace "frame" with window to open in new window
;; (ivy-set-actions
;;  'ivy-switch-buffer
;;  '(("j" switch-to-buffer-other-frame "other frame")
;;    ("k" kill-buffer "kill")
;;    ("r" ivy--rename-buffer-action "rename")))


;;
;;
;; end ivy, swiper, counsel
