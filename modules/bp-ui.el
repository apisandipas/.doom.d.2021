;;; package --- Summary -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; UI Adjustments

;;; Code:

(defun bp/fill-visual-column ()
  "Set the width of the content centered in org-mode."
  (interactive)
  (setq visual-fill-column-center-text t
        visual-fill-column-width 120)
  (visual-fill-column-mode 1))

(add-hook! org-mode 'bp/fill-visual-column)
(add-hook! markdown-mode 'bp/fill-visual-column)


(setq display-line-numbers-type 'relative)
(setq standard-indent 2)
(setq-default indent-tabs-mode nil)

(setq window-divider-default-right-width 24)
(setq window-divider-default-bottom-width 1)
(setq window-divider-default-places 'right-only)
(window-divider-mode t)
;; Make sure new frames use window-divider
(add-hook 'before-make-frame-hook 'window-divider-mode)


;; (set-face-foreground 'window-divider "#11111b")

(defun bp/make-frame-pretty ()
  "Set the initial look and feel of the frame."
  (modify-all-frames-parameters
   '((right-divider-width . 24)
     (alpha-background . 100)
     (mouse-color . "white")
     (internal-border-width . 24))))

(add-hook 'before-make-frame-hook 'bp/make-frame-pretty)

(setq default-frame-alist '((vertical-scroll-bars . nil)
                            (internal-border-width . 24)
                            (left-fringe . 0)
                            (right-fringe . 0)
                            (tool-bar-lines . 0)
                            (menu-bar-lines . 0)))

;; Default frame settings
(setq initial-frame-alist default-frame-alist)

(require 'vertico-posframe)
(setq vertico-posframe-parameters '((left-fringe . 16)
                                    (right-fringe . 16)))
(vertico-posframe-mode 1)


;; (require 'dimmer)
;; (dimmer-configure-which-key)
;; (dimmer-configure-posframe)
;; (dimmer-mode nil)

;; Font Settings
;; (setq doom-font (font-spec :family "Iosevka Term" :size 16)
;;       doom-variable-pitch-font (font-spec :family  "Iosevka Term" :size 16)
;;       doom-big-font (font-spec :family "Iosevka Term" :size 24))

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))


(use-package! ligature
  :config
  (ligature-set-ligatures
   'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                "\\\\" "://"))
  (global-ligature-mode t))

;; Removed dupe 'evil-' from which-key labels
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))))

;; Modeline Config
;; (after! doom-modeline
;;   (setq doom-modeline-height 36
;;         doom-modeline-bar-width 6
;;         doom-modeline-lsp t
;;         doom-modeline-github nil
;;         doom-modeline-mu4e t
;;         doom-modeline-irc t
;;         doom-modeline-minor-modes nil
;;         doom-modeline-persp-name t
;;         doom-modeline-buffer-file-name-style 'truncate-except-project
;;         doom-modeline-major-mode-icon t))

(use-package! dashboard
  :init
  (setq dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-center-content t
        dashboard-week-agenda nil
        dashboard-items '((recents  . 5)
                          (agenda . 5))
        dashboard-startup-banner "~/.doom.d/banners/gnu.png")
  :config
  (dashboard-setup-startup-hook))

(after! counsel
  (setq counsel-linux-app-format-function 'counsel-linux-app-format-function-name-pretty))

;; Ensure help and other buffers open to the right in a tall split
(set-popup-rules!
 '(("^\\*\\([Hh]elp\\|Apropos\\)"
    :slot 20 :side right :size 0.5 :select t :quit t)
   ("^CAPTURE.*\\.org$"
    :slot 20 :side right :size 0.5 :select t)
   ("^\\*Org Src"
    :slot 20 :side right :size 0.5 :select t)
   ("^\\*info\\*$"
    :slot 20 :side right :size 0.5 :select t :quit t)))

(use-package popper
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package! svg-tag-mode
  :hook (org-mode. svg-tag-mode)
  :config
  (use-package! svg-lib)
  (setq svg-tag-tags
        '((":TODO:" . ((svg-tag-make "TODO" :face 'org-tag
                                     :radius 0 :inverse t :margin 0)))
          (":NOTE:" . ((svg-tag-make "NOTE" :face 'font-lock-comment-face
                                     :inverse nil :margin 0 :radius 0)))
          ("\([0-9a-zA-Z]\)" . ((lambda (tag)
                                  (svg-tag-make tag :beg 1 :end -1 :radius 12))))
          ("\([0-9a-zA-Z][0-9a-zA-Z]\)" . ((lambda (tag)
                                             (svg-tag-make tag :beg 1 :end -1 :radius 8))))
          ("|[0-9a-zA-Z- ]+?|" . ((lambda (tag)
                                    (svg-tag-make tag :face 'font-lock-comment-face
                                                  :margin 0 :beg 1 :end -1))))
 ;; Org tags
        ;; (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
        ;; (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))

        ;; Task priority
        ("\\[#[A-Z]\\]" . ( (lambda (tag)
                              (svg-tag-make tag :face 'org-priority
                                            :beg 2 :end -1 :margin 0))))

        ;; Progress
        ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                            (svg-progress-percent (substring tag 1 -2)))))
        ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                          (svg-progress-count (substring tag 1 -1)))))

        ;; TODO / DONE
        ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
        ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))


        ;; Citation of the form [cite:@Knuth:1984]
        ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                          (svg-tag-make tag
                                                        :inverse t
                                                        :beg 7 :end -1
                                                        :crop-right t))))
        ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                (svg-tag-make tag
                                                              :end -1
                                                              :crop-left t))))))
  (svg-tag-mode t))

(provide 'bp-ui)

;;; bp-ui.el ends here.
