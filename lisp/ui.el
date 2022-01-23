;;; UI Adjustments

(setq doom-modeline-height 36)
(setq centaur-tabs-set-icons t)
(setq centaur-tabs-height 36)

(set-frame-parameter (selected-frame)'alpha '(75 . 80))
(set-frame-parameter (selected-frame)'internal-border-width 24)
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

(add-to-list 'default-frame-alist '(alpha . (75 . 80)))
(add-to-list 'default-frame-alist '(internal-border-width . 24))

(setq display-line-numbers-type 'relative)

;;;;; Font Settings
(setq doom-font
      (font-spec
       :family "VictorMono Nerd Font" :size 20)
      doom-variable-pitch-font
      (font-spec
       :family  "VictorMono Nerd Font" :size 20)
      doom-big-font
      (font-spec
       :family "VictorMono Nerd Font" :size 20))

;;;;; Set Indentation Default
(setq standard-indent 2)

;;;;; Set theme
(setq doom-theme 'doom-vibrant)
;; (setq doom-theme 'modus-vivendi)

(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(use-package ligature
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

;;;;; Prefer vertial splits
;; (setq split-height-threshold nil)
;; (setq split-width-threshold 0)

(defun bp/prefer-horizontal-split ()
  (set-variable 'split-height-threshold nil t)
  (set-variable 'split-width-threshold 40 t))
(add-hook! 'markdown-mode-hook #'bp/prefer-horizontal-split)
(add-hook! 'org-mode-hook #'bp/prefer-horizontal-split)
(add-hook! 'prog-mode-hook #'bp/prefer-horizontal-split)

;; Removed dupe 'evil-' from which-key labels
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))
;;
;; Modeline Config
(add-hook! 'after-init-hook
  (lambda ()  (setq doom-modeline-height 36
                    doom-modeline-bar-width 6
                    doom-modeline-lsp t
                    doom-modeline-github t
                    doom-modeline-mu4e t
                    doom-modeline-irc t
                    doom-modeline-minor-modes nil
                    doom-modeline-persp-name nil
                    doom-modeline-buffer-file-name-style 'truncate-except-project
                    doom-modeline-major-mode-icon nil)
    (custom-set-faces '(mode-line ((t (:height 0.85))))
                      '(mode-line-inactive ((t (:height 0.85)))))))

;; ;; ;; ;; -
(use-package all-the-icons-ivy
  :after (all-the-icons ivy)
  :custom (all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window))
  :config
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-dired-jump)
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-find-library)
  (all-the-icons-ivy-setup))

(use-package ivy-posframe
  :after (ivy)
  :config
  (setq ivy-posframe-display-functions-alist
        '((t . ivy-posframe-display-at-frame-center)))
  (ivy-posframe-mode 1))

(defun bp/fill-visual-column ()
  (setq visual-fill-column-center-text t
        visual-fill-column-width 120)
  (visual-fill-column-mode 1))

(use-package dashboard
  :init
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-week-agenda nil)
  (setq dashboard-startup-banner "~/.doom.d/banners/gnu.png")
  (add-hook! 'dashboard-mode-hook #'bp/fill-visual-column)
  :config
  (dashboard-setup-startup-hook))

(add-hook! org-mode #'bp/fill-visual-column)


(defun bp/presentation-mode ()
  (setq-local face-remapping-alist '(
        (header-line (:height 5) variable-pitch)
        (org-document-title (:height 1.75) org-document-title)
        (org-code (:height 1.55) org-code)
        (org-verbatim (:height 1.55) org-verbatim)
        (org-block (:height 1.25) org-block)
        (org-block-begin-line (:height 0.7) org-block)))
  (setq header-line-format " ")
  (org-appear-mode -1)
  (org-image-actual-width nil)
  (org-display-inline-images)
  (org-tree-slide-header nil)
  (display-line-numbers-mode nil))

(use-package org-tree-slide
  :custom
  (add-hook! org-tree-slide-mode 'bp/presentation-mode)
  )


(defun bp/presentation-mode ()
  (setq-local face-remapping-alist '(
        (header-line (:height 5) variable-pitch)
        (org-document-title (:height 1.75) org-document-title)
        (org-code (:height 1.55) org-code)
        (org-verbatim (:height 1.55) org-verbatim)
        (org-block (:height 1.25) org-block)
        (org-block-begin-line (:height 0.7) org-block)))
  (setq header-line-format " ")
  (org-appear-mode -1)
  (org-image-actual-width nil)
  (org-display-inline-images)
  (org-tree-slide-header nil)
  (display-line-numbers-mode nil))

(use-package org-tree-slide
  :custom
  (add-hook! org-tree-slide-mode 'bp/presentation-mode)
  )


(defun bp/presentation-mode ()
  (setq-local face-remapping-alist '(
        (header-line (:height 5) variable-pitch)
        (org-document-title (:height 1.75) org-document-title)
        (org-code (:height 1.55) org-code)
        (org-verbatim (:height 1.55) org-verbatim)
        (org-block (:height 1.25) org-block)
        (org-block-begin-line (:height 0.7) org-block)))
  (setq header-line-format " ")
  (org-appear-mode -1)
  (org-image-actual-width nil)
  (org-display-inline-images)
  (org-tree-slide-header nil)
  (display-line-numbers-mode nil))

(use-package org-tree-slide
  :custom
  (add-hook! org-tree-slide-mode 'bp/presentation-mode)
  )
