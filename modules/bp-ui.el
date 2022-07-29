;; -*- lexical-binding: t; -*-
;;; UI Adjustments

(defun bp/fill-visual-column ()
  (interactive)
  (setq visual-fill-column-center-text t
        visual-fill-column-width 180)
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



(defun bp/make-frame-pretty ()
  "Set the initial look and feel of the frame"
  (modify-all-frames-parameters
   '((right-divider-width . 24)
     (alpha . (95 . 75))
     (mouse-color . "white")
     (internal-border-width . 24))))

(add-hook 'before-make-frame-hook 'bp/make-frame-pretty)

(setq initial-frame-alist
      '((right-divider-width . 24)
        (alpha . (95 . 75))
        (internal-border-width. 24)))

(add-to-list 'default-frame-alist '(internal-border-width . 24))
(add-to-list 'default-frame-alist '(alpha . (95 . 75)))
(add-to-list 'default-frame-alist '(right-divider-width . 24))


(require 'winum)
(global-set-key (kbd "M-0") 'treemacs-select-window)
(global-set-key (kbd "M-1") 'winum-select-window-1)
(global-set-key (kbd "M-2") 'winum-select-window-2)
(global-set-key (kbd "M-3") 'winum-select-window-3)
(global-set-key (kbd "M-4") 'winum-select-window-4)
(global-set-key (kbd "M-5") 'winum-select-window-5)
(global-set-key (kbd "M-6") 'winum-select-window-6)
(global-set-key (kbd "M-7") 'winum-select-window-7)
(global-set-key (kbd "M-8") 'winum-select-window-8)
(winum-mode 1)

(require 'vertico-posframe)
(setq vertico-posframe-parameters '((left-fringe . 16)
                                (right-fringe . 16)))
(vertico-posframe-mode 1)

(require 'dimmer)
(dimmer-configure-which-key)
(dimmer-configure-posframe)
(dimmer-mode nil)

;; Font Settings
(setq doom-font (font-spec :family "Iosevka Term" :size 16)
      doom-variable-pitch-font (font-spec :family  "Iosevka Term" :size 16)
      doom-big-font (font-spec :family "Iosevka Term" :size 24))

;; TODO: Get this refactored out
(use-package! nano-theme)
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))
(set-face-foreground 'window-divider nano-dark-background)
(set-face-foreground 'window-divider-first-pixel nano-dark-background)
(set-face-foreground 'window-divider-last-pixel nano-dark-background)


;; Set theme                               ;
(load-theme 'doom-nord t)
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))


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

;; Removed dupe 'evil-' from which-key labels
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))))

;; Modeline Config
(after! doom-modeline
  (setq doom-modeline-height 36
        doom-modeline-bar-width 6
        doom-modeline-lsp t
        doom-modeline-github nil
        doom-modeline-mu4e t
        doom-modeline-irc t
        doom-modeline-minor-modes nil
        doom-modeline-persp-name t
        doom-modeline-buffer-file-name-style 'truncate-except-project
        doom-modeline-major-mode-icon t))

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

​(setq counsel-linux-app-format-function 'counsel-linux-app-format-function-name-pretty)

(provide 'bp-ui)
