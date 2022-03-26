;; -*- lexical-binding: t; -*-
;;; UI Adjustments

(add-to-list 'default-frame-alist '(mouse-color . "white"))
(set-frame-parameter (selected-frame) 'alpha '(75 . 80))
(add-to-list 'default-frame-alist '(alpha . (75 . 80)))

(setq display-line-numbers-type 'relative)

(setq standard-indent 2)

;; Font Settings
(setq doom-font
      (font-spec
       :family "VictorMono Nerd Font" :size 18)
      doom-variable-pitch-font
      (font-spec
       :family  "VictorMono Nerd Font" :size 18)
      doom-big-font
      (font-spec
       :family "VictorMono Nerd Font" :size 20))

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(setq garbage-collection-messages nil)


;;;;; Set theme
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  ;; (setq doom-theme 'doom-vibrant)
  ;; (setq doom-theme 'modus-vivendi)
  (setq doom-theme 'doom-moonlight))


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

(defun split-horizontally-for-temp-buffers ()
  "Split the window horizontally for temp buffers."
  (when (one-window-p t)
    (split-window-horizontally)))
(add-hook 'temp-buffer-setup-hook 'split-horizontally-for-temp-buffers)

;; Removed dupe 'evil-' from which-key labels
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))
;;
;; Modeline Config
(add-hook! after-init
  (lambda ()  (setq doom-modeline-height 36
                    doom-modeline-bar-width 6
                    doom-modeline-lsp t
                    doom-modeline-github nil
                    doom-modeline-mu4e nil
                    doom-modeline-irc nil
                    doom-modeline-minor-modes nil
                    doom-modeline-persp-name t
                    doom-modeline-buffer-file-name-style 'truncate-except-project
                    doom-modeline-major-mode-icon t)
    (custom-set-faces '(mode-line ((t (:height 0.85))))
                      '(mode-line-inactive ((t (:height 0.85)))))))

;; (use-package! visual-fill-column
;;   :config
;;   (setq visual-fill-column-center-text t
;;         visual-fill-column-width 120)
(defun bp/fill-visual-column ()
  (setq visual-fill-column-center-text t
        visual-fill-column-width 120)
  (visual-fill-column-mode 1))

  (add-hook! org-mode #'bp/fill-visual-column)
  (add-hook! markdown-mode #'bp/fill-visual-column)
  (add-hook! dashboard-mode #'bp/fill-visual-column)



(use-package! dashboard
  :init
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-week-agenda nil)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 0)
                          (projects . 5)
                          (agenda . 5)))
  (setq dashboard-startup-banner "~/.doom.d/banners/gnu.png")
  ;; (add-hook! 'dashboard-mode-hook #'bp/fill-visual-column)
  :config
  (dashboard-setup-startup-hook))

(use-package! vertigo-posframe
  :after vertigo
  :config
  (setq vertico-posframe-parameters
        '((left-fringe . 16)
          (right-fringe . 16)))
  (vertico-posframe-mode 1))

(use-package! auto-dim-other-buffers
  :config
  (set-face-attribute 'auto-dim-other-buffers-face nil :background "#242837")

  (add-hook! after-init #'auto-dim-other-buffers-mode))

(global-set-key (kbd "M-0") 'treemacs-select-window)
(global-set-key (kbd "M-1") 'winum-select-window-1)
(global-set-key (kbd "M-2") 'winum-select-window-2)
(global-set-key (kbd "M-3") 'winum-select-window-3)
(global-set-key (kbd "M-4") 'winum-select-window-4)
(global-set-key (kbd "M-5") 'winum-select-window-5)
(global-set-key (kbd "M-6") 'winum-select-window-6)
(global-set-key (kbd "M-7") 'winum-select-window-7)
(global-set-key (kbd "M-8") 'winum-select-window-8)

(require 'winum)
(winum-mode 1)

;;; Causes issues w treemacs, disables by default
(use-package! edwina
  :config
  (setq display-buffer-base-action '(display-buffer-below-selected))
  (edwina-setup-dwm-keys))


(provide 'bp-ui)
