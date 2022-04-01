;; -*- lexical-binding: t; -*-
;;; UI Adjustments

(defun bp/fill-visual-column ()
  (setq visual-fill-column-center-text t
        visual-fill-column-width 120)
  (visual-fill-column-mode 1))

;; This remains untestes as of 22-04-01
(defun split-vertically-for-temp-buffers ()
  "Split the window horizontally for temp buffers."
  (when (one-window-p t)
    (split-window-vertically)))

;; This assures all frame open with the same paremeters, not just the first one.
(use-package! emacs
  :config
  (setf org-auto-align-tags nil)
  (setq display-line-numbers-type 'relative)
  (setq standard-indent 2)
  (setq-default indent-tabs-mode nil)

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
  (dimmer-mode t)


  (add-hook 'temp-buffer-setup-hook 'split-vertically-for-temp-buffers)

  (add-hook! org-mode #'bp/fill-visual-column)
  (add-hook! markdown-mode #'bp/fill-visual-column))

;; Font Settings
(setq doom-font
      (font-spec
       :family "VictorMono Nerd Font" :size 20)
      doom-variable-pitch-font
      (font-spec
       :family  "VictorMono Nerd Font" :size 20)
      doom-big-font
      (font-spec
       :family "VictorMono Nerd Font" :size 20))

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

;;;;; Set theme
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-vibrant t))

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
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))

;; Modeline Config
(use-package! doom-modeline
  :config
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



;;  Not used currenrly as it
;;; Causes issues w treemacs, disables by default
;; (use-package! edwina
;;   :config
;;   (setq display-buffer-base-action '(display-buffer-below-selected))
;;   (edwina-setup-dwm-keys))


(provide 'bp-ui)
