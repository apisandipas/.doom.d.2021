;; -*- lexical-binding: t; -*-
;;; UI Adjustments

(defun bp/fill-visual-column ()
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


(set-face-foreground 'window-divider "#cccccc")

(defun bp/make-frame-pretty ()
  "Set the initial look and feel of the frame"
  (modify-all-frames-parameters
   '((right-divider-width . 24)
     (alpha-background . 100)
     (mouse-color . "white")
     (internal-border-width . 24))))

(add-hook 'before-make-frame-hook 'bp/make-frame-pretty)

(setq initial-frame-alist
      '((right-divider-width . 24)
        (alpha-background . 100)
        (internal-border-width. 24)))

(add-to-list 'default-frame-alist '(internal-border-width . 24))
(add-to-list 'default-frame-alist '(alpha-background . 100))
(add-to-list 'default-frame-alist '(right-divider-width . 24))

(use-package! winum
  :config
  (global-set-key (kbd "M-0") 'treemacs-select-window)
  (global-set-key (kbd "M-1") 'winum-select-window-1)
  (global-set-key (kbd "M-2") 'winum-select-window-2)
  (global-set-key (kbd "M-3") 'winum-select-window-3)
  (global-set-key (kbd "M-4") 'winum-select-window-4)
  (global-set-key (kbd "M-5") 'winum-select-window-5)
  (global-set-key (kbd "M-6") 'winum-select-window-6)
  (global-set-key (kbd "M-7") 'winum-select-window-7)
  (global-set-key (kbd "M-8") 'winum-select-window-8)
  (winum-mode 1))

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

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

;; Set theme                               ;
;; (load-theme 'doom-moonlight t)
;; (after! doom-themes
;;   (setq doom-themes-enable-bold t
;;         doom-themes-enable-italic t))
(use-package! modus-themes
  :demand
  :init
  (require 'modus-themes)

  (setq modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))
  (setq modus-themes-italic-constructs t
            modus-themes-bold-constructs t
            modus-themes-variable-pitch-ui t
            modus-themes-mixed-fonts t)

  ;; Color customizations
  (setq modus-themes-prompts '(bold))
  (setq modus-themes-completions nil)
  (setq modus-themes-org-blocks 'tinted-background) ;'gray-background)

  ;; Font sizes for titles and headings, including org
  (setq modus-themes-headings '((1 . (light variable-pitch 1.5))
                                (agenda-date . (1.3))
                                (agenda-structure . (variable-pitch light 1.8))
                                                        (t . (medium))))
  ;; Theme overrides
  (customize-set-variable 'modus-themes-common-palette-overrides
                          `(
                            ;; Make the mode-line borderless
                            (bg-mode-line-active bg-inactive)
                            (fg-mode-line-active fg-main)
                            (bg-mode-line-inactive bg-inactive)
                            (fg-mode-line-active fg-dim)
                            (border-mode-line-active bg-inactive)
                            (border-mode-line-inactive bg-main)
                            ))
  (customize-set-variable 'modus-operandi-tinted-palette-overrides
                          `(
                            ;; More subtle gray for the inactive window and modeline
                            (bg-inactive "#efefef")))
  (customize-set-variable 'modus-vivendi-tinted-palette-overrides
                          `(
                            ;; More subtle gray for the inactive window and modeline
                            (bg-inactive "#202020")))

  ;; Quick fix: Don't load immediately, but during after-init-hook so all other modifications further down can be prepared
  (defun ct/modus-themes-init ()
    (load-theme (car modus-themes-to-toggle)))

  :bind ("<f5>" . modus-themes-toggle)
  :hook (after-init . ct/modus-themes-init))


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

(after! counsel
  (setq counsel-linux-app-format-function 'counsel-linux-app-format-function-name-pretty))

;; Ensure help and other buffers open to the right in a tall split
;; (set-popup-rules!
;;   '(("^\\*\\([Hh]elp\\|Apropos\\)"
;;      :slot 20 :side right :size 0.5 :select t :quit t)
;;     ("^CAPTURE.*\\.org$"
;;      :slot 20 :side right :size 0.5 :select t)
;;     ("^\\*Org Src"
;;      :slot 20 :side right :size 0.5 :select t)
;;     ("^\\*info\\*$"
;;      :slot 20 :side right :size 0.5 :select t :quit t)))

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


(defun emacs-run-launcher ()
  "Run the app-launcer in its own frame. for use outside of EXWM"
  (interactive)
  (with-selected-frame (make-frame '((name . "emacs-run-launcher")
                                     (minibuffer . only)
                                     (width . 120)
                                     (height . 11)))
    (unwind-protect
        (counsel-linux-app)
      (delete-frame))))

(provide 'bp-ui)
