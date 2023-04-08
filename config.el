; -*- lexical-binding: t; -*-

(setq user-full-name "Bryan Arden Paronto"
      user-mail-address "bryan@cablecar.digital")

(add-to-list 'load-path "~/.doom.d/modules/")

(after! doom-themes
  ;; (setq doom-theme 'doom-nano-dark)
  (load-theme 'doom-nano-dark t)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (set-face-attribute 'default nil :family "Roboto Mono" :weight 'light :height 120)
  (set-face-attribute 'bold nil :family "Roboto Mono" :weight 'regular)
  (set-face-attribute 'italic nil :family "Victor Mono" :weight 'semilight :slant 'italic)
  (set-fontset-font t 'unicode (font-spec :name "Inconsolata Light" :size 12) nil)
  (set-fontset-font t '(#xe000 . #xffdd) (font-spec :name "RobotoMono Nerd Font" :size 12) nil))

(use-package! doom-nano-modeline
  :config
  (doom-nano-modeline-mode 1)
  (global-hide-mode-line-mode 1))

(require 'bp-keybinds)
(require 'bp-ui)
(require 'bp-org)
(require 'bp-roam)
(require 'bp-rss)
;; (require 'bp-mastodon)
;; (require 'bp-email)
;; (require 'bp-streaming)
;; (require 'bp-presentations)
;; (require 'emacs-with-nyxt)

;; (use-package! eshell
;;   :config
;;   (setq eshell-rc-script "~/.doom.d/eshell/profile"
;;         eshell-aliases-file "~/.doom.d/eshell/aliases"
;;         eshell-history-size 5000
;;         eshell-buffer-maximum-lines 5000
;;         eshell-hist-ignoredups t
;;         eshell-scroll-to-bottom-on-input t
;;         eshell-destroy-buffer-when-process-dies t))

;; (setq inferior-lisp-program "sbcl"
;;       web-mode-markup-indent-offset 2
;;       web-mode-code-indent-offset 2
;;       web-mode-css-indent-offset 2
;;       mac-command-modifier 'meta
;;       js-indent-level 2
;;       typescript-indent-level 2
;;       json-reformat:indent-width 2
;;       prettier-js-args '("--double-quote"))

;; (add-hook 'js2-mode-hook 'prettier-js-mode)
;; (add-hook 'typescript-mode-hook 'prettier-js-mode)
;; (add-hook 'typescript-tsx-mode-hook 'prettier-js-mode)
;; (add-hook 'web-mode-hook 'prettier-js-mode)

;; (after! circe
;;   (set-irc-server! "irc.libera.chat"
;;     '(:tls t
;;       :port 6697
;;       :nick "apis_and_ipas"
;;       :sasl-username ,(+pass-get-user "Chats/Libera")
;;       :sasl-password (lambda (&rest _) (+pass-get-secret "Chats/Libera"))
;;       :channels ("#emacs" "#chat" "#javascript" "#guix" "#react" "#chicago" "#tropin"))))


;; (use-package! youtube-dl)
;; ;; (use-package! engine-mode)

;; ;; (defengine duckduckgo
;; ;;   "https://duckduckgo.com/?q=%s"
;; ;;   :keybinding "n"
;; ;;   :browser 'browse-url-nyxt)
;; (setq browse-url-browser-function 'browse-url-firefox)


;; ;; Ensure help and other buffers open to the right in a tall split
(set-popup-rules!
  '(("^\\*\\([Hh]elp\\|Apropos\\)"
     :slot 20 :side right :size 0.5 :select t :quit t)
    ("^CAPTURE.*\\.org$"
     :slot 20 :side right :size 0.5 :select t)
    ("^\\*Org Src"
     :slot 20 :side right :size 0.5 :select t)
    ("^\\*info\\*$"
     :slot 20 :side right :size 0.5 :select t :quit t)))

