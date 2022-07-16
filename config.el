;; -*- lexical-binding: t; -*-

(add-to-list 'load-path "~/.doom.d/modules/")

(require 'bp-keybinds)
(require 'bp-ui)
(require 'bp-org)
(require 'bp-roam)
(require 'bp-rss)
(require 'bp-email)
(require 'bp-presentations)
(require 'bp-streaming)
(require 'emacs-with-nyxt)

;; Customizations relavent to the nanotheme
(require 'bp-nano)

(use-package! eshell
  :config
  (setq eshell-rc-script "~/.doom.d/eshell/profile"
        eshell-aliases-file "~/.doom.d/eshell/aliases"
        eshell-history-size 5000
        eshell-buffer-maximum-lines 5000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t
        eshell-destroy-buffer-when-process-dies t))

(setq inferior-lisp-program "sbcl")

(use-package! circe
  :config
  (set-irc-server! "irc.libera.chat"
    '(:tls t
      :port 6697
      :nick "apis_and_ipas"
      :sasl-username ,(+pass-get-user "Chats/Libera")
      :sasl-password (lambda (&rest _) (+pass-get-secret "Chats/Libera"))
      :channels ("#emacs" "#chat" "#javascript" "#guix" "#react" "#chicago" "#tropin"))))


(use-package! youtube-dl)
;; (use-package! engine-mode)

;; (defengine duckduckgo
;;   "https://duckduckgo.com/?q=%s"
;;   :keybinding "n"
;;   :browser 'browse-url-nyxt)
(setq browse-url-browser-function 'browse-url-firefox)

