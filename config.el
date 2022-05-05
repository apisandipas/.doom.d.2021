;; -*- lexical-binding: t; -*-

(server-start)

(setq garbage-collection-messages nil)

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

;; Always open help in the main window frame
(add-to-list 'display-buffer-alist
        '("*Help*" display-buffer-same-window))
;; (setq org-tags-column -77)
(setq default-tab-width 2)
(setq typescript-indent-level 2)

(after! eshell
  (setq eshell-rc-script "~/.doom.d/eshell/profile"
        eshell-aliases-file "~/.doom.d/eshell/aliases"
        eshell-history-size 5000
        eshell-buffer-maximum-lines 5000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t
        eshell-destroy-buffer-when-process-dies t))

(setq inferior-lisp-program "sbcl")

(after! circe
  (set-irc-server! "irc.libera.chat"
    '(:tls t
      :port 6697
      :nick "apis_and_ipas"
      :sasl-username ,(+pass-get-user "Chats/Libera")
      :sasl-password (lambda (&rest _) (+pass-get-secret "Chats/Libera"))
      :channels ("#emacs" "#chat" "#javascript" "#guix" "#react" "#chicago"))))

;; Make comments more readable.
;; TODO: Change to a better color. This one is #bada55 :D
(set-face-attribute 'font-lock-comment-face nil
                    :foreground "#bada55")

(use-package! youtube-dl)
(use-package! engine-mode)

;; (defengine duckduckgo
;;   "https://duckduckgo.com/?q=%s"
;;   :keybinding "n"
;;   :browser 'browse-url-nyxt)
(setq browse-url-browser-function 'browse-url-firefox)
