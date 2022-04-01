;; -*- lexical-binding: t; -*-

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

(setq org-tags-column -77)

(after! eshell
  (setq eshell-rc-script "~/.doom.d/eshell/profile"
        eshell-aliases-file "~/.doom.d/eshell/aliases"
        eshell-history-size 5000
        eshell-buffer-maximum-lines 5000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t
        eshell-destroy-buffer-when-process-dies t))

;; Create a variable for our preferred tab width
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)
(setq-default indent-tabs-mode nil)

(setq inferior-lisp-program "sbcl")

;; Supress GC notices
(setq garbage-collection-messages nil)

(or (get-buffer "*dashboard*")
  (get-buffer "*scratch*"))

(after! circe
  (set-irc-server! "irc.libera.chat"
    '(:tls t
      :port 6697
      :sasl-username "apis_and_ipas"
      :sasl-password "qwerty1234"
      :channels ("#emacs" "#chat" "#javascript" "#guix" "#react" "#chicago"))))

;; Make comments more readable.
;; TODO: Change to a better color. This one is #bada55 :D
(set-face-attribute 'font-lock-comment-face nil
                    :foreground "#bada55")

(use-package! youtube-dl)
(use-package! engine-mode)

(defengine duckduckgo
  "https://duckduckgo.com/?q=%s"
  :keybinding "n"
  :browser 'browse-url-nyxt)
(setq browse-url-browser-function 'browse-url-nyxt)
