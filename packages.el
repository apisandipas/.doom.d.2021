;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.
(package! s)                            ;; Is this used?

;; Org Mode packages
(unpin! org-roam)
(package! org-roam-ui)
(package! org-protocol-capture-html)
(package! org-present)
(package! org-auto-tangle)
(package! org-superstar)
(package! org-super-agenda)
(package! toc-org)
(package! org-appear)
(package! org-mobile-sync)
(package! ob-typescript)
(package! doct)

;; UI Enhancing Packages
(package! visual-fill-column)
(package! dashboard)
(package! vertico-posframe)
(package! dimmer)
(package! winum)
(package! ligature
  :recipe
  (:host github
   :repo "mickeynp/ligature.el"))

;; Language-specific packages
(package! prettier-js)

;; Themes
(package! one-themes)
(package! leuven-theme)


;;; Elfeed Packages
(package! elfeed-dashboard)
(package! elfeed-goodies)

;; EXWM Related Packages
(package! exwm)
(package! desktop-environment)
(package! exwm-outer-gaps
  :recipe
  (:host github
   :repo "lucasgruss/exwm-outer-gaps"))
(package! exwm-firefox-evil)
(package! doom-modeline-exwm
  :recipe
  (:host github
   :repo "elken/doom-modeline-exwm"))

;; Mu4e enhancements
(package! mu4e-thread-folding
  :recipe
  (:host github
   :repo "rougier/mu4e-thread-folding"))
(package! mu4e-dashboard
  :recipe
  (:host github
   :repo "rougier/mu4e-dashboard"))

;; Integration Related Packages
(package! stumpwm-mode)
(package! engine-mode)
(package! youtube-dl
  :recipe
  (:host github
   :repo "skeeto/youtube-dl-emacs"))

(package! popper)


;;; Streaming Related
(package! websocket)
(package! obs-websocket
  :recipe
  (:host github :repo "sachac/obs-websocket-el"))

(package! prisma-mode
  :recipe
  (:host github
   :repo "pimeys/emacs-prisma-mode"
   :branch "main"))

(package! mastodon)
