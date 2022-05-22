;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.
(package! s)
(package! one-themes)
(package! winum)
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

(package! leuven-theme)

(package! bespoke-modeline
  :recipe (:type git :host github :repo "mclear-tools/bespoke-modeline"))
(package! bespoke-themes
  :recipe (:host github :repo "mclear-tools/bespoke-themes" :branch "main"))

(package! nano-theme)
(package! mood-line)
(package! nano-modeline)

(package! vertico-posframe)

;;; Additons Elfeed Packages
(package! elfeed-dashboard)
(package! elfeed-goodies)

(package! visual-fill-column)
(package! dashboard)
(package! youtube-dl
  :recipe
  (:host github
   :repo "skeeto/youtube-dl-emacs"))

(package! ligature
  :recipe
  (:host github
   :repo "mickeynp/ligature.el"))

(package! exwm)
(package! edwina)
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

(package! mu4e-thread-folding
  :recipe
  (:host github
   :repo "rougier/mu4e-thread-folding"))
(package! mu4e-dashboard
  :recipe
  (:host github
   :repo "rougier/mu4e-dashboard"))

(package! dimmer)
(package! stumpwm-mode)

;;; Streaming setup
(package! websocket)
(package! obs-websocket
  :recipe
  (:host github :repo "sachac/obs-websocket-el"))


;;; Nyxt hacker browser goodnes
(package! engine-mode)
