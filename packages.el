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

;; unused
(package! doct)

(package! vertico-posframe)

(package! elfeed-goodies)

(package! visual-fill-column)
(package! dashboard)
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

(package! mu4e-dashboard
  :recipe
  (:host github
   :repo "rougier/mu4e-dashboard"))

(package! auto-dim-other-buffers)
(package! stumpwm-mode)

;;; Streaming setup
(package! websocket)
(package! obs-websocket
  :recipe
  (:host github :repo "sachac/obs-websocket-el"))
