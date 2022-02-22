;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.
(package! s)
(package! org-protocol-capture-html)
(unpin! org-roam)
(package! org-roam-ui)
(package! doct)
(package! org-tree-slide)
(package! one-themes)
(package! elfeed-goodies)
(package! org-super-agenda)
(package! visual-fill-column)
(package! org-superstar)
(package! dashboard)
(package! ligature
  :recipe
  (:host github
   :repo "mickeynp/ligature.el"))

(package! exwm)
;; (package! perspective-exwm
;;   :recipe
;;   (:host github
;;    :repo "emacsmirror/perspective-exwm"))

(package! desktop-environment)
(package! exwm-outer-gaps
  :recipe
  (:host github
   :repo "lucasgruss/exwm-outer-gaps"))

(package! doom-modeline-exwm
  :recipe
  (:host github
   :repo "elken/doom-modeline-exwm"))

(package! all-the-icons-ivy)
(package! ivy-posframe)

(package! mu4e-dashboard
  :recipe
  (:host github
   :repo "rougier/mu4e-dashboard"))

(package! auto-dim-other-buffers)
;; (when (package! eaf :recipe (:host github
;;                              :repo "manateelazycat/emacs-application-framework"
;;                              :files ("*.el" "*.py" "app" "core")
;;                              :build (:not compile)))

;;   (package! ctable :recipe (:host github :repo "kiwanami/emacs-ctable"))
;;   (package! deferred :recipe (:host github :repo "kiwanami/emacs-deferred"))
;;   (package! epc :recipe (:host github :repo "kiwanami/emacs-epc")))

;; (package! eaf-mindmap :recipe (:host github :repo "emacs-eaf/eaf-mindmap"))
;; (package! eaf-browser :recipe (:host github :repo "emacs-eaf/eaf-browser"))
