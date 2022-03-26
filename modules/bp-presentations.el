;; -*- lexical-binding: t; -*-

(defun bp/org-present-prepare-slide ()
  (org-overview)
  (org-show-entry)
  (org-show-children))

(defun bp/org-present-hook ()
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.5) variable-pitch)
                                     (org-document-title (:height 1.75) org-document-title)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))
  (setq header-line-format " ")
  ;; (org-appear-mode -1)
  (centaur-tabs-mode -1)
  (hide-mode-line-mode 'toggle)
  (display-line-numbers-mode 'toggle)
  (org-display-inline-images)
  (bp/org-present-prepare-slide)
  (if (fboundp 'bp/kill-panel)
      (bp/kill-panel)))

(defun bp/org-present-quit-hook ()
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (setq header-line-format nil)
  (org-present-small)
  (org-remove-inline-images)
  ;; (org-appear-mode 1)
  (display-line-numbers-mode 'toggle)
  (centaur-tabs-mode 1)
  (hide-mode-line-mode 'toggle)
  (bp/org-font-setup)
  (superword-mode 1)
  (visual-fill-column-mode)
  (if (fboundp 'bp/start-panel)
      (bp/start-panel)))

(defun bp/org-present-prev ()
  (interactive)
  (org-present-prev)
  (bp/org-present-prepare-slide))

(defun bp/org-present-next ()
  (interactive)
  (org-present-next)
  (bp/org-present-prepare-slide))


(use-package! org-present
  :commands org-present-mode
  :config
  (substitute-key-definition 'org-present-next 'bp/org-present-next (current-global-map))
  (substitute-key-definition 'org-present-prev 'bp/org-present-prev (current-global-map))
  :hook
  (org-present-mode . bp/org-present-hook)
  (org-present-mode-quit . bp/org-present-quit-hook))

(provide 'bp-presentations)
