;;; bp-roam-dailies --- Customizations for org-roam-dailies. -*- lexical-binding: t; -*-
;;; Commentary:
;;; This shit is a mess. I may be running on caffiene and misconceptions of how it's supposed to work.
;;; THIS IS ALL DEPRECATED. SOON TO BE DELETED
;;; Code:



(after! org-roam

  (defhydra bp/org-roam-jump-menu (:exit t)
    "
  ^Jump To Day^    ^Capture^       ^Periodic^
  ^^^^^^^^-------------------------------------------------
  _t_: today       _T_: today       _m_: current month
  _r_: tomorrow    _R_: tomorrow    _e_: current year
  _y_: yesterday   _Y_: yesterday   ^ ^
  _d_: date        ^ ^              ^ ^
  "
    ("t" bp/org-roam-goto-today)
    ("r" org-roam-dailies-goto-tomorrow)
    ("y" org-roam-dailies-goto-yesterday)
    ("d" org-roam-dailies-goto-date)
    ("T" org-roam-dailies-capture-today)
    ("R" org-roam-dailies-capture-tomorrow)
    ("Y" org-roam-dailies-capture-yesterday)
    ("m" bp/org-roam-goto-month)
    ("e" bp/org-roam-goto-year)
    ("q" nil "cancel"))

  (map! :leader
        :prefix ("n" "notes")
        :desc "Journal Manager"
        "j" #'bp/org-roam-jump-menu/body)


  (defun bp/org-roam-goto-today ()
    "Go to the current days temporal note"
    (interactive)
    (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias (format-time-string "%Y-%m-%d %a")) '(4))
                       :node (org-roam-node-create)
                       :templates `(("d" "daily" plain (file "~/Dropbox/org/brain/bins/templates/daily.tmp.org")
                                     :if-new (file+head "temporal/daily/%<%Y-%m-%d>.org"
                                                        ,(bp/org-roam/daily-note-header))
                                     :unnarrowed t))))

  (defun bp/org-roam/daily-note-header ()
    "Generate the monthly note link for daily note header"
    (interactive)
    (concat "#+title: %<%Y-%m-%d %a>\n\n"
            "Parent :: "
            (bp/org-roam/get-monthly-note-link)
            "\n\n"))

;;;  Untested functions below!!
  (defun bp/org-roam-goto-month ()
    "Go to the current months temporal note"
    (interactive)
    (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias (format-time-string "%Y-%B")) '(4))
                       :node (org-roam-node-create)
                       :templates '(("m" "month" plain "\n\n* Goals\n\n%?* Summary\n\n"
                                     :if-new (file+head "temporal/monthly/%<%Y-%B>.org"
                                                        "#+title: %<%Y-%B>\n#+filetags: temporal/monthly\n\n")
                                     :unnarrowed t))))

  (defun bp/org-roam-goto-year ()
    "Go to the current years temporal note"
    (interactive)
    (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias (format-time-string "%Y")) '(4))
                       :node (org-roam-node-create)
                       :templates '(("y" "year" plain "\n\n* Goals\n\n%?* Summary\n\n"
                                     :if-new (file+head "temporal/yearly/%<%Y>.org"
                                                        "#+title: %<%Y>\n#+filetags: temporal/yearly\n\n")
                                     :unnarrowed t))))


  (defun bp/get-roam-node-link-string (node)
    "Take an org roam NODE and returns a string
      that contains the link to that node."
    (org-link-make-string
     (concat
      "id:" (org-roam-node-id node))
     (org-roam-node-title node)))

  (defun bp/org-roam/get-node-by-format-and-date (format &optional date)
    "Retrieves an org-roam node by the format string and date associated with the node"
    (org-roam-node-from-title-or-alias (format-time-string format date)))

  (defun bp/org-roam/get-monthly-note-link ()
    "Get the link for the current months temporal note,
     otherwise create it and return its link"
    (interactive)
    (let* ((node (bp/org-roam/get-node-by-format-and-date "%Y-%B")))
      (if node
          (bp/get-roam-node-link-string node)
        (progn
          (org-roam-capture- :goto nil
                             :node (org-roam-node-create)
                             :templates `(("m" "month" entry "\n\n* Goals\n\n%?* Summary\n\n"
                                           :if-new (file+head "temporal/monthly/%<%Y-%B>.org"
                                                              ,(bp/org-roam/monthly-note-header))
                                           :immediate-finish t
                                           :unnarrowed t)))
          (org-roam-db-sync)
          (let ((node (bp/org-roam/get-node-by-format-and-date "%Y-%B")))
            (bp/get-roam-node-link-string node))))))

  ;;

  (defun bp/org-roam/monthly-note-header ()
    "Generate the monthly note header."
    (concat "#+title: %<%Y-%B>\n"
            "#+filetags: temporal/monthly\n\n"
            "Parent :: "
            (bp/org-roam/get-yearly-note-link)
            "\n\n"))

  (defun bp/org-roam/yearly-note-header ()
    "Generate the yearly note header."
    (concat "#+title: %<%Y>\n"
            "#+filetags: temporal/yearly\n\n"
            " Parent :: [[id:ee48e0b8-14c0-4c57-a24c-e8248fdca288][Temporal MOC]]"
            "\n\n"))

  (defun bp/org-roam/get-yearly-note-link ()
    "Get the link for the current years temporal note,
     otherwise create it and return its link"
    (interactive)
    (let* ((node (bp/org-roam/get-node-by-format-and-date "%Y")))
      (if node
          (bp/get-roam-node-link-string node)
        (progn
          (org-roam-capture- :goto nil
                             :node (org-roam-node-create)
                             :templates `(("y" "year" entry "\n\n* Goals\n\n%?* Summary\n\n"
                                           :if-new (file+head "temporal/yearly/%<%Y>.org"
                                                              ,(bp/org-roam/yearly-note-header))
                                           :immediate-finish t
                                           :unnarrowed t)))
          (org-roam-db-sync)
          (let ((node (bp/org-roam/get-node-by-format-and-date "%Y")))
            (bp/get-roam-node-link-string node))))))



  )

(provide 'bp-roam-dailies)
;;; bp-roam-dailies.el ends here.
