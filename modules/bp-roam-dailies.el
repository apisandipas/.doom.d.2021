
(defun yesterday ()
  (time-subtract (current-time) (seconds-to-time 86400)))

(defun tomorrow ()
  (time-add (current-time) (seconds-to-time 86400)))

(defun yesterday-filename ()
  "Returns yesterday's date in the format '%Y-%m-%d %a'."
  (let* ((yesterday (time-subtract (current-time) (seconds-to-time 86400)))
         (year (format-time-string "%Y" yesterday))
         (month (format-time-string "%m" yesterday))
         (day (format-time-string "%d" yesterday)))
    (concat year "-" month "-" day)))

(defun yesterday-title ()
  "Returns yesterday's date in the format '%Y-%m-%d %a'."
  (let* ((yesterday (time-subtract (current-time) (seconds-to-time 86400)))
         (year (format-time-string "%Y" yesterday))
         (month (format-time-string "%m" yesterday))
         (day (format-time-string "%d" yesterday))
         (weekday (format-time-string "%a" yesterday)))
    (concat year "-" month "-" day " " weekday)))


(defun tomorrow-filename ()
  "Returns yesterday's date in the format '%Y-%m-%d %a'."
  (let* ((tomorrow (time-add (current-time) (seconds-to-time 86400)))
         (year (format-time-string "%Y" tomorrow))
         (month (format-time-string "%m" tomorrow))
         (day (format-time-string "%d" tomorrow)))
    (concat year "-" month "-" day)))

(defun tomorrow-title ()
  "Returns tomorrow's date in the format '%Y-%m-%d %a'."
  (let* ((tomorrow (time-add (current-time) (seconds-to-time 86400)))
         (year (format-time-string "%Y" tomorrow))
         (month (format-time-string "%m" tomorrow))
         (day (format-time-string "%d" tomorrow))
         (weekday (format-time-string "%a" tomorrow)))
    (concat year "-" month "-" day " " weekday)))


(after! org-roam

  (defhydra bp/org-roam-jump-menu (:exit t)
    "
  ^Jump To Day^    ^Periodic^
  ^^^^^^^^-------------------------------------------------
  _t_: today       _m_: current month
  _r_: tomorrow    _e_: current year
  _y_: yesterday  ^ ^
  _d_: date       ^ ^
  "
    ("t" bp/todays-note)
    ("r" bp/tomorrows-note)
    ("y" bp/yesterdays-note)
    ("d" org-roam-dailies-goto-date)
    ("m" bp/monthly-note)
    ("e" bp/yearly-note)
    ("q" nil "cancel"))

  (map! :leader
        :prefix ("n" "notes")
        :desc "Journal Manager"
        "j" #'bp/org-roam-jump-menu/body)

  (defun org-roam-daily-note (template-name filename title parent &optional open date)
    "Create a daily note using a template file."
    (interactive)
    (let* ((template-file (concat org-roam-directory "/bins/templates/" template-name ".tmp.org"))
           ;; (title (format-time-string "%Y-%m-%d"))
           (file (concat org-roam-directory "/temporal/" template-name "/" filename ".org"))
           (link-to-parent (parent-link parent date))
           (org-id-overriding-file-name file)
           id)
      (unless (file-exists-p file)
        (with-temp-buffer
          (insert
           (concat ":PROPERTIES:\n:ID:        \n:END:\n"
                   "#+title: " title " \n\n"
                   "Parent :: " link-to-parent "\n\n"
                   ))
          (goto-char 25)
          (setq id (org-id-get-create))
          (goto-line 8)
          (insert-file-contents template-file)
          (write-file file)
          (org-roam-db-update-file file)
          (format "[[id:%s][%s]]" id title)))
      (if open (find-file file))

      ))


  (defun bp/todays-note ()
    "Create and open todays note"
    (interactive)
    (org-roam-daily-note  "daily"
                          (format-time-string "%Y-%m-%d")
                          (format-time-string "%Y-%m-%d %a")
                          "%B %Y"
                          :t
                          (current-time)))

  (defun bp/yesterdays-note ()
    "Create and open yesterdays note"
    (interactive)
    (org-roam-daily-note  "daily"
                          (yesterday-filename)
                          (yesterday-title)
                          "%B %Y"
                          :t
                          (yesterday)))

  (defun bp/tomorrows-note ()
    "Create and open tomorrow's note"
    (interactive)
    (org-roam-daily-note  "daily"
                          (tomorrow-filename)
                          (tomorrow-title)
                          "%B %Y"
                          :t
                          (tomorrow)))


  (defun bp/monthly-note ()
    "Create and open this month's note"
    (interactive)
    (org-roam-daily-note  "monthly"
                          (format-time-string "%Y-%B")
                          (format-time-string "%B %Y")
                          "%Y"
                          :t
                          (current-time)))

  (defun bp/yearly-note ()
    "Create and open this year's note"
    (interactive)
    (org-roam-daily-note  "yearly"
                          (format-time-string "%Y")
                          (format-time-string "%Y")
                          ""
                          :t
                          (current-time)))


  (defun bp/get-roam-link (node)
    "Take an org roam NODE and returns a string
      that contains the link to that node."
    (org-link-make-string
     (concat
      "id:" (org-roam-node-id node))
     (org-roam-node-title node)))

  (defun bp/get-roam-node-by-format-and-date (format &optional date)
    "Retrieves an org-roam node by the format string and date associated with the node"
    (org-roam-node-from-title-or-alias (format-time-string format date)))

  (defun parent-link (format &optional date)
    "Insert the link to the parent node"
    (let* ((node (bp/get-roam-node-by-format-and-date format date)))
      (if node
          (bp/get-roam-link node)
        "No Parent")))


  )

(parent-link "%B %Y")


(provide 'bp-roam-dailies)
