
(defvar bp/mu4e-account-alist
  '(
    ("bparonto@gmail.com"
     (user-mail-address  "bparonto@gmail.com")
     (user-full-name     "Bryan Paronto")
     (mu4e-sent-folder   "~/Mail/bparonto@gmail.com/Sent Items")
     (mu4e-drafts-folder "~/Mail/bparonto@gmail.com/Drafts")
     (mu4e-trash-folder  "~/Mail/bparonto@gmail.com/Deleted Items")
     (mu4e-refile-folder "~/Mail/bparonto@gmail.com/Archive"))

    ;; TODO Edit Gsuite admin config to allow
    ("bryan@cablecar.digital"
     (user-mail-address  "bryan@cablecar.digital")
     (user-full-name     "Bryan Paronto")
     (mu4e-sent-folder   "~/Mail/bryan@cablecar.digital/Sent Items")
     (mu4e-drafts-folder "~/Mail/bryan@cablecar.digital/Drafts")
     (mu4e-trash-folder  "~/Mail/bryan@cablecar.digital/Deleted Items")
     (mu4e-refile-folder "~/Mail/bryan@cablecar.digital/Archive"))
    ))

(setq mu4e-user-mail-address-list
      (mapcar (lambda (account) (cadr (assq 'user-mail-address account)))
              bp/mu4e-account-alist))
