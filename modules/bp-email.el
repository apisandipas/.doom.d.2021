;; -*- lexical-binding: t; -*-
;;

;; (defvar bp/mu4e-account-alist
;;   '(("bparonto@gmail.com"
;;      (user-mail-address  "bparonto@gmail.com")
;;      (user-full-name     "Bryan Paronto")
;;      (mu4e-sent-folder   "~/Mail/bparonto@gmail.com/Sent Items")
;;      (mu4e-drafts-folder "~/Mail/bparonto@gmail.com/Drafts")
;;      (mu4e-trash-folder  "~/Mail/bparonto@gmail.com/Deleted Items")
;;      (mu4e-refile-folder "~/Mail/bparonto@gmail.com/Archive")
;;      (smtpmail-local-domain "gmail.com")
;;      (smtpmail-default-smtp-server "smtp.gmail.com")
;;      (smtpmail-smtp-server "smtp.gmail.com")
;;      (smtpmail-smtp-service 587)
;;      )

;;     ("bryan@cablecar.digital"
;;      (user-mail-address  "bryan@cablecar.digital")
;;      (user-full-name     "Bryan Paronto")
;;      (mu4e-sent-folder   "~/Mail/bryan@cablecar.digital/Sent Items")
;;      (mu4e-drafts-folder "~/Mail/bryan@cablecar.digital/Drafts")
;;      (mu4e-trash-folder  "~/Mail/bryan@cablecar.digital/Deleted Items")
;;      (mu4e-refile-folder "~/Mail/bryan@cablecar.digital/Archive")
;;      (smtpmail-local-domain "gmail.com")
;;      (smtpmail-default-smtp-server "smtp.gmail.com")
;;      (smtpmail-smtp-server "smtp.gmail.com")
;;      (smtpmail-smtp-service 587)
;;      )
;;     ))


; we installed this with homebrew
(setq mu4e-mu-binary (executable-find "mu"))
;; this command is called to sync imap servers:
(setq mu4e-get-mail-command (concat (executable-find "mbsync") " -a"))
;; how often to call it in seconds:
(setq mu4e-update-interval 300)
;; this is the directory we created before:
(setq mu4e-maildir "~/Main")
;; list of your email adresses:
(setq mu4e-user-mail-address-list '("bparonto@gmail.com"
                                    "bryan@cablecar.digital"))
(setq mu4e-maildir-shortcuts
        '(("/bparonto@gmail/Inbox" . ?g)
          ("/bryan@cablecar/Inbox" . ?c)))

(setq mu4e-contexts
      `(,(make-mu4e-context
          :name "bparonto@gmail"
          :enter-func
          (lambda () (mu4e-message "Enter bparonto@gmail context"))
          :leave-func
          (lambda () (mu4e-message "Leave bparonto@gmail context"))
          :match-func
          (lambda (msg)
            (when msg
              (mu4e-message-contact-field-matches msg
                                                  :to "bparonto@gmail.com")))
          :vars '((user-mail-address . "bparonto@gmail.com" )
                  (user-full-name . "Bryan Paronto")
                  (mu4e-drafts-folder . "/bparonto@gmail/Drafts")
                  (mu4e-refile-folder . "/bparonto@gmail/Archive")
                  (mu4e-sent-folder . "/bparonto@gmail/Sent Messages")
                  (mu4e-trash-folder . "/bryan@gmail/Deleted Messages")))

        ,(make-mu4e-context
          :name "bryan@cablecar"
          :enter-func
          (lambda () (mu4e-message "Enter bryan@cablecar.digital context"))
          :leave-func
          (lambda () (mu4e-message "Leave bryan@cablecar.digital context"))
          :match-func
          (lambda (msg)
            (when msg
              (mu4e-message-contact-field-matches msg
                                                  :to "bryan@cablecar.digital")))
          :vars '((user-mail-address . "bryan@cablecar.digital")
                  (user-full-name . "Dummy McDummerson")
                  (mu4e-drafts-folder . "/bryan@cablecar/Drafts")
                  (mu4e-refile-folder . "/bryan@cablecar/Archive")
                  (mu4e-sent-folder . "/bryan@cablecar/Sent Messages")
                  (mu4e-trash-folder . "/bryan@cablecar/Trash")))))

(setq mu4e-context-policy 'pick-first) ;; start with the first (default) context;
(setq mu4e-compose-context-policy 'ask) ;; ask for context if no context matches;

(defun bp/set-smtp-account ()
  (if (message-mail-p)
      (save-excursion
        (let*
            ((from (save-restriction
                     (message-narrow-to-headers)
                     (message-fetch-field "from")))
             (account
              (cond
               ((string-match "bparonto@gmail.com" from) "bparonto@gmail")
               ((string-match "bryan@cablecar.digital" from) "bryan@cablecar"))))
          (setq message-sendmail-extra-arguments (list '"-a" account))))))

(add-hook 'message-send-mail-hook 'bp/set-smtp-account)
;; mu4e cc & bcc
;; this is custom as well
(add-hook 'mu4e-compose-mode-hook
          (defun bp/add-cc-and-bcc ()
            "My Function to automatically add Cc & Bcc: headers.
    This is in the mu4e compose mode."
            (save-excursion (message-add-header "Cc:\n"))
            (save-excursion (message-add-header "Bcc:\n"))))

;; mu4e address completion
(add-hook 'mu4e-compose-mode-hook 'company-mode)

(provide 'bp-email)
