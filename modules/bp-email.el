;; -*- lexical-binding: t; -*-

(require 'mu4e)

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'sent)
(setq mu4e-mu-binary (executable-find "mu"))

;; this command is called to sync imap servers:
(setq mu4e-get-mail-command (concat (executable-find "mbsync") " -a"))

;; how often to call it in seconds:
(setq mu4e-update-interval 300)

;; this is the directory we created before:
(setq mu4e-root-maildir "~/Mail")

(setq mu4e-alert-interesting-mail-query
      (concat
       "flag:unread"
       " AND NOT flag:trashed"
       " AND NOT maildir:"
       "\"/bparonto@gmail/[Gmail].All Mail\""
       " AND NOT maildir:"
       "\"/bryan@cablecar/[Gmail].All Mail\""
       " AND NOT maildir:"
       "\"/bparonto@gmail/[Gmail].Trash\""
       " AND NOT maildir:"
       "\"/bryan@cablecar/[Gmail].Trash\""
       ))

;; attempt to show images when viewing messages
(setq mu4e-view-show-images t
      mu4e-show-images t
      mu4e-view-image-max-width 800
      mu4e-compose-dont-reply-to-self t
      mu4e-change-filenames-when-moving t)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; attachments go here
(setq mu4e-attachment-dir  "~/Downloads")

                                        ; start with the first (default) context;
(setq mu4e-context-policy 'ask)

;; ask for context if no context matches;
(setq mu4e-compose-context-policy 'ask)

(defun bp/render-html-message ()
  (let ((dom (libxml-parse-html-region (point-min) (point-max))))
    (erase-buffer)
    (shr-insert-document dom)
    (goto-char (point-min))))
(setq mu4e-html2text-command 'bp/render-html-message)

;; list of your email adresses:
(setq mu4e-personal-addresses '("bparonto@gmail.com"
                                "bryan@cablecar.digital"))
;; (setq mu4e-maildir-shortcuts
;;       '(("/bparonto@gmail/INBOX" . ?g)
;;         ("/bryan@cablecar/INBOX" . ?c)))

(setq mu4e-contexts
      `(,(make-mu4e-context
          :name "gmail"
          :enter-func (lambda () (mu4e-message "Entering gmail context"))
          :leave-func (lambda () (mu4e-message "Leaving gmail context"))
          :match-func (lambda (msg)
                        (when msg
                          (string= (mu4e-message-field msg :maildir) "/bparonto@gmail")))
          :vars '((user-mail-address . "bparonto@gmail.com")
                  (user-full-name . "Bryan Paronto")
                  (mu4e-drafts-folder . "/bparonto@gmail/[Gmail].Drafts")
                  (mu4e-refile-folder . "/bparonto@gmail/[Gmail].All Mail")
                  (mu4e-sent-folder . "/bparonto@gmail/[Gmail].Sent Mail")
                  (mu4e-trash-folder . "/bparonto@gmail/[Gmail].Trash")
                  ;; SMTP configuration
                  (starttls-use-gnutls . t)
                  (smtpmail-starttls-credentials . '(("smtp.gmail.com" 587 nil nil)))
                  (smtpmail-smtp-user . "bparonto@gmail.com")
                  (smtpmail-auth-credentials .
                                             '(("smtp.gmail.com" 587 "bparonto@gmail.com" nil)))
                  (smtpmail-default-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-service . 587)))
        ,(make-mu4e-context
          :name "cablecar"
          :enter-func (lambda () (mu4e-message "Entering cablecar context"))
          :leave-func (lambda () (mu4e-message "Leaving cablecar context"))

          :match-func (lambda (msg)
                        (when msg
                          (string= (mu4e-message-field msg :maildir) "/bryan@cablecar")))
          :vars '((user-mail-address . "bryan@cablecar.digital")
                  (user-full-name . "Bryan Paronto")
                  (mu4e-drafts-folder . "/bryan@cablecar/[Gmail].Drafts")
                  (mu4e-refile-folder . "/bryan@cablecar/[Gmail].All Mail")
                  (mu4e-sent-folder . "/bryan@cablecar/[Gmail].Sent Mail")
                  (mu4e-trash-folder . "/bryan@cablecar/[Gmail].Trash")
                  ;; SMTP configuration
                  (starttls-use-gnutls . t)
                  (smtpmail-starttls-credentials . '(("smtp.gmail.com" 587 nil nil)))
                  (smtpmail-smtp-user . "bryan@cablecar.digital")
                  (smtpmail-auth-credentials .
                                             '(("smtp.gmail.com" 587 "bryan@cablecar.digital" nil)))
                  (smtpmail-default-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-service . 587)))
        ))

(setq mu4e-maildir-shortcuts  '((:maildir "/bryan@cablecar/INBOX"               :key ?i)
                                (:maildir "/bryan@cablecar/[Gmail].Sent Mail"   :key ?s)
                                (:maildir "/bryan@cablecar/[Gmail].Drafts"      :key ?d)
                                (:maildir "/bryan@cablecar/[Gmail].Trash"       :key ?t)
                                (:maildir "/bryan@cablecar/[Gmail].All Mail"    :key ?a)
                                (:maildir "/bparonto@gmail/INBOX"               :key ?I)
                                (:maildir "/bparonto@gmail/[Gmail].Sent Mail"   :key ?S)
                                (:maildir "/bparonto@gmail/[Gmail].Drafts"      :key ?D)
                                (:maildir "/bparonto@gmail/[Gmail].Trash"       :key ?T)
                                (:maildir "/bparonto@gmail/[Gmail].All Mail"    :key ?A)))

(add-hook 'mu4e-compose-mode-hook
          (defun bp/add-cc-and-bcc ()
            "Automatically add Cc & Bcc: headers in the mu4e compose mode."
            (save-excursion (message-add-header "Cc:\n"))
            (save-excursion (message-add-header "Bcc:\n"))))

;; mu4e address completion
(add-hook 'mu4e-compose-mode-hook 'company-mode)
(require 'mu4e-thread-folding)

  (add-to-list 'mu4e-header-info-custom
               '(:empty . (:name "Empty"
                           :shortname ""
                           :function (lambda (msg) "  "))))
  (setq mu4e-headers-fields '((:empty         .    2)
                              (:human-date    .   12)
                              (:flags         .    6)
                              (:mailing-list  .   10)
                              (:from          .   22)
                              (:subject       .   nil)))

(define-key mu4e-headers-mode-map (kbd "<tab>")     'mu4e-headers-toggle-at-point)
(define-key mu4e-headers-mode-map (kbd "<left>")    'mu4e-headers-fold-at-point)
(define-key mu4e-headers-mode-map (kbd "<S-left>")  'mu4e-headers-fold-all)
(define-key mu4e-headers-mode-map (kbd "<right>")   'mu4e-headers-unfold-at-point)
(define-key mu4e-headers-mode-map (kbd "<S-right>") 'mu4e-headers-unfold-all)
(mu4e-thread-folding-mode 1)


(provide 'bp-email)
