;;; init-mastodon.el --- Mastodon layout mockup -*- lexical-binding: t -*-

(use-package! mastodon
  :config
  (setq mastodon-instance-url "https://emacs.ch"
          mastodon-active-user "cablecardigital"))

;; Copyright (C) 2022 Nicolas P. Rougier

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Work in progress (= may break at any point...)
;;
;; Known bugs:
;;  - When name + handle combination is too long, date will continue
;;    on next line
;;  - When the toot is boosted or favorited, this mess up with prev/next.
;;    Problem dissapear if the while timeline is reloaded.
;;    -> mastodon-toot--action needs to be modified such as to add the
;;       'toot-base-id' (that is used to identify toots region)

;;; Code:
(require 'mastodon)
(require 'mastodon-tl)

;; Personal settings (OSX)
(setq mastodon-tl--display-media-p nil)
(set-fontset-font t 'emoji '("Apple Color Emoji" . "iso10646-1") nil 'prepend)
(set-fontset-font t 'symbol (font-spec :family "Symbola") nil 'prepend)
(setq mastodon-tl--enable-relative-timestamps t)
(custom-set-faces
 '(mastodon-boosted-face      ((t (:inherit (nano-faded)))))
 '(mastodon-display-name-face ((t (:inherit (nano-strong))))))
;; End personal settings


(defface mastodon-tl-separator-face
  `((t :inherit font-lock-comment-face
       :strike-through t
       :height 1.0
       :extend t))
  "Face for line separating toots."
  :group 'mastodon-tl)

(defface mastodon-tl-update-face
  `((t :inherit (error bold)
       :height 1.0
       :extend t))
  "Face for line separating new from old toots."
  :group 'mastodon-tl)

(defface mastodon-tl-status-face
  `((t :inherit font-lock-comment-face))
  "Face for toot status (below separator)."
  :group 'mastodon-tl)

(defface mastodon-tl-actions-face
  `((t :inherit font-lock-comment-face))
  "Face for toot actions (bottom left)."
  :group 'mastodon-tl)

(defface mastodon-tl-timestamp-face
  `((t :inherit default))
  "Face for toot timestamp."
  :group 'mastodon-tl)

(defface mastodon-tl-box-face
  `((t :inherit font-lock-comment-face
       :extend t))
  "Face for boxes outline."
  :group 'mastodon-tl)

(defcustom mastodon-tl-toot-actions t
  "Whether to show toot actions menu (on the bottom left)"
  :type 'bool
  :group 'mastodon-tl)

(defcustom mastodon-tl-toot-separator t
  "Whether to show separation between toots."
  :type 'bool
  :group 'mastodon-tl)

(defcustom mastodon-tl-toot-timestamp t
  "Whether to show toot timestamp."
  :type 'bool
  :group 'mastodon-tl)

(defcustom mastodon-tl-toot-status t
  "Whether to show toot status (on the bottom right)."
  :type 'bool
  :group 'mastodon-tl)

(defcustom mastodon-tl-shorten-url t
  "Whether to shorten url in toots."
  :type 'bool
  :group 'mastodon-tl)

(defcustom mastodon-tl-box-boosted t
  "Whether to enclose boosted toots in a text box."
  :type 'bool
  :group 'mastodon-tl)

(defcustom mastodon-tl-box-width -3
  "Width of the box for boosted toots (a negative value v means (-
window width v)"
  :type 'number
  :group 'mastodon-tl)

(defcustom mastodon-tl-box-prefix " "
  "Prefix to add in front of a box for boosted toots."
  :type 'string
  :group 'mastodon-tl)

;; Symbols using nerd-fonts, if you get tofu, just replace them
(defcustom mastodon-tl-symbols '((reply     . "")
                                 (boost     . "")
                                 (favourite . "")
                                 (bookmark  . "")
                                 (media     . "")
                                 (verified  . ""))
  "Set of symbols (or strings) to be used for displaying toot status"
  :type '(alist :key-type symbol :value-type string)
  :group 'mastodon-tl)


(defun mastodon--shorten-url (url)
  "Shorten a url to its domain. For example, https//github.com/rougier
would become [github.com] and the echo help will display the original
url and original keymap is untouched."

  (with-temp-buffer
    (insert url)
    (goto-char (point-min))
    (while (search-forward-regexp "\\(\\w+://\\([^/]+\\)[^ \n]*\\)" nil t)
      (replace-match
       (propertize (format "[%s]" (match-string 2))
                   'face 'shr-link
                   'shr-url (match-string 1)
                   'mastodon-tab-stop 'shr-url
                   'keymap mastodon-tl--shr-map-replacement
                   'follow-link t
                   'mouse-face 'highlight
                   'help-echo (format "URL: %s" (match-string 1)))))
    (buffer-substring (point-min) (point-max))))


(defun mastodon-tl--mark-update ()
  "Add a mark update at the top of a feed, this should be ran just
before an update."

  (remove-overlays (point-min) (point-max) 'mastodon-update t)
  (let* ((overlay (make-overlay (point-min) (+ (point-min) 1) nil t))
        (width (- (window-width) 1))
        (update (format-time-string " Update %H:%M"))
        (line (concat
               (propertize (make-string (- width (length update) 1) ? )
                           'face '(:inherit mastodon-tl-update-face :strike-through t))
               (propertize update
                           'face '(:inherit mastodon-tl-update-face :strike-through nil))
               (propertize "\n"
                           'face '(:inherit mastodon-tl-update-face :strike-through nil))
               )))
    (overlay-put overlay 'mastodon-update t)
    (overlay-put overlay 'display line)))



(defun mastodon-tl--toot-box (text &optional size prefix)
  "Enclose TEXT with a unicode box with given SIZE and prefix the
BOX using PREFIX."

  (let* ((prefix (or prefix mastodon-tl-box-prefix))
         (size (or size mastodon-tl-box-width))
         (size (cond ((and size (> size 0)) size)
                     ((and size (< size 0)) (+ (window-width) size))
                     (t (- (window-size) 2))))
         (text (with-temp-buffer
                 (insert text)
                 (goto-char (point-min))
                  ;; 6 instead of 4 to have room for the "filler"
                 (let ((fill-column (- size 6))
                       (sentence-end-double-space nil))
                   (fill-region (point-min) (point-max)))
                 (buffer-substring (point-min) (point-max))))
         (line-format (format "%s│ %%s %s│\n" prefix
            (propertize " "  'display `(space :align-to ,(- size (length prefix) -1))))))
    (concat prefix "┌" (make-string (- size 2) ?─) "┐\n"
            (mapconcat (lambda (line)
                         (format line-format line))
                       (split-string text "[\n]+") "")
            prefix "└" (make-string (- size 2) ?─) "┘")))

(defun mastodon-tl--update ()
  "Update timeline with new toots."

  (interactive)
  (mastodon-tl--mark-update)
  (save-excursion
    (message "Loading newer toots...")
    (let* ((endpoint (mastodon-tl--get-endpoint))
           (update-function (mastodon-tl--get-update-function))
           (id (mastodon-tl--newest-id))
           (json (mastodon-tl--updated-json endpoint id)))
      (when json
        (let ((inhibit-read-only t))
          (goto-char (or mastodon-tl--update-point (point-min)))
          (funcall update-function json))))))

(defun mastodon-tl--more ()
  "Append older toots to timeline, asynchronously."

  (interactive)
  (mastodon-tl--mark-update)
  (save-excursion
  (message "Loading older toots...")
  (if (string= (buffer-name (current-buffer)) "*mastodon-favourites*")
      ;; link-header: can't build a URL with --more-json-async, endpoint/id:
      (let* ((next (car (mastodon-tl--link-header)))
             (prev (cadr (mastodon-tl--link-header)))
             (url (mastodon-tl--build-link-header-url next)))
        (mastodon-http--get-response-async url 'mastodon-tl--more* (current-buffer)
                                           (point) :headers))
    (mastodon-tl--more-json-async (mastodon-tl--get-endpoint) (mastodon-tl--oldest-id)
                                  'mastodon-tl--more* (current-buffer) (point)))))



(defun mastodon-tl--insert-status (toot body author-byline action-byline
                                        &optional id parent-toot detailed-p)
  "Display the content and byline of timeline element TOOT.

BODY will form the section of the toot above the byline.
AUTHOR-BYLINE is an optional function for adding the author
portion of the byline that takes one variable. By default it is
`mastodon-tl--byline-author'
ACTION-BYLINE is also an optional function for adding an action,
such as boosting favouriting and following to the byline. It also
takes a single function. By default it is
`mastodon-tl--byline-boosted'.

ID is that of the toot, which is attached as a property if it is
a notification. If the status is a favourite or a boost,
PARENT-TOOT is the JSON of the toot responded to.

DETAILED-P means display more detailed info. For now
this just means displaying toot client."
  (let ((start-pos (point))
        (reblog (alist-get 'reblog toot))
        (actions (if mastodon-tl-toot-actions
                     "follow | reply | mute | block"
                   ""))
        (status  (if mastodon-tl-toot-status
                     (string-trim (or (mastodon-tl--format-faves-count toot) ""))
                   ""))
        (body (if mastodon-tl-shorten-url
                  (mastodon--shorten-url body)
                body)))
    (insert
     (propertize
      (concat
       (propertize "\n" 'face 'mastodon-tl-separator-face
                        'toot-start t)
       (mastodon-tl--byline toot author-byline action-byline detailed-p)
       "\n"
       (if (and reblog mastodon-tl-box-boosted)
           (mastodon-tl--toot-box body)
         (string-fill body (- (window-width) 2)))
       (unless (string= (substring body -1) "\n")
         "\n")
       "\n"
       (concat (propertize actions 'face 'mastodon-tl-actions-face)
               (propertize " " 'display `(space :align-to (- right ,(+ (length status) 2))))
               (propertize status 'face 'mastodon-tl-status-face)
               ""))
      'toot-id      (or id ; for notifications
                        (alist-get 'id toot))
      'base-toot-id (mastodon-tl--toot-id
                     ;; if a favourite/boost notif, get ID of toot responded to:
                     (or parent-toot toot))
      'toot-json    toot
      'parent-toot parent-toot)
     "\n")
    (when mastodon-tl--display-media-p
      (mastodon-media--inline-images start-pos (point)))))


;; HACK: Make use of symbols
(defun mastodon-tl--format-faves-count (toot)
  "Format a favourites, boosts, replies count for a TOOT.
Used as a help-echo when point is at the start of a byline, i.e.
where `mastodon-tl--goto-next-toot' leaves point. Also displays a
toot's media types and optionally the binding to play moving
image media from the byline."
  (let* ((toot-to-count
          (or
           ;; simply praying this order works
           (alist-get 'status toot) ; notifications timeline
           ;; fol-req notif, has 'type
           ;; placed before boosts coz fol-reqs have a (useless) reblog entry:
           ;; TODO: cd also test for notifs buffer before we do this to be sure
           (when (alist-get 'type toot)
             toot)
           (alist-get 'reblog toot) ; boosts
           toot)) ; everything else
         (fol-req-p (or (string= (alist-get 'type toot-to-count) "follow")
                        (string= (alist-get 'type toot-to-count) "follow_request"))))
    (unless fol-req-p
      (let* ((media-types (mastodon-tl--get-media-types toot))
             (format-faves (format "%s %s | %s %s | %s %s"
                                   (alist-get 'favourites_count toot-to-count)
                                   (alist-get 'favourite mastodon-tl-symbols)
                                   (alist-get 'reblogs_count toot-to-count)
                                   (alist-get 'boost mastodon-tl-symbols)
                                   (alist-get 'replies_count toot-to-count)
                                   (alist-get 'reply mastodon-tl-symbols)))
             (format-media (when media-types
                             (format " | %s"
                                     (alist-get 'media mastodon-tl-symbols)
                                     (mapconcat #'identity media-types " "))))
             (format-media-binding (when (and (or
                                               (member "video" media-types)
                                               (member "gifv" media-types))
                                              (require 'mpv nil :no-error))
                                     (format " | C-RET to view with mpv"))))
        (format "%s" (concat format-faves format-media format-media-binding))))))


;; HACK: Complete change of layout
(defun mastodon-tl--byline (toot author-byline action-byline &optional detailed-p)
  "Generate byline for TOOT.

AUTHOR-BYLINE is a function for adding the author portion of
the byline that takes one variable.
ACTION-BYLINE is a function for adding an action, such as boosting,
favouriting and following to the byline. It also takes a single function.
By default it is `mastodon-tl--byline-boosted'.

DETAILED-P means display more detailed info. For now
this just means displaying toot client."
  (let* ((created-time
          ;; bosts and faves in notifs view
          ;; (makes timestamps be for the original toot
          ;; not the boost/fave):
          (or (mastodon-tl--field 'created_at
                                  (mastodon-tl--field 'status toot))
              ;; all other toots, inc. boosts/faves in timelines:
              ;; (mastodon-tl--field auto fetches from reblogs if needed):
              (mastodon-tl--field 'created_at toot)))
         (parsed-time (date-to-time created-time))
         (faved (equal 't (mastodon-tl--field 'favourited toot)))
         (boosted (equal 't (mastodon-tl--field 'reblogged toot)))
         (bookmarked (equal 't (mastodon-tl--field 'bookmarked toot)))
         (bookmark-str (if (fontp (char-displayable-p #10r128278))
                           "🔖"
                         "K"))
         (visibility (mastodon-tl--field 'visibility toot)))
    (concat
     ;; Boosted/favourited markers are not technically part of the byline, so
     ;; we don't propertize them with 'byline t', as per the rest. This
     ;; ensures that `mastodon-tl--goto-next-toot' puts point on
     ;; author-byline, not before the (F) or (B) marker. Not propertizing like
     ;; this makes the behaviour of these markers consistent whether they are
     ;; displayed for an already boosted/favourited toot or as the result of
     ;; the toot having just been favourited/boosted.

     ;; !! WARNING: Here I (N.Rougier) propertize boosted/favourited
     ;; markes to be consistent with the new next/prev methods that are
     ;; based on toot-id changes.
     (propertize
      (concat (when boosted
                (mastodon-tl--format-faved-or-boosted-byline "B"))
              (when faved
                (mastodon-tl--format-faved-or-boosted-byline "F"))
              (when bookmarked
                (mastodon-tl--format-faved-or-boosted-byline bookmark-str)))
      'toot-id (alist-get 'id toot))
     (propertize
      (concat
       ;; we propertize help-echo format faves for author name
       ;; in `mastodon-tl--byline-author'
       (funcall author-byline toot)
       (cond ((equal visibility "direct")
              (if (fontp (char-displayable-p #10r9993))
                  " ✉"
                " [direct]"))
             ((equal visibility "private")
              (if (fontp (char-displayable-p #10r128274))
                  " 🔒"
                " [followers]")))
       (funcall action-byline toot)
       " "
       ;; TODO: Once we have a view for toot (responses etc.) make
       ;; this a tab stop and attach an action.

       (let* ((absolute (format-time-string mastodon-toot-timestamp-format parsed-time))
              (relative (mastodon-tl--relative-time-description parsed-time)))
         (if mastodon-tl--enable-relative-timestamps
             (concat (propertize " " 'display `(space :align-to (- right 12 2)))
                     (propertize relative
                                 'display relative
                                 'face 'mastodon-tl-timestamp-face
                                 'timestamp parsed-time))
           (concat (propertize " " 'display `(space :align-to (- right ,(length absolute) 2 )))
                   (propertize absolute
                               'face 'mastodon-tl-timestamp-face
                               'timestamp parsed-time))))
       (when detailed-p
         (let* ((app (alist-get 'application toot))
                (app-name (alist-get 'name app))
                (app-url (alist-get 'website app)))
           (when app
             (concat
              (propertize " via " 'face 'default)
              (propertize app-name
                          'face 'mastodon-display-name-face
                          'follow-link t
                          'mouse-face 'highlight
		          'mastodon-tab-stop 'shr-url
		          'shr-url app-url
                          'help-echo app-url
		          'keymap mastodon-tl--shr-map-replacement)))))
       "\n"
       )
      'favourited-p faved
      'boosted-p    boosted
      'bookmarked-p bookmarked
      'byline       t))))


;; HACK: Because of byline first, this has been changed
(defun mastodon-tl--goto-prev-toot ()
  "Jump to last toot header."
  (interactive)

  (let ((prev (previous-single-property-change (point) 'base-toot-id)))
    (if (not prev)
        (progn
          (mastodon-tl--update)
          (goto-char (+ (point-min) 1)))
      (goto-char (- prev 1))))

  (let ((prev (previous-single-property-change (point) 'base-toot-id)))
    (if (not prev)
        (goto-char (+ (point-min) 1))
      (goto-char (+ prev 1)))))


;; HACK: Because of byline first, this has been changed
(defun mastodon-tl--goto-next-toot ()
  "Jump to next toot header."
  (interactive)

  (let ((next (+ (next-single-property-change (point) 'base-toot-id) 2)))
    (if (> next (point-max))
        (mastodon-tl--more)
      (goto-char next))))

;; HACK: For display reason, the resulting string MUST be 12 characters long.
(defun mastodon-tl--relative-time-details (timestamp &optional current-time)
  "Return cons of (descriptive string . next change) for the TIMESTAMP.

Use the optional CURRENT-TIME as the current time (only used for
reliable testing).

The descriptive string is a human readable version relative to
the current time while the next change timestamp give the first
time that this description will change in the future.

TIMESTAMP is assumed to be in the past."
  (let* ((now (or current-time (current-time)))
         (time-difference (time-subtract now timestamp))
         (seconds-difference (float-time time-difference))
         (regular-response
          (lambda (seconds-difference multiplier unit-name)
            (let ((n (floor (+ 0.5 (/ seconds-difference multiplier)))))
              (cons (format "%12s" (format "%d %ss ago" n unit-name))
                    (* (+ 0.5 n) multiplier)))))
         (relative-result
          (cond
           ((< seconds-difference 60)
            (cons "         now"
                  60))
           ((< seconds-difference (* 1.5 60))
            (cons "1 minute ago"
                  90)) ;; at 90 secs
           ((< seconds-difference (* 60 59.5))
            (funcall regular-response seconds-difference 60 "min"))
           ((< seconds-difference (* 1.5 60 60))
            (cons "  1 hour ago"
                  (* 60 90))) ;; at 90 minutes
           ((< seconds-difference (* 60 60 23.5))
            (funcall regular-response seconds-difference (* 60 60) "hour"))
           ((< seconds-difference (* 1.5 60 60 24))
            (cons "   1 day ago"
                  (* 1.5 60 60 24))) ;; at a day and a half
           ((< seconds-difference (* 60 60 24 6.5))
            (funcall regular-response seconds-difference (* 60 60 24) "day"))
           ((< seconds-difference (* 1.5 60 60 24 7))
            (cons "one week ago"
                  (* 1.5 60 60 24 7))) ;; a week and a half
           ((< seconds-difference (* 60 60 24 7 52))
            (if (= 52 (floor (+ 0.5 (/ seconds-difference 60 60 24 7))))
                (cons "52 weeks ago"
                      (* 60 60 24 7 52))
              (funcall regular-response seconds-difference (* 60 60 24 7) "week")))
           ((< seconds-difference (* 1.5 60 60 24 365))
            (cons "one year ago"
                  (* 60 60 24 365 1.5))) ;; a year and a half
           (t
            (funcall regular-response seconds-difference (* 60 60 24 365.25) "year")))))
    (cons (car relative-result)
          (time-add timestamp (seconds-to-time (cdr relative-result))))))

;; Open Mastodon URLS directly in mastodon.el ... not working?
(add-to-list 'browse-url-handlers '("https?://[^/]+/@[^/]+/.*" . 'my-mastodon-open-at-point))

   (defun my-mastodon-open-at-point ()
     "Open the URL at point, or prompt if a URL is not found."
     (interactive)
     (mastodon-url-lookup (or (thing-at-point 'url) (read-string "URL: "))))

(provide 'bp-mastodon)
