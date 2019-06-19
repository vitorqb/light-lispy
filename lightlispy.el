;;; lightlispy.el --- Light Lispy -*- lexical-binding: t -*-

;; Copyright (C) 2019 -

;; Author: Vitor <vitorqb@gmail.com>
;; Version: 0.0.1
;; Maintainer: Vitor <vitorqb@gmail.com>
;; Created: 2019-06-18
;; Keywords: lispy structural-editting
;; Homepage: https://github.com/vitorqb/lightlispy

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; ***********************************
;;    *DISCLAIMER*
;; 
;; MOST OF THIS CODE IS FROM https://github.com/abo-abo/lispy/blob/master/lispy.el
;; All credits and rights for the code from the lispy package goes for the
;; author, Oleh Krehel.
;; ***********************************
;;
;; An extraction from the `abo-abo` `lispy` package containing only
;; structural editting, without code evaluation.


;;; code
(defvar lightlispy-right "[])}]"
  "Closing delimiter.")

(defvar lightlispy-ignore-whitespace nil
  "When set to t, function `lightlispy-right' will not clean up whitespace.")

(defvar lightlispy-outline "^;;\\(?:;[^#]\\|\\*+\\)"
  "Outline delimiter.")

(defvar lightlispy-left "[([{]"
  "Opening delimiter.")

(defvar lightlispy-right "[])}]"
  "Closing delimiter.")

(defvar-local lightlispy-outline-header ";;"
  "Store the buffer-local outline start.")

(defvar lightlispy-pos-ring (make-ring 100)
  "Ring for point and mark position history.")

(defcustom lightlispy-verbose t
  "If t, lightlispy will display some messages on error state.
These messages are similar to \"Beginning of buffer\" error for
`backward-char' and can safely be ignored."
  :type 'boolean
  :group 'lightlispy)

(defsubst lightlispy-looking-back (regexp)
  "Forward to (`looking-back' REGEXP)."
  (looking-back regexp (line-beginning-position)))

(defsubst lightlispy-right-p ()
  "Return t if after variable `lightlispy-right'."
  (looking-back lightlispy-right
                (line-beginning-position)))

(defsubst lightlispy-left-p ()
  "Return t if before variable `lightlispy-left'."
  (looking-at lightlispy-left))

;; Should we keep this?
(declare-function lightlispy-bounds-python-block "le-python")

(defmacro lightlispy-dotimes (n &rest bodyform)
  "Execute N times the BODYFORM unless an error is signaled.
Return nil if couldn't execute BODYFORM at least once.
Otherwise return the amount of times executed."
  (declare (indent 1)
           (debug (form body)))
  `(let ((i 0))
     (catch 'result
       (condition-case e
           (progn
             (while (<= (cl-incf i) ,n)
               ,@bodyform)
             ,n)
         (error
          (when (eq (car e) 'buffer-read-only)
            (message "Buffer is read-only: %s" (current-buffer)))
          (cl-decf i)
          (and (> i 0) i))))))

(defmacro lightlispy-save-excursion (&rest body)
  "More intuitive (`save-excursion' BODY)."
  (declare (indent 0))
  `(let ((out (save-excursion
                ,@body)))
     (when (lightlispy-bolp)
       (back-to-indentation))
     out))

(defun lightlispy-bolp ()
  "Return t if point is at beginning of line, after optional spaces."
  (save-excursion
    (skip-chars-backward " \t")
    (bolp)))

(defun lightlispy--symbolp (str)
  "Return t if STR is a symbol."
  (string-match "\\`\\(?:\\sw\\|\\s_\\)+\\'" str))

(defun lightlispy--mark (bnd)
  "Mark BND.  BND is a cons of beginning and end positions."
  (setq deactivate-mark nil)
  (set-mark (car bnd))
  (goto-char (cdr bnd)))

(defun lightlispy--exit-string ()
  "When in string, go to its beginning."
  (let ((s (syntax-ppss)))
    (when (nth 3 s)
      (goto-char (nth 8 s)))))

(defun lightlispy--in-comment-p ()
  "Test if point is inside a comment."
  (save-excursion
    (unless (eolp)
      (forward-char 1))
    (nth 4 (syntax-ppss))))

(defun lightlispy--in-string-or-comment-p ()
  "Test if point is inside a string or a comment."
  (let* ((sp (syntax-ppss))
         (beg (nth 8 sp)))
    (when (or (eq (char-after beg) ?\")
              (nth 4 sp))
      beg)))

(defun lightlispy-after-string-p (str)
  "Return t if the string before point is STR."
  (string=
   (buffer-substring
    (max
     (- (point) (length str))
     (point-min))
    (point))
   str))

(defun lightlispy--string-dwim (&optional bounds)
  "Return the string that corresponds to BOUNDS.
`lightlispy--bounds-dwim' is used if BOUNDS is nil."
  (setq bounds (or bounds (lightlispy--bounds-dwim)))
  (buffer-substring-no-properties (car bounds) (cdr bounds)))

(defun lightlispy--ensure-visible ()
  "Remove overlays hiding point."
  (let ((overlays (overlays-at (point)))
        ov expose)
    (while (setq ov (pop overlays))
      (if (and (invisible-p (overlay-get ov 'invisible))
               (setq expose (overlay-get ov 'isearch-open-invisible)))
          (funcall expose ov)))))

(defun lightlispy-complain (msg)
  "Display MSG if `lightlispy-verbose' is t."
  (when (and lightlispy-verbose (null noninteractive))
    (message "%s: %s"
             (propertize
              (prin1-to-string
               this-command)
              'face 'font-lock-keyword-face)
             msg)
    nil))

(defun lightlispy--indent-region (beg end)
  "Indent region BEG END without reporting progress."
  (save-excursion
    (setq end (copy-marker end))
    (goto-char beg)
    (while (< (point) end)
      (or (and (bolp) (eolp))
          (indent-according-to-mode))
      (forward-line 1))
    (move-marker end nil)))

(defun lightlispy--skip-delimiter-preceding-syntax-backward ()
  "Move backwards past syntax that could precede an opening delimiter such as '.
Specifically, move backwards to the closest whitespace char or opening delimiter
or to the beginning of the line."
  (re-search-backward (concat "[[:space:]]" "\\|"
                              lightlispy-left "\\|"
                              "^"))
  (goto-char (match-end 0)))

(defun lightlispy-different ()
  "Switch to the different side of current sexp."
  (interactive)
  (cond ((and (region-active-p)
              (not (= (region-beginning) (region-end))))
         (exchange-point-and-mark))
        ((lightlispy-right-p)
         (backward-list))
        ((lightlispy-left-p)
         (forward-list))
        (t
         (user-error "Unexpected"))))

(defun lightlispy-right (arg)
  "Move outside list forwards ARG times.
Return nil on failure, t otherwise."
  (interactive "p")
  (lightlispy--remember)
  (when (bound-and-true-p abbrev-mode)
    (ignore-errors (expand-abbrev)))
  (cond ((region-active-p)
         (lightlispy-mark-right arg))
        ((looking-at lightlispy-outline)
         (lightlispy-outline-right))
        (t
         (lightlispy--out-forward arg))))

(defun lightlispy-left (arg)
  "Move outside list forwards ARG times.
Return nil on failure, t otherwise."
  (interactive "p")
  (lightlispy--remember)
  (cond ((region-active-p)
         (lightlispy-mark-left arg))
        ((looking-at lightlispy-outline)
         (lightlispy-outline-left))
        (t
         (or (lispy--out-backward arg)
             (ignore-errors
               (up-list -1))))))

(defun lightlispy--bounds-string ()
  "Return bounds of current string."
  (unless (lightlispy--in-comment-p)
    (let ((beg (or (nth 8 (syntax-ppss))
                   (and (eq (char-after (point)) ?\")
                        (not (eq ?\\ (char-before)))
                        (point)))))
      (when (and beg (not (comment-only-p beg (1+ (point)))))
        (ignore-errors
          (cons beg (save-excursion
                      (goto-char beg)
                      (forward-sexp)
                      (point))))))))

(defun lightlispy--out-forward (arg)
  "Move outside list forwards ARG times.
Return nil on failure, (point) otherwise."
  (lightlispy--exit-string)
  (catch 'break
    (dotimes (_i arg)
      (if (ignore-errors (up-list) t)
          (if buffer-read-only
              (deactivate-mark)
            (unless lightlispy-ignore-whitespace
              (lispy--remove-gaps)
              (lispy--indent-for-tab)))
        (when (lispy-left-p)
          (forward-list))
        (throw 'break nil)))
    (point)))

(defun lightlispy--remove-gaps ()
  "Remove dangling `\\s)'."
  (when (and (lightlispy-right-p)
             (looking-back "[^ \t\n]\\([ \t\n]+\\)\\s)"
                           (condition-case nil
                               (save-excursion
                                 (backward-list)
                                 (point))
                             (error (point-min))))
             (not (eq ?\\ (aref (match-string 0) 0))))
    (unless (save-excursion
              (save-match-data
                (goto-char (match-beginning 1))
                (lightlispy--in-string-or-comment-p)))
      (delete-region (match-beginning 1)
                     (match-end 1)))))

(defun lightlispy--remember ()
  "Store the current point and mark in history."
  (let* ((emptyp (zerop (ring-length lightlispy-pos-ring)))
         (top (unless emptyp
                (ring-ref lightlispy-pos-ring 0))))
    (if (region-active-p)
        (let* ((bnd (lightlispy--bounds-dwim))
               (bnd (cons
                     (move-marker (make-marker) (car bnd))
                     (move-marker (make-marker) (cdr bnd)))))
          (when (or emptyp
                    (not (equal bnd top)))
            (ring-insert lightlispy-pos-ring bnd)))
      (when (or emptyp
                (not (equal (point-marker) top)))
        (ring-insert lightlispy-pos-ring (point-marker))))))

(defun lightlispy--beginning-of-comment ()
  "Go to beginning of comment on current line."
  (end-of-line)
  (comment-beginning)
  (let ((cs (comment-search-backward (line-beginning-position) t)))
    (or
     (when cs
       (goto-char cs))
     (and (looking-at (concat "^" lightlispy-outline-header))
          (point)))))

(defun lightlispy-outline-left ()
  "Move left."
  (interactive)
  (when (looking-at lightlispy-outline)
    (lightlispy--remember)
    (let ((level-up (1- (funcall outline-level))))
      (when (> level-up 0)
        (re-search-backward (format "^#\\*\\{1,%d\\} " level-up) nil t)))))

(defun lightlispy--sub-slurp-forward (arg)
  "Grow current marked symbol by ARG words forwards.
Return the amount of successful grow steps, nil instead of zero."
  (when (looking-at "\\s_")
    (let ((end (cdr (bounds-of-thing-at-point 'symbol)))
          prev)
      (lightlispy-dotimes arg
        (setq prev (point))
        (forward-word 1)
        (when (> (point) end)
          (goto-char prev)
          (throw 'result (1- i)))))))

(defun lightlispy--slurp-backward ()
  "Grow current sexp backward by one sexp."
  (let ((pt (point))
        (char (char-after)))
    (backward-sexp)
    (delete-region pt (1+ pt))
    (insert char)
    (backward-char)))

(defun lightlispy--sub-slurp-backward (arg)
  "Grow current marked symbol by ARG backwards.
Return the amount of successful grow steps, nil instead of zero."
  (when (lightlispy-looking-back "\\s_")
    (let ((beg (car (bounds-of-thing-at-point 'symbol)))
          prev)
      (lightlispy-dotimes arg
        (setq prev (point))
        (backward-word 1)
        (when (< (point) beg)
          (goto-char prev)
          (throw 'result (1- i)))))))

(defun lightlispy--slurp-forward ()
  "Grow current sexp forward by one sexp."
  (let ((pt (point))
        (char (char-before)))
    (skip-chars-forward " \t")
    (delete-region pt (point))
    (unless (or (lightlispy-after-string-p "()")
                (lightlispy-after-string-p "[]")
                (lightlispy-after-string-p "{}")
                (eolp))
      (insert " "))
    (when (ignore-errors
            (forward-sexp) t)
      (delete-region (1- pt) pt)
      (insert char))))

(defun lightlispy--barf-forward ()
  "Shrink current sexp forward by one sexp."
  (let ((pt (point))
        (char (char-after)))
    (unless (looking-at "()")
      (forward-char)
      (forward-sexp)
      (delete-region pt (1+ pt))
      (skip-chars-forward " \n	")
      (insert char)
      (backward-char)
      (indent-region pt (point))
      (lightlispy--reindent 1))))

(defun lightlispy--barf-backward ()
  "Shrink current sexp backward by one sexp."
  (let ((pt (point))
        (char (char-before)))
    (unless (lightlispy-after-string-p "()")
      (backward-char)
      (backward-sexp)
      (skip-chars-backward " \n	")
      (while (lightlispy--in-comment-p)
        (goto-char (comment-beginning))
        (skip-chars-backward " \n	"))
      (delete-region (1- pt) pt)
      (insert char)
      (lightlispy--indent-region (point) pt))))

(defun lightlispy-outline-right ()
  "Move right."
  (interactive)
  (let ((pt (point))
        result)
    (save-restriction
      (org-narrow-to-subtree)
      (forward-char)
      (if (re-search-forward lightlispy-outline nil t)
          (progn
            (goto-char (match-beginning 0))
            (setq result t))
        (goto-char pt)))
    (lightlispy--ensure-visible)
    result))

(defun lightlispy-mark-left (arg)
  "Go left ARG times and mark."
  (interactive "p")
  (if (lightlispy-mark-right arg)
      (lightlispy-different)
    (when (= (point) (region-end))
      (exchange-point-and-mark))))

(defun lightlispy-mark-right (arg)
  "Go right ARG times and mark."
  (interactive "p")
  (let* ((pt (point))
         (mk (mark))
         (lightlispy-ignore-whitespace t)
         (r (lightlispy--out-forward arg)))
    (deactivate-mark)
    (if (or (= pt (point))
            (= mk (point))
            (and (region-active-p)
                 (= (region-beginning)
                    (region-end))))
        (progn
          (lightlispy-complain "can't go any further")
          (if (> mk pt)
              (lightlispy--mark (cons pt mk))
            (lightlispy--mark (cons mk pt)))
          nil)
      (lightlispy--mark
       (lightlispy--bounds-dwim))
      r)))

(defun lightlispy--reindent (&optional arg)
  "Reindent current sexp.  Up-list ARG times before that."
  (cond ((region-active-p)
         (indent-region (region-beginning)
                        (region-end)))
        (arg
         (lightlispy-save-excursion
           (lightlispy--out-forward arg)
           (backward-list)
           (indent-sexp)))

        ((lightlispy-right-p)
         (save-excursion
           (backward-list)
           (indent-sexp)))

        ((lightlispy-left-p)
         (indent-sexp))

        (t
         (save-excursion
           (lightlispy--out-forward 1)
           (backward-list)
           (indent-sexp)))))

(defun lightlispy--bounds-comment ()
  "Return bounds of current comment."
  (and (lightlispy--in-comment-p)
       (save-excursion
         (when (lightlispy--beginning-of-comment)
           (let ((pt (point)))
             (while (and (lightlispy--in-comment-p)
                         (forward-comment -1)
                         (lightlispy-looking-back "^[[:space:]]*")
                         (= 1 (- (count-lines (point) pt)
                                 (if (bolp) 0 1))))
               (setq pt (point)))
             (goto-char pt))
           (if (looking-at "#|")
               (cons (point)
                     (progn
                       (comment-forward)
                       (point)))
             (let ((beg (lightlispy--beginning-of-comment))
                   (pt (point))
                   (col (current-column)))
               (while (and (lightlispy--in-comment-p)
                           (forward-comment 1)
                           (lightlispy--beginning-of-comment)
                           (and (= 1 (- (count-lines pt (point))
                                        (if (bolp) 0 1)))
                                ;; count comments starting in different columns
                                ;; as separate
                                (= col (current-column))
                                ;; if there's code in between,
                                ;; count comments as separate
                                (lightlispy-looking-back "^\\s-*")))
                 (setq pt (point)))
               (goto-char pt)
               (end-of-line)
               (cons beg (point))))))))

(defun lightlispy-forward (arg)
  "Move forward list ARG times or until error.
Return t if moved at least once,
otherwise call function `lightlispy-right' and return nil."
  (interactive "p")
  (when (= arg 0)
    (setq arg 2000))
  (lightlispy--exit-string)
  (let ((bnd (lightlispy--bounds-comment)))
    (when bnd
      (goto-char (1+ (cdr bnd)))))
  (let ((pt (point))
        (r (lightlispy-dotimes arg
             (when (= (point) (point-max))
               (error "Reached end of buffer"))
             (forward-list))))
    ;; `forward-list' returns true at and of buffer
    (if (or (null r)
            (= pt (point))
            (and (not (lightlispy-right-p))
                 (progn
                   (backward-list)
                   (forward-list)
                   (= pt (point)))))
        (prog1 nil
          (lightlispy--out-forward 1))
      (point))))

(defun lightlispy-flow (arg)
  "Move inside list ARG times.
Don't enter strings or comments.
Return nil if can't move."
  (interactive "p")
  (lightlispy--remember)
  (let ((pt (point))
        r)
    (cond
      ((and (lightlispy-bolp)
            (looking-at ";"))
       (setq r (lightlispy--re-search-in-code lightlispy-left 'forward arg)))
      ((lightlispy-left-p)
       (setq r (lightlispy--re-search-in-code lightlispy-left 'forward arg)))
      ((lightlispy-right-p)
       (backward-char)
       (when (setq r (lightlispy--re-search-in-code lightlispy-right 'backward arg))
         (forward-char))))
    (or r
        (progn
          (goto-char pt)
          nil))))

(defun lightlispy--re-search-in-code (regexp direction &optional count)
  "Move to the next REGEXP in DIRECTION, COUNT times.
DIRECTION is either 'forward or 'backward.
Return the amount of successful moves, or nil otherwise."
  (setq count (or count 1))
  (let ((to-move (abs count))
        (advancer
         (if (eq direction 'forward)
             (if (> count 0)
                 #'re-search-forward
               #'re-search-backward)
           (if (> count 0)
               #'re-search-backward
             #'re-search-forward)))
        (pt (point)))
    (if (and (eq direction 'forward) (> count 0))
        (when (looking-at regexp)
          (goto-char (match-end 0))))
    (while (and (> to-move 0)
                (funcall advancer regexp nil t))
      (unless (lightlispy--in-string-or-comment-p)
        (cl-decf to-move)))
    (if (= to-move (abs count))
        (progn
          (goto-char pt)
          nil)
      (if (eq direction 'forward)
          (goto-char (match-beginning 0)))
      (- count to-move))))

(defun lightlispy-backward (arg)
  "Move backward list ARG times or until error.
If couldn't move backward at least once, move up backward and return nil."
  (interactive "p")
  (when (= arg 0)
    (setq arg 2000))
  (lightlispy--exit-string)
  (let ((bnd (lightlispy--bounds-comment)))
    (when bnd
      (goto-char (car bnd))))
  (let ((pt (point))
        (r (lightlispy-dotimes arg
             (when (= (point) (point-min))
               (error "Reached beginning of buffer"))
             (backward-list))))
    ;; `backward-list' returns true at beginning of buffer
    (if (or (null r)
            (= pt (point))
            (and (not (lightlispy-left-p))
                 (progn
                   (forward-list)
                   (backward-list)
                   (= pt (point)))))
        (prog1 nil
          (condition-case nil
              (progn
                (lightlispy--out-forward 1)
                (backward-list))
            (error
             (progn
               (goto-char pt)
               (up-list -1)))))
      (point))))

(defun lightlispy--insert-or-call (def plist)
  "Return a lambda to call DEF if position is special.
Otherwise call `self-insert-command'.
PLIST currently accepts:
- :disable with a mode to disable
- :override with a lambda to conditionally abort command"
  (let ((disable (plist-get plist :disable))
        (override (plist-get plist :override))
        (inserter (plist-get plist :inserter)))
    `(lambda ()
       ,(format "Call `%s' when special, self-insert otherwise.\n\n%s"
                (symbol-name def) (documentation def))
       (interactive)
       ,@(when disable `((,disable -1)))
       (unless (looking-at lightlispy-outline)
         (lightlispy--ensure-visible))
       (cond ,@(cond ((null override) nil)
                     ((functionp override)
                      `((funcall ,override)))
                     ((eq (car override) 'cond)
                      (cdr override))
                     (t
                      (error "Unexpected :override %S" override)))

             ((region-active-p)
              (call-interactively ',def))

             ((lightlispy--in-string-or-comment-p)
              (setq this-command 'self-insert-command)
              (call-interactively 'self-insert-command))

             ((or (lightlispy-left-p)
                  (lightlispy-right-p)
                  (and (lightlispy-bolp)
                       (or (looking-at lightlispy-outline-header)
                           (looking-at lightlispy-outline))))
              (call-interactively ',def))

             (t
              (setq this-command 'self-insert-command)
              (call-interactively
               (quote
                ,(or inserter
                     'self-insert-command))))))))

(defun lightlispy-slurp (arg)
  "Grow current sexp by ARG sexps.
If ARG is zero, grow as far as possible. If ARG is -1, grow until the end or
beginning of the line. If it is not possible to slurp to the end of the line,
slurp as far as possible within the line. If before a multi-line list, slurp to
the end of the line where that list ends."
  (interactive "p")
  (if (region-active-p)
      (if (= (point) (region-end))
          (cond ((= arg 0)
                 (while (and (lightlispy-dotimes 1 (forward-sexp 1))
                             (not (looking-at "\\'")))))
                ((= arg -1)
                 (while (and (not (looking-at (concat lightlispy-right "*$")))
                             (lightlispy-dotimes 1 (forward-sexp 1)))))
                ((or (looking-at "\\s_")
                     (save-excursion
                       (goto-char (region-beginning))
                       (and (not (lightlispy-left-p))
                            (lightlispy-looking-back "\\s_"))))
                 (lightlispy--sub-slurp-forward arg))
                ((looking-at "[\n ]+;")
                 (goto-char (match-end 0))
                 (goto-char (cdr (lightlispy--bounds-comment))))
                (t
                 (lightlispy-dotimes arg
                   (forward-sexp 1))))
        (cond ((= arg 0)
               (while (and (lightlispy-dotimes 1 (forward-sexp -1))
                           (not (looking-at "\\`")))))
              ((= arg -1)
               (while (and (not (lightlispy-looking-back "^[[:space:]]*"))
                           (lightlispy-dotimes 1 (forward-sexp -1)))))
              ((or (and (not (lightlispy-left-p))
                        (lightlispy-looking-back "\\s_"))
                   (save-excursion
                     (goto-char (region-end))
                     (looking-at "\\s_")))
               (lightlispy--sub-slurp-backward arg))
              ((save-excursion
                 (skip-chars-backward " \n")
                 (lightlispy--in-comment-p))
               (skip-chars-backward " \n")
               (goto-char (car (lightlispy--bounds-comment))))
              (t
               (lightlispy-dotimes arg
                 (forward-sexp -1)))))
    (if (lightlispy-right-p)
        (cond ((= arg 0)
               (let ((last-pos (point)))
                 (while (and (lightlispy-dotimes 1
                               (lightlispy--slurp-forward)
                               (lightlispy--reindent))
                             (not (= (point) last-pos)))
                   (setq last-pos (point)))))
              ((= arg -1)
               (while (and (not (looking-at (concat "\\("
                                                    lightlispy-right
                                                    "\\|$\\)")))
                           (lightlispy-dotimes 1
                             (lightlispy--slurp-forward)))))
              (t
               (lightlispy-dotimes arg
                 (lightlispy--slurp-forward))))
      (if (lightlispy-left-p)
          (cond ((= arg 0)
                 ;; lightlispy--slurp-backward errors when reaching another delimiter
                 (while (and (lightlispy-dotimes 1
                               (lightlispy--slurp-backward))
                             (not (lightlispy-looking-back "\\`")))))
                ((= arg -1)
                 (while (and (not (lightlispy-looking-back "^[[:space:]]*"))
                             (lightlispy-dotimes 1
                               (lightlispy--slurp-backward)))))
                (t
                 (lightlispy-dotimes arg
                   (lightlispy--slurp-backward))))))
    (lightlispy--reindent)))

(defun lightlispy-barf (arg)
  "Shrink current sexp or region by ARG sexps."
  (interactive "p")
  (cond ((region-active-p)
         (let* ((bnd (lightlispy--bounds-dwim))
                (str (lightlispy--string-dwim bnd))
                (one-symbolp (lightlispy--symbolp str)))
           (if (= (point) (region-end))
               (cond (one-symbolp
                      (lightlispy-dotimes arg
                        (if (re-search-backward "\\sw\\s_+" (region-beginning) t)
                            (forward-char 1)
                          (throw 'result i))))
                     ((lightlispy--in-comment-p)
                      (goto-char (car (lightlispy--bounds-comment)))
                      (if (= (point) (region-beginning))
                          (goto-char (cdr (lightlispy--bounds-comment)))
                        (skip-chars-backward " \n")))
                     (t
                      (cl-incf arg)
                      (lightlispy-dotimes arg
                        (lightlispy--backward-sexp-or-comment))
                      (when (< (point) (car bnd))
                        (goto-char (car bnd)))
                      (lightlispy--forward-sexp-or-comment)))
             (cond (one-symbolp
                    (lightlispy-dotimes arg
                      (if (re-search-forward "\\s_+\\sw" (region-end) t)
                          (backward-char 1)
                        (throw 'result i))))
                   ((lightlispy--in-comment-p)
                    (goto-char (cdr (lispy--bounds-comment)))
                    (if (= (region-beginning) (region-end))
                        (goto-char (car bnd))
                      (skip-chars-forward " \n")))
                   (t
                    (save-restriction
                      (narrow-to-region (point-min)
                                        (region-end))
                      (cl-incf arg)
                      (lightlispy-dotimes arg
                        (lightlispy--forward-sexp-or-comment))
                      (if (lightlispy--in-comment-p)
                          (goto-char (car (lightlispy--bounds-comment)))
                        (forward-sexp -1))
                      (widen)))))))

        ((looking-at "()"))

        ((lightlispy-right-p)
         (lightlispy-dotimes arg
           (lightlispy--barf-backward)))

        ((lightlispy-left-p)
         (lightlispy-dotimes arg
           (lightlispy--barf-forward)))))

(defun lightlispy-down (arg)
  "Move down ARG times inside current list."
  (interactive "p")
  (lightlispy--remember)
  (cond ((region-active-p)
         (let ((leftp (= (point) (region-beginning))))
           (when leftp
             (exchange-point-and-mark))
           (cond ((save-excursion
                    (skip-chars-forward " \n")
                    (eobp)))
                 ((lightlispy--symbolp (lightlispy--string-dwim))
                  (lightlispy-dotimes arg
                    (when (lightlispy-slurp 1)
                      (lightlispy-different)
                      (lightlispy-barf 1)
                      (lightlispy-different))))

                 ((looking-at "[\n ]+\\(;\\)")
                  (deactivate-mark)
                  (goto-char (match-beginning 1))
                  (lightlispy--mark (lightlispy--bounds-comment)))

                 (t
                  (lightlispy-dotimes arg
                    (forward-sexp 1)
                    (lightlispy-different)
                    (if (lightlispy--in-comment-p)
                        (progn
                          (goto-char (1+ (cdr (lightlispy--bounds-comment))))
                          (skip-chars-forward "\n"))
                      (forward-sexp 2)
                      (forward-sexp -1))
                    (lightlispy-different))))
           (when leftp
             (exchange-point-and-mark))))

        ((lightlispy-left-p)
         (lightlispy-forward arg)
         (let ((pt (point))
               (lightlispy-ignore-whitespace t))
           (if (lightlispy-forward 1)
               (lightlispy-backward 1)
             (goto-char pt)
             (lightlispy-different))))

        ((lightlispy-right-p)
         (let ((pt (point)))
           (unless (lightlispy-forward arg)
             (goto-char pt))))

        ((or (looking-at lightlispy-outline)
             (and (bolp) (looking-at ";")))
         (let ((pt (point)))
           (lightlispy-dotimes arg
             (outline-next-visible-heading 1)
             (if (looking-at lightlispy-outline)
                 (setq pt (point))
               (goto-char pt)
               (error "Last outline reached")))))

        (t
         (lightlispy-forward 1)
         (lightlispy-backward 1)))
  (lightlispy--ensure-visible))

(defun lightlispy--bounds-dwim ()
  "Return a cons of region bounds if it's active.
Otherwise return cons of current string, symbol or list bounds."
  (let (bnd)
    (cond ((region-active-p)
           (cons (region-beginning)
                 (region-end)))
          ((and (setq bnd (lightlispy--bounds-string))
                (or (eq (point) (car bnd))
                    (eq (point) (1- (cdr bnd)))))
           bnd)
          ((looking-at lightlispy-outline)
           (save-excursion
             (cons
              (progn
                (outline-end-of-heading)
                (1+ (point)))
              (progn
                (outline-end-of-subtree)
                (skip-chars-backward "\n")
                (when (setq bnd (lightlispy--bounds-comment))
                  (goto-char (1- (car bnd))))
                (point)))))
          ((save-excursion
             (when (lightlispy-right-p)
               (backward-list))
             (and (or (looking-at (concat "[^[:space:]\n]*" lightlispy-left))
                      (looking-at "[`'#]"))
                  (setq bnd (bounds-of-thing-at-point 'sexp))))
           (save-excursion
             (goto-char (car bnd))
             (lightlispy--skip-delimiter-preceding-syntax-backward)
             (cons (point) (cdr bnd))))
          ((looking-at ";;")
           (lightlispy--bounds-comment))
          ;; !!!! TODO Should we keep this?
          ((and (eq major-mode 'python-mode)
                (lightlispy-bolp))
           (lightlispy-bounds-python-block))
          (t
           (let ((res (ignore-errors
                        (bounds-of-thing-at-point
                         (if (looking-at lightlispy-right)
                             'symbol
                           'sexp)))))
             (if res
                 (save-excursion
                   (goto-char (cdr res))
                   (lightlispy--in-string-or-comment-p)
                   (skip-chars-backward "[.,]")
                   (cons (car res) (point)))
               (or
                (ignore-errors
                  (bounds-of-thing-at-point 'symbol))
                (and (lightlispy-looking-back "\" *")
                     (save-excursion
                       (goto-char (match-beginning 0))
                       (lightlispy--bounds-string)))
                (ignore-errors
                  (bounds-of-thing-at-point 'sentence))
                (ignore-errors
                  (save-excursion
                    (backward-word 1)
                    (bounds-of-thing-at-point 'symbol)))
                (ignore-errors
                  (save-excursion
                    (forward-word 1)
                    (bounds-of-thing-at-point 'symbol))))))))))

(defun lightlispy-define-key (keymap key def &rest plist)
  "Forward to (`define-key' KEYMAP KEY FUNC).
FUNC is obtained from (`lightlispy--insert-or-call' DEF PLIST)."
  (declare (indent 3))
  (let ((func (defalias (intern (concat "special-" (symbol-name def)))
                (lightlispy--insert-or-call def plist))))
    (define-key keymap (kbd key) func)))

(defvar lightlispy-mode-map (make-sparse-keymap))
(defvar lightlispy-mode-map-special
  (let ((map (make-sparse-keymap)))
    (lightlispy-define-key map "l" 'lightlispy-right)
    (lightlispy-define-key map "h" 'lightlispy-left)
    (lightlispy-define-key map "f" 'lightlispy-flow)
    (lightlispy-define-key map "j" 'lightlispy-down)
    (lightlispy-define-key map "]" 'lightlispy-forward)
    map))

;; !!!! TODO -> Someday we might want to add the themes
(setq lightlispy-mode-map lightlispy-mode-map-special)

(define-minor-mode lightlispy-mode
  "A light version of lispy-mode - focusing only on structural editing
for lisps."
  :keymap lightlispy-mode-map
  :group 'lightlispy
  :lighter "LLY")

;;; orgext.el ends here
