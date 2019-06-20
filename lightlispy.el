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
(defvar geiser-active-implementations)
(defvar clojure-align-forms-automatically)
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

(defvar-local lightlispy-bind-var-in-progress nil
  "When t, `lightlispy-mark-symbol' will exit `iedit'.")

(defvar lightlispy-pos-ring (make-ring 100)
  "Ring for point and mark position history.")

(defvar lightlispy-parens-only-left-in-string-or-comment t
  "Whether \"(\" should insert only the left paren in strings and comments.")

(defvar lispy-map-input-overlay nil
  "The input overlay for mapping transformations.")

(defvar lightlispy-brackets-preceding-syntax-alist
  '((clojure-mode . ("[`']" "#[A-z.]*"))
    (clojurescript-mode . ("[`']" "#[A-z.]*"))
    (clojurec-mode . ("[`']" "#[A-z.]*"))
    (cider-repl-mode . ("[`']" "#[A-z.]*"))
    (cider-clojure-interaction-mode . ("[`']" "#[A-z.]*"))
    (scheme-mode . ("[#`',@]+" "#hash"))
    (t . nil))
  "An alist of `major-mode' to a list of regexps.
Each regexp describes valid syntax that can precede an opening bracket in that
major mode. These regexps are used to determine whether to insert a space for
`lightlispy-brackets'.")

(defvar lightlispy-braces-preceding-syntax-alist
  '((clojure-mode . ("[`'^]" "#[:]*[A-z.:]*"))
    (clojurescript-mode . ("[`'^]" "#[:]*[A-z.:]*"))
    (clojurec-mode . ("[`'^]" "#[:]*[A-z.:]*"))
    (cider-repl-mode . ("[`'^]" "#[:]*[A-z.:]*"))
    (cider-clojure-interaction-mode . ("[`'^]" "#[:]*[A-z.:]*"))
    (t . nil))
  "An alist of `major-mode' to a list of regexps.
Each regexp describes valid syntax that can precede an opening brace in that
major mode. These regexps are used to determine whether to insert a space for
`lightlispy-braces'.")

(defvar lightlispy-parens-preceding-syntax-alist
  '((lisp-mode . ("[#`',.@]+" "#[0-9]*" "#[.,Ss+-]" "#[0-9]+[=Aa]"))
    (emacs-lisp-mode . ("[#`',@]+" "#s" "#[0-9]+="))
    (clojure-mode . ("[`'~@]+" "#" "#\\?@?"))
    (clojurescript-mode . ("[`'~@]+" "#" "#\\?@?"))
    (clojurec-mode . ("[`'~@]+" "#" "#\\?@?"))
    (cider-repl-mode . ("[`'~@]+" "#" "#\\?@?"))
    (cider-clojure-interaction-mode . ("[`'~@]+" "#" "#\\?@?"))
    (scheme-mode . ("[#`',@]+" "#hash"))
    (t . ("[`',@]+")))
  "An alist of `major-mode' to a list of regexps.
Each regexp describes valid syntax that can precede an opening paren in that
major mode. These regexps are used to determine whether to insert a space for
`lightlispy-parens'.")

(defvar lightlispy-no-indent-modes '(minibuffer-inactive-mode
                                     comint-mode)
  "List of major modes where `indent-for-tab-command' should not be used.
`lightlispy--indent-for-tab' will do nothing if the current mode or any of its parent
modes is in this list.")

(defvar lightlispy-clojure-modes
  '(clojure-mode clojurescript-mode clojurex-mode clojurec-mode)
  "Modes for which clojure related functions are appropriate.")

(defvar lightlispy-do-fill nil
  "If t, `lightlispy-insert-1' will try to fill.")

(defvar lightlispy--braces-table
  (let ((table (make-char-table 'syntax-table nil)))
    (modify-syntax-entry ?\{ "(}  " table)
    (modify-syntax-entry ?\} "){  " table)
    (modify-syntax-entry ?\[ "(]  " table)
    (modify-syntax-entry ?\] ")[  " table)
    (modify-syntax-entry ?\( "()  " table)
    (modify-syntax-entry ?\) ")(  " table)
    table)
  "Syntax table for paired braces.")

(defvar lightlispy-delete-sexp-from-within nil
  "When cursor is adjacent to an opening or closing pair,
`lightlispy-delete' or `lightlispy-delete-backward' toward the delimiter
will kill the whole sexp (string or list).")

(defvar lightlispy-delete-backward-recenter -20
  "When cursor is near top of screen when calling
  `lightlispy-delete-backward', recenter cursor with arg.")

(defcustom lightlispy-safe-actions-ignore-strings t
  "When non-nil, don't try to act safely in strings.
Any unmatched delimiters inside of strings will be copied or deleted. This only
applies when `lightlispy-safe-delete', `lightlispy-safe-copy', and/or `lightlispy-safe-paste'
are non-nil."
  :group 'lightlispy
  :type 'boolean)

(defcustom lightlispy-safe-actions-ignore-comments t
  "When non-nil, don't try to act safely in comments.
Any unmatched delimiters inside of comments will be copied or deleted. This only
applies when `lightlispy-safe-delete', `lightlispy-safe-copy', and/or `lightlispy-safe-paste'
are non-nil."
  :group 'lightlispy
  :type 'boolean)

(defcustom lightlispy-safe-actions-no-pull-delimiters-into-comments nil
  "When non-nil, don't pull unmatched delimiters into comments when deleting.
This prevents the accidental unbalancing of expressions from commenting out
delimiters. This only applies when `lightlispy-safe-delete', `lightlispy-safe-copy',
and/or `lightlispy-safe-paste' are non-nil."
  :group 'lightlispy
  :type 'boolean)

(defcustom lightlispy-no-space nil
  "When non-nil, don't insert a space before parens/brackets/braces/colons."
  :type 'boolean
  :group 'lightlispy)
(make-variable-buffer-local 'lightlispy-no-space)

(defcustom lightlispy-insert-space-after-wrap t
  "When non-nil, insert a space after the point when wrapping.
This applies to the commands that use `lightlispy-pair'."
  :group 'lightlispy
  :type 'boolean)

(defcustom lightlispy-verbose t
  "If t, lightlispy will display some messages on error state.
These messages are similar to \"Beginning of buffer\" error for
`backward-char' and can safely be ignored."
  :type 'boolean
  :group 'lightlispy)

(defcustom lightlispy-move-after-commenting t
  "When non-nil, adjust point to next sexp after commenting out a
  sexp. If at last sexp in list, move out and backwards to
  enclosing sexp."
  :type 'boolean
  :group 'lightlispy)

(defcustom lightlispy-comment-use-single-semicolon nil
  "When non-nil, prefer single semicolons for comments at the
  right of the source code (after lightlispy-right or at eol)."
  :type 'boolean
  :group 'lightlispy)

(defcustom lightlispy-safe-delete nil
  "When non-nil, killing/deleting an active region keeps delimiters balanced.
This applies to `lightlispy-delete', `lightlispy-kill-at-point', `lightlispy-paste', and
`lightlispy-delete-backward'. This also applies to `lightlispy-yank' when
`delete-selection-mode' is non-nil."
  :group 'lightlispy
  :type 'boolean)

(defcustom lightlispy-safe-threshold 1500
  "The max size of an active region that lightlispy will try to keep balanced.
This only applies when `lightlispy-safe-delete', `lightlispy-safe-copy', and/or
`lightlispy-safe-paste' are non-nil."
  :group 'lightlispy
  :type 'number)

(defcustom lightlispy-close-quotes-at-end-p nil
  "If t, when pressing the `\"' at the end of a quoted string, it will move you past the end quote."
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

(defun lightlispy--splice-to-str (sexp)
  "Return the printed representation of SEXP.
The outer delimiters are stripped."
  (if sexp
      (substring
       (prin1-to-string sexp) 1 -1)
    ""))

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

(defmacro lightlispy-from-left (&rest body)
  "Ensure that BODY is executed from start of list."
  (let ((at-start (cl-gensym "at-start")))
    `(let ((,at-start (lightlispy--leftp)))
       (unless ,at-start
         (lightlispy-different))
       (unwind-protect
            (lispy-save-excursion
              ,@body)
         (unless (eq ,at-start (lightlispy--leftp))
           (lightlispy-different))))))

(defmacro lightlispy-flet (binding &rest body)
  "Temporarily override BINDING and execute BODY."
  (declare (indent 1))
  (let* ((name (car binding))
         (old (cl-gensym (symbol-name name))))
    `(let ((,old (symbol-function ',name)))
       (unwind-protect
            (progn
              (fset ',name (lambda ,@(cdr binding)))
              ,@body)
         (fset ',name ,old)))))

(defun lightlispy-bolp ()
  "Return t if point is at beginning of line, after optional spaces."
  (save-excursion
    (skip-chars-backward " \t")
    (bolp)))

(defun lightlispy-map-delete-overlay ()
  "Delete `lightlispy-map-input-overlay'."
  (when (overlayp lightlispy-map-input-overlay)
    (delete-overlay lightlispy-map-input-overlay)))

(defun lightlispy--symbolp (str)
  "Return t if STR is a symbol."
  (string-match "\\`\\(?:\\sw\\|\\s_\\)+\\'" str))

(defun lightlispy-map-done ()
  (interactive)
  (lightlispy-map-delete-overlay)
  (setq lightlispy-bind-var-in-progress nil)
  (lightlispy-backward 1))

(defun lightlispy--mark (bnd)
  "Mark BND.  BND is a cons of beginning and end positions."
  (setq deactivate-mark nil)
  (set-mark (car bnd))
  (goto-char (cdr bnd)))

(defun lightlispy--string-markedp ()
  "Return t if the current active region is a string."
  (and (region-active-p)
       (eq ?\" (char-after (region-beginning)))
       (eq ?\" (char-before (region-end)))))

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

(defun lightlispy--skip-elisp-char ()
  (unless (lightlispy-after-string-p "?")
    (error "unexpected"))
  (if (looking-at "\\\\")
      (forward-sexp 1)
    (forward-char 1)))

(defun lightlispy--replace-regexp-in-code (regexp to-string)
  "Replace text matching REGEXP with TO-STRING in whole buffer.
Ignore the matches in strings and comments."
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (unless (lightlispy--in-string-or-comment-p)
      (replace-match to-string))))

(defun lightlispy--quote-string (str &optional quote-newlines)
  "Quote the quotes and backslashes in STR.
Quote the newlines if QUOTE-NEWLINES is t."
  (setq str (replace-regexp-in-string "\\\\" "\\\\\\\\" str))
  (setq str (replace-regexp-in-string "\"" "\\\\\"" str))
  (if quote-newlines
      (replace-regexp-in-string "\n" "\\\\n" str)
    str))

(defun lightlispy--delete-whitespace-backward ()
  "Delete spaces backward."
  (let ((pt (point)))
    (skip-chars-backward " ")
    (delete-region (point) pt)))

(defun lightlispy-open-square (arg)
  "Forward to (`lightlispy-brackets' ARG).
Insert \"[\" in strings and comments."
  (interactive "P")
  (if (lightlispy--in-string-or-comment-p)
      (insert "[")
    (lightlispy-brackets arg)))

(defun lightlispy--raw-comment-p (expr)
  "Return t if EXPR is a raw comment."
  (and (listp expr)
       (eq (car expr) 'ly-raw)
       (consp (cdr expr))
       (eq (cadr expr) 'comment)))

(defun lightlispy--in-string-or-comment-p ()
  "Test if point is inside a string or a comment."
  (let* ((sp (syntax-ppss))
         (beg (nth 8 sp)))
    (when (or (eq (char-after beg) ?\")
              (nth 4 sp))
      beg)))

(defun lightlispy--leftp ()
  "Return t if at region beginning, or at start of the list."
  (if (region-active-p)
      (= (point) (region-beginning))
    (or (lightlispy-left-p)
        (looking-at lightlispy-outline))))

(defun lightlispy-after-string-p (str)
  "Return t if the string before point is STR."
  (string=
   (buffer-substring
    (max
     (- (point) (length str))
     (point-min))
    (point))
   str))

(defun lightlispy--in-string-p ()
  "Test if point is inside a string.
Return start of string it is."
  (let ((syn (syntax-ppss)))
    (or (and (nth 3 syn)
             (nth 8 syn))
        (and (eq (char-after) ?\")
             (not (eq ?\\ (char-before)))
             (point)))))

(defun lightlispy--delimiter-space-unless (preceding-syntax-alist)
  "Like `lightlispy--space-unless' but use PRECEDING-SYNTAX-ALIST for decision.
PRECEDING-SYNTAX-ALIST should be an alist of `major-mode' to a list of regexps.
When `looking-back' at any of these regexps, whitespace, or a delimiter, do not
insert a space."
  (lightlispy--space-unless
   (concat "^\\|\\s-\\|" lightlispy-left
           (lightlispy--preceding-syntax preceding-syntax-alist "\\|"))))

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

(defun lightlispy--delete-leading-garbage ()
  "Delete any syntax before an opening delimiter such as '.
Delete backwards to the closest whitespace char or opening delimiter or to the
beginning of the line."
  (let ((pt (point)))
    (re-search-backward (concat "[[:space:]]" "\\|"
                                lightlispy-left "\\|"
                                "^"))
    (goto-char (match-end 0))
    (delete-region (point) pt)))

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

(defun lightlispy--sexp-trim-trailing-newlines (foo comment)
  "Trim trailing (ly-raw newline) from FOO.
Treat comments differently when COMMENT is t."
  (if (and (consp foo) (consp (cdr foo)))
      (let ((expr (reverse foo)))
        (while (and (consp expr)
                    (listp expr)
                    (equal (car expr) '(ly-raw newline))
                    (not (and comment
                              (lightlispy--raw-comment-p (cadr expr)))))
          (setq expr (cdr expr)))
        (reverse expr))
    foo))

(defun lightlispy--prin1-to-string (expr offset mode)
  "Return the string representation of EXPR.
EXPR is indented first, with OFFSET being the column position of
the first character of EXPR.
MODE is the major mode for indenting EXPR."
  (let ((lif lisp-indent-function))
    (with-temp-buffer
      (funcall mode)
      (dotimes (_i offset)
        (insert ?\ ))
      (let ((lisp-indent-function lif))
        (lightlispy--insert expr))
      (buffer-substring-no-properties
       (+ (point-min) offset)
       (point-max)))))

(defun lightlispy--sexp-normalize (foo)
  "Return a pretty version of FOO.
Only `ly-raw' lists within FOO are manipulated."
  (cond ((null foo)
         nil)

        ((consp foo)
         (cons (lightlispy--sexp-normalize
                (lightlispy--sexp-trim-trailing-newlines (car foo) t))
               (lightlispy--sexp-normalize
                (lispy--sexp-trim-trailing-newlines (cdr foo) t))))
        (t
         foo)))

(defun lispy--out-backward (arg)
  "Move outside list forwards ARG times.
Return nil on failure, t otherwise."
  (let ((oldpt (point))
        newpt)
    (lightlispy--out-forward arg)
    (when (lightlispy-right-p)
      (forward-list -1))
    (if (= oldpt (setq newpt (point)))
        nil
      newpt)))

(defun lightlispy--space-unless (context)
  "Insert one space.
Unless inside string or comment, or `looking-back' at CONTEXT."
  (let ((inhibit-field-text-motion t))
    (unless (or lightlispy-no-space
                (bolp)
                (and (window-minibuffer-p)
                     (eq (point) (minibuffer-prompt-end)))
                (lightlispy--in-string-or-comment-p)
                (lightlispy-looking-back context))
      (insert " "))))

(defun lightlispy--indent-for-tab ()
  "Call `indent-for-tab-command'."
  (unless (or (memq major-mode lightlispy-no-indent-modes)
              (apply #'derived-mode-p lightlispy-no-indent-modes)
              (= 0 (buffer-size)))
    (let ((tab-always-indent t))
      (lightlispy-flet (message (&rest _x))
                       (indent-for-tab-command)))))

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

(defun lightlispy--preceding-syntax (preceding-syntax-alist &optional before after)
  "Return a regexp corresponding to valid syntax that can precede delimiters.
This is done by checking PRECEDING-SYNTAX-ALIST for the current major mode.
Return nil if there is no entry for the current major mode. When there is an
entry, prepend BEFORE and append AFTER to the regexp when they are specified."
  (let ((regexps (or (cdr (assoc major-mode preceding-syntax-alist))
                     (cdr (assoc t preceding-syntax-alist)))))
    (when regexps
      (concat before
              "\\(?:"
              (apply #'concat
                     (lightlispy-interleave
                      "\\|"
                      regexps))
              "\\)"
              after))))

(defun lightlispy--delete-pair-in-string (left right)
  "Delete a pair of LEFT and RIGHT in string."
  (let ((bnd (lightlispy--bounds-string)))
    (when bnd
      (let ((pos (cond ((looking-at left)
                        (save-excursion
                          (let ((b1 (match-beginning 0))
                                (e1 (match-end 0))
                                b2 e2)
                            (when (re-search-forward right (cdr bnd) t)
                              (setq b2 (match-beginning 0)
                                    e2 (match-end 0))
                              (delete-region b2 e2)
                              (delete-region b1 e1)
                              b1))))
                       ((looking-at right)
                        (save-excursion
                          (let ((b1 (match-beginning 0))
                                (e1 (match-end 0))
                                b2 e2)
                            (when (re-search-backward left (car bnd) t)
                              (setq b2 (match-beginning 0)
                                    e2 (match-end 0))
                              (delete-region b1 e1)
                              (delete-region b2 e2)
                              (+ (point) (- b1 e2)))))))))
        (when pos
          (goto-char pos))))))

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

(defun lightlispy--surround-region (alpha omega)
  "Surround active region with ALPHA and OMEGA and re-indent."
  (let ((beg (region-beginning))
        (end (region-end)))
    (goto-char end)
    (insert omega)
    (goto-char beg)
    (insert alpha)
    (deactivate-mark)
    (indent-region beg (+ 2 end))))

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

(defun lightlispy--in-empty-list-p (preceding-syntax-alist)
  "Test whether the point is in a list with no sexps.
A list with only characters that can precede a delimiter (e.g. \"`(,)\") is
consider an empty list."
  (and (lightlispy-looking-back
        (concat lightlispy-left
                "[[:space:]]*"
                (lightlispy--preceding-syntax preceding-syntax-alist nil "*")))
       (looking-at (concat "[[:space:]]*" lightlispy-right))))

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

(defun lightlispy--bounds-list ()
  "Return the bounds of smallest list that includes the point."
  (save-excursion
    (lightlispy--exit-string)
    (when (looking-at lispy-left)
      (forward-char))
    (when (lightlispy-looking-back lightlispy-right)
      (backward-char))
    (ignore-errors
      (let (beg end)
        (up-list)
        (setq end (point))
        (backward-list)
        (setq beg (point))
        (cons beg end)))))

(defun lightlispy--maybe-safe-delete-region (beg end)
  "Delete the region from BEG to END.
If `lispy-safe-delete' is non-nil, exclude unmatched delimiters."
  (if lightlispy-safe-delete
      (let ((safe-regions (lightlispy--find-safe-regions beg end)))
        (dolist (safe-region safe-regions)
          (delete-region (car safe-region) (cdr safe-region))))
    (delete-region beg end)))

(defun lightlispy--find-safe-regions (beg end)
  "Return a list of regions between BEG and END that are safe to delete.
The regions are returned in reverse order so that they can be easily deleted
without recalculation."
  (let ((unmatched-delimiters (lightlispy--find-unmatched-delimiters beg end))
        (maybe-safe-pos beg)
        safe-regions)
    (dolist (unsafe-pos unmatched-delimiters)
      (unless (= maybe-safe-pos unsafe-pos)
        (setq safe-regions
              (nconc (lightlispy--maybe-split-safe-region maybe-safe-pos unsafe-pos
                                                     t)
                     safe-regions)))
      (setq maybe-safe-pos (1+ unsafe-pos)))
    (setq safe-regions
          (nconc (lightlispy--maybe-split-safe-region maybe-safe-pos end)
                 safe-regions))))

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

(defun lightlispy-kill-word (arg)
  "Kill ARG words, keeping parens consistent."
  (interactive "p")
  (if (< arg 0)
      (lightlispy-backward-kill-word (- arg))
    (let (bnd)
      (lightlispy-dotimes arg
        (while (not (or (eobp)
                        (memq (char-syntax (char-after))
                              '(?w ?_))))
          (forward-char 1))
        (unless (lightlispy-bolp)
          (delete-horizontal-space))
        (if (setq bnd (lightlispy--bounds-string))
            (save-restriction
              (narrow-to-region (1+ (car bnd)) (1- (cdr bnd)))
              (kill-word 1)
              (widen))
          (kill-word 1))))))

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

(defun lightlispy--not-at-sexp-p (preceding-syntax-alist)
  "Test whether the point is at a \"free\" spot and not at a wrappable sexp.
PRECEDING-SYNTAX-ALIST should be an alist of `major-mode' to a list of regexps.
The regexps correspond to valid syntax that can precede an opening delimiter in
each major mode."
  (let* ((space "[[:space:]]")
         (space-or-eol (concat "\\(" space "+\\|" space "*$\\)"))
         (right-or-eol (concat "\\(" lightlispy-right "+\\|" space "*$\\)"))
         (special-syntax (lightlispy--preceding-syntax preceding-syntax-alist))
         (line (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position))))
    (or (lightlispy--in-empty-list-p preceding-syntax-alist)
        ;; empty line
        (string-match (concat "^" space "*" special-syntax "*" space "*$")
                      line)
        ;; empty position at end of list or line
        (and (looking-at right-or-eol)
             (lightlispy-looking-back (concat space "+" special-syntax "*")))
        ;; empty position at beginning of list
        (and (looking-at space-or-eol)
             (lightlispy-looking-back (concat lightlispy-left special-syntax "*")))
        ;; empty position in middle
        (and (looking-at (concat space "+"))
             (lightlispy-looking-back (concat space "+" special-syntax "*"))))))

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

(defun lightlispy--normalize-1 ()
  "Normalize/prettify current sexp."
  (when (and (looking-at "(")
             (= (point)
                (save-excursion
                  (lightlispy--out-backward 99)
                  (point))))
    (let ((pt (point)))
      (skip-chars-backward " \t")
      (delete-region pt (point))))
  (let* ((bnd (lightlispy--bounds-dwim))
         (str (lightlispy--string-dwim bnd))
         (offset (save-excursion
                   (goto-char (car bnd))
                   (current-column)))
         (was-left (lightlispy-left-p)))
    (cond ((or (and (memq major-mode lightlispy-clojure-modes)
                    (or (string-match "\\^" str)
                        (string-match "~" str)))
               (> (length str) 10000))
           (lightlispy-from-left
            (indent-sexp)))
          ((looking-at ";;"))
          (t
           (let* ((max-lisp-eval-depth 10000)
                  (max-specpdl-size 10000)
                  (geiser-active-implementations
                   (and (bound-and-true-p geiser-active-implementations)
                        (list (car geiser-active-implementations))))
                  (res (lightlispy--sexp-normalize
                        (lightlispy--read str)))
                  (new-str (lightlispy--prin1-to-string res offset major-mode)))
             (unless (string= str new-str)
               (delete-region (car bnd)
                              (cdr bnd))
               (insert new-str)
               (when was-left
                 (backward-list))))))
    (when (and (memq major-mode lightlispy-clojure-modes)
               clojure-align-forms-automatically)
      (clojure-align (car bnd) (cdr bnd)))))

(defun lightlispy-backward-kill-word (arg)
  "Kill ARG words backward, keeping parens consistent."
  (interactive "p")
  (let (bnd
        (pt (point))
        (last-command (if (eq last-command 'lightlispy-backward-kill-word)
                          'kill-region
                        last-command)))
    (lightlispy-dotimes arg
      (when (lightlispy--in-comment-p)
        (skip-chars-backward " \n"))
      (if (memq (char-syntax (char-before))
                '(?w ?_ 32))
          (if (lightlispy-looking-back "\\_<\\s_+")
              (delete-region (match-beginning 0)
                             (match-end 0))
            (backward-kill-word 1)
            (when (and (lightlispy--in-string-p)
                       (not (lightlispy-looking-back "\\\\\\\\"))
                       (lightlispy-looking-back "\\\\"))
              (delete-char -1)))
        (delete-region (point) pt)
        (while (not (or (bobp)
                        (memq (char-syntax (char-before))
                              '(?w ?_))))
          (backward-char 1))
        (if (setq bnd (lightlispy--bounds-string))
            (progn
              (save-restriction
                (if (and (looking-at "\\s-+\"")
                         (eq (match-end 0) (cdr bnd)))
                    (goto-char (1- (cdr bnd)))
                  (when (and (> pt (car bnd))
                             (< pt (cdr bnd)))
                    (goto-char pt)))
                (narrow-to-region (1+ (car bnd)) (point))
                (kill-region (progn
                               (forward-word -1)
                               (when (and (not (lightlispy-looking-back "\\\\\\\\"))
                                          (lightlispy-looking-back "\\\\"))
                                 (backward-char))
                               (point))
                             (point-max))
                (widen)))
          (backward-kill-word 1))))))

(defun lightlispy-mark-symbol ()
  "Mark current symbol."
  (interactive)
  (let (bnd)
    (cond (lightlispy-bind-var-in-progress
           (lightlispy-map-done)
           (setq lightlispy-bind-var-in-progress nil)
           (forward-sexp 2)
           (lightlispy-mark-symbol))

          ((lightlispy--in-comment-p)
           (if (and (looking-at "\\(?:\\w\\|\\s_\\)*'")
                    (setq bnd (match-end 0))
                    (looking-back "`\\(?:\\w\\|\\s_\\)*"
                                  (line-beginning-position)))
               (progn
                 (goto-char (match-beginning 0))
                 (set-mark (point))
                 (goto-char bnd))
             (lightlispy--mark (lightlispy--bounds-comment))))

          ((and
            (not (region-active-p))
            (setq bnd (lightlispy--bounds-string))
            (= (1+ (point))
               (cdr bnd)))
           (lightlispy--mark bnd))

          ((and (lightlispy-after-string-p "\"")
                (not (lightlispy--in-string-or-comment-p)))
           (set-mark-command nil)
           (forward-sexp -1)
           (exchange-point-and-mark))

          ((looking-at " *[[({]")
           (if (and (lightlispy-looking-back "\\sw\\|\\s_")
                    (not (region-active-p)))
               (progn
                 (set-mark-command nil)
                 (forward-sexp -1)
                 (exchange-point-and-mark))
             (let ((pt (point)))
               (skip-chars-forward "(){}[] \"\n")
               (set-mark-command nil)
               (if (looking-at "\\sw\\|\\s_")
                   (forward-sexp)
                 (condition-case nil
                     (progn
                       (re-search-forward "[][(){} \n]")
                       (while (lightlispy--in-string-or-comment-p)
                         (re-search-forward "[() \n]"))
                       (backward-char 1))
                   (error
                    (message "No further symbols found")
                    (deactivate-mark)
                    (goto-char pt)))))))

          ((region-active-p)
           (let ((bnd (lightlispy--bounds-string)))
             (condition-case nil
                 (progn
                   (forward-sexp)
                   (when (and bnd (> (point) (cdr bnd)))
                     (goto-char (cdr bnd))
                     (error "`forward-sexp' went through string bounds")))
               (error
                (deactivate-mark)
                (re-search-forward "\\sw\\|\\s_")
                (forward-char -1)
                (set-mark-command nil)
                (forward-sexp)))))

          ((lightlispy-right-p)
           (skip-chars-backward "}]) \n")
           (set-mark-command nil)
           (re-search-backward "[][{}() \n]")
           (while (lightlispy--in-string-or-comment-p)
             (re-search-backward "[() \n]"))
           (forward-char 1))

          ((looking-at lightlispy-right)
           (lightlispy--mark
            (save-excursion
              (backward-char 1)
              (lightlispy--bounds-dwim))))

          (t
           (lightlispy--mark (lightlispy--bounds-dwim))))))

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

(defun lightlispy-unstringify ()
  "Unquote string at point."
  (interactive)
  (if (region-active-p)
      (if (lightlispy--string-markedp)
          (let (deactivate-mark
                (str (lightlispy--string-dwim))
                (leftp (lightlispy--leftp)))
            (delete-active-region)
            (set-mark (point))
            (insert (read str))
            (when leftp
              (lightlispy-different)))
        (lightlispy-complain "the current region isn't a string"))
    (let* ((bnd (lightlispy--bounds-string))
           (str (lightlispy--string-dwim bnd))
           (str-1 (concat (substring str 0 (- (point) (car bnd))) "\""))
           (offset (length (read str-1))))
      (delete-region (car bnd) (cdr bnd))
      (save-excursion (insert (read str)))
      (forward-char offset))))

(defun lightlispy-quotes (arg)
  "Insert a pair of quotes around the point.

When the region is active, wrap it in quotes instead.
When inside string, if ARG is nil quotes are quoted,
otherwise the whole string is unquoted."
  (interactive "P")
  (let (bnd)
    (cond ((region-active-p)
           (if arg
               (lightlispy-unstringify)
             (lightlispy-stringify)))
          ((and (setq bnd (lightlispy--bounds-string))
                (not (= (point) (car bnd))))
           (if arg
               (lightlispy-unstringify)
             (if (and lightlispy-close-quotes-at-end-p (looking-at "\""))
                 (forward-char 1)
               (progn (insert "\\\"\\\""))
               (backward-char 2))))

          (arg
           (lightlispy-stringify))

          ((lightlispy-after-string-p "?\\")
           (self-insert-command 1))

          (t
           (lightlispy--space-unless "^\\|\\s-\\|\\s(\\|[#]")
           (insert "\"\"")
           (unless (looking-at "\n\\|)\\|}\\|\\]\\|$")
             (just-one-space)
             (backward-char 1))
           (backward-char)))))

(defun lightlispy-stringify (&optional arg)
  "Transform current sexp into a string.
Quote newlines if ARG isn't 1."
  (interactive "p")
  (setq arg (or arg 1))
  (let* ((bnd (lightlispy--bounds-dwim))
         (pt (point))
         (str-1 (buffer-substring-no-properties (car bnd) pt))
         (str-2 (buffer-substring-no-properties pt (cdr bnd)))
         (regionp (region-active-p))
         (leftp (lightlispy--leftp))
         deactivate-mark)
    (when (and regionp leftp)
      (exchange-point-and-mark))
    (if (lightlispy--in-string-p)
        (if regionp
            (progn
              (insert "\\\"")
              (exchange-point-and-mark)
              (insert "\\\"")
              (backward-char 2)
              (unless leftp
                (exchange-point-and-mark)))
          (lightlispy-complain "can't do anything useful here"))
      (deactivate-mark)
      (setq str-1 (lightlispy--quote-string str-1 (/= arg 1)))
      (setq str-2 (lightlispy--quote-string str-2 (/= arg 1)))
      (delete-region (car bnd) (cdr bnd))
      (insert "\"" str-1)
      (save-excursion (insert str-2 "\""))
      (when regionp
        (unless (looking-at "\"")
          (backward-char 1))
        (lightlispy-mark-symbol)
        (if (and leftp (= (point) (region-end)))
            (exchange-point-and-mark))))))

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

(defun lightlispy--maybe-split-safe-region (beg end &optional end-unsafe-p)
  "Return a list of regions between BEG and END that are safe to delete.
It is expected that there are no unmatched delimiters in between BEG and END.
Split the region if deleting it would pull unmatched delimiters into a comment.
Specifically, split the region if all of the following are true:

- `lightlispy-safe-actions-no-pull-delimiters-into-comments' is non-nil
- BEG is inside a comment
- END is not in a comment
- Either there are unmatched delimiters on the line after END or END-UNSAFE-P is
  non-nil

Otherwise, just return a list with the initial region. The regions are returned
in reverse order so that they can be easily deleted without recalculation."
  (if (and lightlispy-safe-actions-no-pull-delimiters-into-comments
           ;; check that BEG is inside a comment
           ;; `lightlispy--in-comment-p' returns t at comment start which is
           ;; unwanted here
           (and (save-excursion
                  (nth 4 (syntax-ppss beg))))
           (save-excursion
             (goto-char end)
             ;; check that END is not inside or a comment and that the
             ;; following line has unmatched delimiters or has been specified
             ;; as unsafe to pull into a comment
             (and (not (lightlispy--in-comment-p))
                  (or end-unsafe-p
                      (lightlispy--find-unmatched-delimiters
                       end
                       (line-end-position))))))
      ;; exclude newline; don't pull END into a comment
      (let ((comment-end-pos (save-excursion
                               (goto-char beg)
                               (cdr (lightlispy--bounds-comment)))))
        (list (cons (1+ comment-end-pos) end)
              (cons beg comment-end-pos)))
    (list (cons beg end))))

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

(defun lightlispy-raise (arg)
  "Use current sexp or region as replacement for its parent.
Do so ARG times."
  (interactive "p")
  (lightlispy-dotimes arg
    (let ((regionp (region-active-p))
          (leftp (lightlispy--leftp))
          (deactivate-mark nil)
          bnd1 bnd2)
      ;; re-indent first
      (lightlispy-save-excursion (lightlispy--out-forward 1))
      (unless leftp
        (lightlispy-different))
      (setq bnd1 (lightlispy--bounds-dwim))
      (deactivate-mark)
      (lightlispy--out-forward 1)
      (setq bnd2 (lightlispy--bounds-dwim))
      (delete-region (cdr bnd2) (cdr bnd1))
      (delete-region (car bnd2) (car bnd1))
      (if regionp
          (progn
            (indent-region (car bnd2) (point))
            (lightlispy--mark (cons (car bnd2) (point))))
        (lightlispy-from-left
         (indent-sexp)))
      (unless (eq leftp (lightlispy--leftp))
        (lightlispy-different)))))

(defun lightlispy-raise-some ()
  "Use current sexps as replacement for their parent.
The outcome when ahead of sexps is different from when behind."
  (interactive)
  (let ((pt (point)))
    (cond ((region-active-p))

          ((lightlispy-left-p)
           (if (null (lightlispy--out-forward 1))
               (progn
                 (goto-char pt)
                 (lightlispy-complain "Not enough depth to raise"))
             (backward-char 1)
             (set-mark (point))
             (goto-char pt)))

          ((lightlispy-right-p)
           (if (null (lightlispy--out-forward 1))
               (progn
                 (goto-char pt)
                 (lightlispy-complain "Not enough depth to raise"))
             (backward-list)
             (forward-char 1)
             (set-mark (point))
             (goto-char pt)))

          (t
           (error "Unexpected")))
    (lightlispy-raise 1)
    (deactivate-mark)))

(defun lightlispy-pair (left right preceding-syntax-alist)
  "Return (lambda (arg)(interactive \"P\")...) using LEFT RIGHT.
PRECEDING-SYNTAX-ALIST should be an alist of `major-mode' to a list of regexps.
The regexps correspond to valid syntax that can precede LEFT in each major mode.
When this function is called:
- with region active:
  Wrap region with LEFT RIGHT.
- with region active and arg 1:
  Wrap region with LEFT RIGHT and put the point after LEFT followed by a space.
- with arg nil:
  Insert LEFT RIGHT.
- with arg negative:
  Wrap as many sexps as possible until the end of the line with LEFT RIGHT.
- with arg 0:
  Wrap as many sexps as possible with LEFT RIGHT.
- with the universal arg:
  Wrap one sexp with LEFT RIGHT.
- with arg positive:
  Wrap that number of sexps with LEFT RIGHT or as many as possible."
  `(lambda (arg)
     (interactive "P")
     (cond ((not arg))
           ((listp arg)
            (setq arg 1))
           (t
            (setq arg (prefix-numeric-value arg))))
     (cond ((region-active-p)
            (lightlispy--surround-region ,left ,right)
            (when (and (lightlispy-looking-back lightlispy-left)
                       (or (lightlispy-left-p)
                           (> (or arg 0) 0)))
              (insert " "))
            (backward-char 1))
           ((and (lightlispy--in-string-p)
                 (lightlispy-looking-back "\\\\\\\\"))
            (insert ,left "\\\\" ,right)
            (backward-char 3))
           ((lightlispy--in-string-or-comment-p)
            (if (and lightlispy-parens-only-left-in-string-or-comment
                     (string= ,left "(")
                     (= ?\( (aref (this-command-keys-vector) 0)))
                (insert "(")
              (insert ,left ,right)
              (backward-char 1)))
           ((lightlispy-after-string-p "?\\")
            (insert ,left))
           ((not arg)
            (lightlispy--indent-for-tab)
            (lightlispy--delimiter-space-unless ,preceding-syntax-alist)
            (insert ,left ,right)
            (unless (or (eolp)
                        (lightlispy--in-string-p)
                        (looking-at "\n\\|)\\|}\\|\\]"))
              (just-one-space)
              (backward-char 1))
            (when (looking-at ,(regexp-quote left))
              (insert " ")
              (backward-char))
            (backward-char))
           (t
            ;; don't jump backwards or out of a list when not at a sexp
            (unless (lightlispy--not-at-sexp-p ,preceding-syntax-alist)
              (when (lightlispy--bounds-dwim)
                (goto-char (car (lightlispy--bounds-dwim)))))
            (lightlispy--indent-for-tab)
            (insert ,left ,right)
            (save-excursion
              (lightlispy-slurp arg))
            (when (or (looking-at lightlispy-right)
                      (and (eolp)
                           (looking-back lightlispy-right (1- (point)))))
              ;; failed to wrap anything
              (backward-char))
            (when (and lightlispy-insert-space-after-wrap
                       (not (lightlispy--in-empty-list-p ,preceding-syntax-alist))
                       (not (eolp)))
              (just-one-space)
              (backward-char))))))

(defalias 'lightlispy-braces
    (lightlispy-pair "{" "}" 'lightlispy-braces-preceding-syntax-alist)
    "`lightlispy-pair' with {}.")

(defalias 'lightlispy-parens
  (lightlispy-pair "(" ")" 'lightlispy-parens-preceding-syntax-alist)
  "`lightlispy-pair' with ().")

(defun lightlispy-kill ()
  "Kill line, keeping parens consistent."
  (interactive)
  (let (bnd)
    (cond ((or (lightlispy--in-comment-p)
               (and (looking-at " *;")
                    (save-excursion
                      (goto-char (match-end 0))
                      (lightlispy--in-comment-p))))
           (kill-line))

          ((and (setq bnd (lightlispy--bounds-string))
                (or
                 (not (eq (point) (car bnd)))
                 (> (count-lines (car bnd) (cdr bnd)) 1)))
           (if (> (cdr bnd) (line-end-position))
               (if (eq (point) (car bnd))
                   (kill-region (car bnd) (cdr bnd))
                 (kill-line))
             (kill-region (point) (1- (cdr bnd)))))
          ((looking-at " *\n")
           (kill-region
            (match-beginning 0)
            (match-end 0))
           (lightlispy--indent-for-tab))
          ((and (looking-at lightlispy-right) (looking-back lightlispy-left
                                                            (line-beginning-position)))
           (delete-char 1)
           (backward-delete-char 1))
          ((lightlispy-left-p)
           (if (progn
                 (setq bnd (lightlispy--bounds-list))
                 (> (count-lines (car bnd) (cdr bnd)) 1))
               (kill-region (car bnd)
                            (cdr bnd))
             (narrow-to-region (car bnd) (line-end-position))
             (let ((pt (point)))
               (while (and (ignore-errors
                             (forward-list))
                           (> (point) pt))
                 (setq pt (point)))
               (when (looking-at "[\t ]*;[^\n]*$")
                 (setq pt (match-end 0)))
               (goto-char (point-min))
               (widen)
               (kill-region (point) pt))))
          (t
           (let ((beg (point))
                 (end (line-end-position))
                 bnd)
             (while (and (< (point) end)
                         (ignore-errors
                           (forward-sexp 1)
                           (skip-chars-forward " ")
                           t))
               (when (setq bnd (lightlispy--bounds-comment))
                 (goto-char (cdr bnd))))
             (skip-chars-forward " \t")
             (kill-region beg (point)))))))

(defun lightlispy--find-unmatched-delimiters (beg end)
  "Return the positions of unmatched delimiters between BEG and END.
When the region is a greater size than `lightlispy-safe-threshold', it will not be
checked and nil will be returned."
  (if (> (- end beg) lightlispy-safe-threshold)
      nil
    (save-excursion
      (goto-char beg)
      (let ((lightlispy-delimiters (concat (substring lightlispy-right 0 -1)
                                      "\""
                                      (substring lightlispy-left 1)))
            matched-left-quote-p
            string-bounds
            string-end
            comment-end
            left-positions
            right-positions)
        (while (re-search-forward lightlispy-delimiters end t)
          (let* ((match-beginning (match-beginning 0))
                 (matched-delimiter (buffer-substring-no-properties
                                     match-beginning
                                     (match-end 0))))
            (cond
              ((and lightlispy-safe-actions-ignore-strings
                    (save-excursion
                      (goto-char match-beginning)
                      (setq string-bounds (lightlispy--bounds-string))
                      (setq string-end (cdr string-bounds))))
               (setq matched-left-quote-p (= (1- (point))
                                             (car string-bounds)))
               (cond ((< (1- string-end) end)
                      (goto-char string-end)
                      ;; when skipping strings, will only match right quote
                      ;; if left quote is not in the region
                      (when (not matched-left-quote-p)
                        (push (1- string-end) right-positions)))
                     (t
                      (when matched-left-quote-p
                        ;; unmatched left quote
                        (push match-beginning left-positions))
                      (goto-char end))))
              ((and lightlispy-safe-actions-ignore-comments
                    (save-excursion
                      (goto-char match-beginning)
                      (setq comment-end (cdr (lightlispy--bounds-comment)))))
               (if (< comment-end end)
                   (goto-char comment-end)
                 (goto-char end)))
              (t
               (unless (looking-back "\\\\." (- (point) 2))
                 (if (or (string-match lightlispy-left matched-delimiter)
                         (and (string= matched-delimiter "\"")
                              (lightlispy--in-string-p)))
                     (push match-beginning left-positions)
                   (if (> (length left-positions) 0)
                       (pop left-positions)
                     (push match-beginning right-positions))))))))
        (nreverse (append left-positions right-positions))))))

(defalias 'lightlispy-brackets
  (lightlispy-pair "[" "]" 'lightlispy-brackets-preceding-syntax-alist)
  "`lightlispy-pair' with [].")

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

(defun lightlispy-join ()
  "Join sexps."
  (interactive)
  (let ((pt (point))
        bnd)
    (cond ((lightlispy-right-p)
           (when (lightlispy-forward 1)
             (backward-list)
             (delete-char 1)
             (goto-char pt)
             (backward-delete-char 1)
             (lightlispy--out-forward 1)
             (lightlispy--reindent 1)))
          ((lightlispy-left-p)
           (when (lightlispy-backward 1)
             (forward-list)
             (backward-delete-char 1)
             (goto-char (1- pt))
             (delete-char 1)
             (lightlispy-save-excursion
               (forward-char 1)
               (lightlispy-left 2)
               (lightlispy--normalize-1))))
          ((and (setq bnd (lightlispy--bounds-string))
                (or (save-excursion
                      (goto-char (car bnd))
                      (skip-chars-backward " \t\n")
                      (when (eq (char-before) ?\")
                        (delete-region (1- (point))
                                       (1+ (car bnd)))
                        t))
                    (save-excursion
                      (goto-char (cdr bnd))
                      (skip-chars-forward " \t\n")
                      (when (looking-at "\"")
                        (delete-region (1- (cdr bnd))
                                       (1+ (point)))
                        t))))))))

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

(defun lightlispy-splice (arg)
  "Splice ARG sexps into containing list."
  (interactive "p")
  (lightlispy-dotimes arg
    (let ((bnd (lispy--bounds-dwim))
          (deactivate-mark nil))
      (cond ((region-active-p)
             (save-excursion
               (goto-char (cdr bnd))
               (re-search-backward lispy-right)
               (delete-region (point) (cdr bnd)))
             (save-excursion
               (goto-char (car bnd))
               (re-search-forward lispy-left)
               (delete-region (car bnd) (point))))
            ((lispy-splice-let))

            ((lispy-left-p)
             (save-excursion
               (goto-char (cdr bnd))
               (delete-char -1))
             (lispy--delete-leading-garbage)
             (delete-char 1)
             (lispy-forward 1)
             (lispy-backward 1))

            ((lispy-right-p)
             (setq bnd (lispy--bounds-dwim))
             (delete-char -1)
             (goto-char (car bnd))
             (let ((pt (point)))
               (re-search-forward lispy-left nil t)
               (delete-region pt (point)))
             (lispy-backward 1)
             (forward-list))

            (t
             (setq bnd (lispy--bounds-list))
             (save-excursion
               (goto-char (cdr bnd))
               (delete-char -1))
             (save-excursion
               (goto-char (car bnd))
               (delete-char 1)))))))

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

(defun lightlispy-up (arg)
  "Move up ARG times inside current list."
  (interactive "p")
  (lightlispy--remember)
  (cond ((region-active-p)
         (let ((leftp (= (point) (region-beginning))))
           (unless leftp
             (exchange-point-and-mark))
           (cond ((save-excursion
                    (skip-chars-backward "\n ")
                    (bobp)))
                 ((looking-back "^ *\\(;\\)[^\n]*[\n ]*"
                                (save-excursion
                                  (ignore-errors
                                    (backward-sexp 1))
                                  (point)))
                  (deactivate-mark)
                  (goto-char (match-beginning 1))
                  (lightlispy--mark (lightlispy--bounds-comment))
                  (exchange-point-and-mark))
                 ((lightlispy--symbolp (lightlispy--string-dwim))
                  (lightlispy-dotimes arg
                    (when (lightlispy-slurp 1)
                      (lightlispy-different)
                      (lightlispy-barf 1)
                      (lightlispy-different))))
                 (t
                  (lightlispy-dotimes arg
                    (backward-sexp 1)
                    (lightlispy-different)
                    (if (lightlispy--in-comment-p)
                        (progn
                          (goto-char (1- (car (lightlispy--bounds-comment))))
                          (skip-chars-backward "\n"))
                      (backward-sexp 2)
                      (backward-sexp -1))
                    (lightlispy-different))))
           (unless leftp
             (exchange-point-and-mark))))

        ((lightlispy-left-p)
         (let ((pt (point)))
           (unless (lightlispy-backward arg)
             (goto-char pt))))

        ((lightlispy-right-p)
         (lightlispy-backward arg)
         (let ((pt (point)))
           (if (lightlispy-backward 1)
               (lightlispy-forward 1)
             (goto-char pt)
             (lightlispy-different))))

        ((or (looking-at lightlispy-outline)
             (and (bolp) (looking-at ";")))
         (let ((pt (point)))
           (lightlispy-dotimes arg
             (outline-previous-visible-heading 1)
             (if (looking-at lightlispy-outline)
                 (setq pt (point))
               (goto-char pt)
               (error "First outline reached")))))
        (t
         (lightlispy-backward 1)
         (lightlispy-forward 1)))
  (lightlispy--ensure-visible))

(defun lightlispy-delete (arg)
  "Delete ARG sexps."
  (interactive "p")
  (let (bnd)
    (cond ((< arg 0)
           (lightlispy-delete-backward (- arg)))

          ((region-active-p)
           (lightlispy--maybe-safe-delete-region (region-beginning) (region-end)))

          ((setq bnd (lightlispy--bounds-string))
           (cond ((eq (1+ (point)) (cdr bnd))
                  (goto-char (car bnd))
                  (when lightlispy-delete-sexp-from-within
                    (lightlispy-delete arg)))
                 ((looking-at "\\\\\"")
                  (if (eq (+ (point) 2) (cdr bnd))
                      (goto-char (car bnd))
                    (delete-char 2)))
                 ((and (looking-at "\"")
                       (lightlispy-looking-back "\\\\"))
                  (backward-char 1)
                  (delete-char 2))
                 ((lightlispy--delete-pair-in-string "\\\\\\\\(" "\\\\\\\\)"))
                 ((looking-at "\\\\\\\\")
                  (delete-char 2))
                 ((and (looking-at "\\\\")
                       (lightlispy-looking-back "\\\\"))
                  (backward-char 1)
                  (delete-char 2))
                 ((eq (point) (car bnd))
                  (delete-region (car bnd)
                                 (cdr bnd))
                  (let ((pt (point)))
                    (skip-chars-forward " ")
                    (delete-region pt (point))))
                 ((save-excursion
                    (forward-char 1)
                    (lightlispy--in-string-or-comment-p))
                  (delete-char arg))
                 (t
                  (lightlispy--exit-string))))

          ((lightlispy--in-comment-p)
           (if (lightlispy-bolp)
               (let ((bnd (lightlispy--bounds-comment)))
                 (delete-region (car bnd) (cdr bnd)))
             (delete-char arg)))

          ((looking-at lightlispy-right)
           (lightlispy-left 1)
           (when lightlispy-delete-sexp-from-within
             (lightlispy-delete arg)))

          ((lightlispy-left-p)
           (lightlispy--delete-leading-garbage)
           (lightlispy-dotimes arg
             (lightlispy--delete)))

          ((eolp)
           (delete-char 1)
           (let ((pt (point)))
             (skip-chars-forward " ")
             (delete-region pt (point))
             (unless (or (eolp)
                         (bolp)
                         (lightlispy-bolp)
                         (eq (char-before) ?\ ))
               (insert " "))))

          (t
           (delete-char arg)))))

(defun lightlispy-delete-backward (arg)
  "From \")|\", delete ARG sexps backwards.
Otherwise (`backward-delete-char-untabify' ARG)."
  (interactive "p")
  (let (bnd)
    (cond ((< arg 0)
           (lightlispy-delete (- arg)))

          ((use-region-p)
           (lightlispy--maybe-safe-delete-region (region-beginning)
                                            (region-end)))
          ((bobp))

          ((and (setq bnd (lightlispy--bounds-string))
                (not (eq (point) (car bnd))))
           (cond ((eq (- (point) (car bnd)) 1)
                  (goto-char (cdr bnd))
                  (if lightlispy-delete-sexp-from-within
                      (lightlispy-delete-backward arg)))
                 ((or (looking-back "\\\\\\\\(" (car bnd))
                      (looking-back "\\\\\\\\)" (car bnd)))
                  (let ((pt (point)))
                    (goto-char (match-beginning 0))
                    (unless (lightlispy--delete-pair-in-string
                             "\\\\\\\\(" "\\\\\\\\)")
                      (goto-char pt)
                      (backward-delete-char-untabify arg))))
                 ((looking-back "[^\\]\\\\[^\\]" (car bnd))
                  (backward-delete-char 2))
                 (t
                  (backward-delete-char-untabify arg))))

          ((looking-at lightlispy-outline)
           (if (lightlispy-looking-back (concat lightlispy-outline ".*\n"))
               (delete-region
                (match-beginning 0)
                (match-end 0))
             (delete-char -1)))

          ((lightlispy--in-comment-p)
           (cond ((lightlispy-looking-back "^ +")
                  (delete-region (max (1- (match-beginning 0))
                                      (point-min))
                                 (match-end 0))
                  (lightlispy--indent-for-tab))
                 ((and (looking-at "$") (lightlispy-looking-back "; +"))
                  (let ((pt (point)))
                    (skip-chars-backward " ;")
                    (delete-region (point) pt)
                    (if (lightlispy-looking-back "^")
                        (lightlispy--indent-for-tab)
                      (let ((p (point)))
                        (lightlispy--out-forward 1)
                        (lightlispy--normalize-1)
                        (goto-char p)))))
                 (t
                  (backward-delete-char-untabify arg))))

          ((lightlispy-looking-back "\\\\.")
           (backward-delete-char-untabify arg))

          ((and (lightlispy-looking-back (concat lightlispy-right " "))
                (looking-at " *$"))
           (backward-delete-char-untabify arg))

          ((or (and (lightlispy-right-p)
                    (or (memq major-mode lightlispy-clojure-modes)
                        (not (lightlispy-looking-back "[\\?]."))))
               (and (lightlispy-looking-back (concat lightlispy-right " "))
                    (or (lightlispy-left-p) (looking-at "\""))))
           (let ((pt (point)))
             (lightlispy-backward arg)
             (unless (lightlispy-right-p)
               (lightlispy--skip-delimiter-preceding-syntax-backward))
             (skip-chars-backward " \t")
             (while (plist-get (text-properties-at (point)) 'read-only)
               (forward-char))
             (delete-region (point) pt)
             (unless (or (looking-at " ")
                         (lightlispy-bolp)
                         (and (lightlispy-right-p)
                              (not (or (lightlispy-left-p)
                                       (looking-at "\""))))
                         (lightlispy-looking-back lightlispy-left)
                         ;; REPL prompt, e.g. `ielm'
                         (lightlispy-after-string-p "> "))
               (just-one-space))
             (setq pt (point))
             (if (and
                  (not (lightlispy-bolp))
                  (not (lightlispy-left-p))
                  (progn
                    (skip-chars-backward " \t\n")
                    (lightlispy-right-p)))
                 (delete-region (point) pt)
               (goto-char pt)
               (lightlispy--indent-for-tab))))

          ((and (lightlispy-looking-back lightlispy-left)
                (not (lispy-looking-back "[\\?].")))
           (lightlispy--out-forward 1)
           (lightlispy-delete-backward 1))

          ((eq (char-before) ?\")
           (backward-char 1)
           (let ((bnd (lightlispy--bounds-string)))
             (delete-region (car bnd)
                            (cdr bnd))
             (lightlispy--delete-whitespace-backward)
             (unless (looking-at " ")
               (insert " "))
             (lightlispy--indent-for-tab)))

          ((and (lightlispy-after-string-p "\" ")
                (not (looking-at lightlispy-right)))
           (let ((pt (point)))
             (backward-char 2)
             (delete-region (car (lightlispy--bounds-string)) pt))
           (lightlispy--delete-whitespace-backward)
           (unless (lightlispy-looking-back lightlispy-left)
             (just-one-space))
           (lightlispy--indent-for-tab))

          ((lightlispy-bolp)
           (delete-region
            (line-beginning-position)
            (point))
           (unless (bobp)
             (if (and (not (eolp))
                      (save-excursion
                        (backward-char 1)
                        (lightlispy--in-comment-p)))
                 (progn
                   (backward-char 1)
                   (let ((bnd (lightlispy--bounds-comment)))
                     (delete-region (car bnd) (cdr bnd)))
                   (delete-char 1))
               (backward-delete-char 1)
               (unless (or (eolp)
                           (looking-at lightlispy-right)
                           (lightlispy-looking-back lightlispy-left))
                 (just-one-space)))
             (lightlispy--indent-for-tab)))

          ((lightlispy-looking-back "[^ ]  +")
           (delete-region (+ (match-beginning 0) 2) (point)))

          (t
           (backward-delete-char-untabify arg))))
  (when (and (buffer-file-name)
             (< (- (line-number-at-pos (point))
                   (line-number-at-pos (window-start)))
                5)
             lightlispy-delete-backward-recenter)
    (ignore-errors
      (recenter lightlispy-delete-backward-recenter)))
  (when (and (lightlispy-left-p)
             (not (lightlispy--in-string-or-comment-p)))
    (indent-sexp)))

(defun lightlispy-comment (&optional arg)
  "Comment ARG sexps."
  (interactive "p")
  (setq arg (or arg 1))
  (if (and (> arg 1) (lightlispy--in-comment-p))
      (let ((bnd (lightlispy--bounds-comment)))
        (uncomment-region (car bnd) (cdr bnd)))
    (lightlispy-dotimes arg
      (let (bnd)
        (cond ((region-active-p)
               (comment-dwim nil)
               (when (lightlispy--in-string-or-comment-p)
                 (lightlispy--out-backward 1)))
              ((lightlispy--in-string-or-comment-p)
               (cond ((and (eq major-mode 'emacs-lisp-mode)
                           (lightlispy-after-string-p ";;; "))
                      (delete-char -1)
                      (insert "###autoload")
                      (forward-char 1))
                     ((lightlispy-after-string-p ";; ")
                      (backward-char 1)
                      (insert ";")
                      (forward-char 1))
                     ((and lightlispy-comment-use-single-semicolon
                           (lightlispy-after-string-p "; "))
                      (delete-region
                       (point)
                       (progn
                         (skip-chars-backward "; \n")
                         (point)))
                      (insert " ;; "))
                     (t
                      (self-insert-command 1))))
              ((memq (char-before) '(?\\ ?\#))
               (self-insert-command 1))
              ((lightlispy-left-p)
               (setq bnd (lightlispy--bounds-dwim))
               (when lightlispy-move-after-commenting
                 (lightlispy-down 1))
               (comment-region (car bnd) (cdr bnd))
               (when lightlispy-move-after-commenting
                 (when (or (lightlispy--in-string-or-comment-p)
                           (looking-at ";"))
                   (lightlispy--out-backward 1))))
              ((lightlispy-right-p)
               (if lightlispy-comment-use-single-semicolon
                   (progn
                     (unless (eolp)
                       (newline-and-indent)
                       (skip-chars-backward "\n\t "))
                     (comment-dwim nil)
                     (just-one-space))
                 (progn
                   (newline-and-indent)
                   (insert ";; ")
                   (unless (eolp)
                     (newline)
                     (lightlispy--reindent 1)
                     (skip-chars-backward "\n\t ")
                     (forward-char 1)))))
              ((eolp)
               (comment-dwim nil)
               (when lightlispy-comment-use-single-semicolon
                 (just-one-space)))
              ((looking-at " *[])}]")
               (if lightlispy-comment-use-single-semicolon
                   (if (lightlispy-bolp)
                       (insert ";;\n")
                     (insert ";\n"))
                 (progn
                   (unless (lightlispy-bolp)
                     (insert "\n"))
                   (insert ";;\n")))
               (when (lightlispy--out-forward 1)
                 (lightlispy--normalize-1))
               (move-end-of-line 0)
               (insert " "))
              ((lightlispy-bolp)
               (let ((bnd (lightlispy--bounds-list)))
                 (cond ((null bnd)
                        (comment-region (point) (line-end-position)))
                       ((<= (cdr bnd) (line-end-position))
                        (comment-region (point)
                                        (1- (cdr bnd))))
                       (t
                        (let ((beg (point))
                              (ln-start (line-number-at-pos)))
                          (forward-sexp)
                          (while (and (= (line-number-at-pos) ln-start)
                                      (not (eolp)))
                            (forward-sexp))
                          (comment-region beg (point))
                          (goto-char beg))))
                 (skip-chars-forward " ")))
              ((setq bnd (save-excursion
                           (and (lightlispy--out-forward 1)
                                (point))))
               (let ((pt (point)))
                 (if (re-search-forward "\n" bnd t)
                     (if (= (count-matches lightlispy-left pt (point))
                            (count-matches lightlispy-right pt (point)))
                         (progn (comment-region pt (point))
                                (lightlispy-forward 1)
                                (lightlispy-backward 1))
                       (goto-char pt)
                       (re-search-forward lightlispy-left bnd t)
                       (backward-char 1)
                       (forward-list 1)
                       (comment-region pt (point))
                       (lightlispy-forward 1)
                       (lightlispy-backward 1))
                   (comment-region (point) (1- bnd))
                   (lightlispy--out-backward 1))))
              (t
               (self-insert-command 1)))))))

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

(defun lightlispy--read (str)
  "Read STR including comments and newlines."
  (let* ((deactivate-mark nil)
         (mode major-mode)
         cbnd
         (str (with-temp-buffer
                (funcall mode)
                (insert str)
                ;; ——— ly-raw —————————————————
                (lightlispy--replace-regexp-in-code "(ly-raw" "(ly-raw raw")
                ;; ——— comments ———————————————
                (goto-char (point-min))
                (while (comment-search-forward (point-max) t)
                  (lightlispy--beginning-of-comment)
                  (setq cbnd (cons (point) (line-end-position)))
                  (setq str (lightlispy--string-dwim cbnd))
                  (delete-region (car cbnd) (cdr cbnd))
                  (insert (format "(ly-raw comment %S)" str)))
                ;; ——— reader macro syntax (LISP)
                (goto-char (point-min))
                (while (re-search-forward "#[a-z][\"(]" nil t)
                  (forward-char -1)
                  (unless (lightlispy--in-string-or-comment-p)
                    (let ((beg (match-beginning 0))
                          rep)
                      (forward-sexp 1)
                      (setq rep (format "(ly-raw lisp-macro %S)"
                                        (buffer-substring-no-properties
                                         beg (point))))
                      (delete-region beg (point))
                      (insert rep))))
                ;; ——— strings ————————————————
                (goto-char (point-min))
                (while (re-search-forward "\"" nil t)
                  (progn
                    (setq cbnd (lightlispy--bounds-string))
                    (when cbnd
                      (if (or (lightlispy-after-string-p "ly-raw comment \"")
                              (lightlispy-after-string-p "ly-raw lisp-macro \""))
                          (goto-char (cdr cbnd))
                        (setq str (lightlispy--string-dwim cbnd))
                        (delete-region (car cbnd) (cdr cbnd))
                        (insert (format "(ly-raw string %S)" str))))))
                ;; ——— newlines ———————————————
                (lightlispy--replace-regexp-in-code "\n" " (ly-raw newline)")
                ;; ——— numbers ————————————————
                (goto-char (point-min))
                (while (re-search-forward "\\b[+-]?[0-9]+\\(?:\\.[0-9]+\\)?\\(?:e[+-]?[0-9]*\\)" nil t)
                  (if (setq cbnd (lightlispy--bounds-string))
                      (goto-char (cdr cbnd))
                    (let ((s (match-string-no-properties 0)))
                      (delete-region (match-beginning 0) (match-end 0))
                      (insert (format "(ly-raw float \"%s\")" s)))))
                ;; ——— () —————————————————————
                (goto-char (point-min))
                (while (re-search-forward "\\(?:[^\\]\\|^\\)\\(()\\)" nil t)
                  (unless (lightlispy--in-string-or-comment-p)
                    (replace-match "(ly-raw empty)" nil nil nil 1)))
                ;; ——— ? char syntax ——————————
                (goto-char (point-min))
                (while (re-search-forward "\\(?:\\s-\\|\\s(\\)\\?" nil t)
                  (unless (lightlispy--in-string-or-comment-p)
                    (let ((pt (point))
                          sexp)
                      (lightlispy--skip-elisp-char)
                      (setq sexp (buffer-substring-no-properties pt (point)))
                      (delete-region (1- pt) (point))
                      (insert (format "(ly-raw char %S)" sexp)))))
                ;; ——— \ char syntax (Clojure)—
                (goto-char (point-min))
                (while (re-search-forward "\\\\\\(\\sw\\|space\\|tab\\)\\b" nil t)
                  (unless (lightlispy--in-string-or-comment-p)
                    (replace-match (format "(ly-raw clojure-char %S)"
                                           (substring-no-properties
                                            (match-string 0)))
                                   nil t)))
                ;; ——— \ char syntax (LISP)————
                (goto-char (point-min))
                (while (re-search-forward "#\\\\\\(.\\)" nil t)
                  (unless (lightlispy--in-string-or-comment-p)
                    (replace-match (format "(ly-raw lisp-char %S)"
                                           (substring-no-properties
                                            (match-string 0)))
                                   nil t)))
                ;; ——— Clojure gensym —————————
                (goto-char (point-min))
                (while (re-search-forward "\\([a-zA-Z][a-zA-z-/_0-9]*#\\)" nil t)
                  (unless (lightlispy--in-string-or-comment-p)
                    (replace-match (format "(ly-raw clojure-gensym %S)"
                                           (match-string-no-properties 1)))))
                ;; ——— #' —————————————————————
                (goto-char (point-min))
                (while (re-search-forward "#'" nil t)
                  (unless (lightlispy--in-string-or-comment-p)
                    (forward-sexp)
                    (insert ")")
                    (replace-match "(ly-raw function ")))
                ;; ——— ,@ —————————————————————
                (goto-char (point-min))
                (while (re-search-forward "\\(?:[^\\]\\|^\\),@" nil t)
                  (unless (lightlispy--in-string-or-comment-p)
                    (backward-char 2)
                    (let ((beg (point))
                          (sxp (ignore-errors (read (current-buffer)))))
                      (when (and (consp sxp)
                                 (eq (car sxp) '\,@))
                        (insert ")")
                        (goto-char beg)
                        (delete-char 2)
                        (insert "(ly-raw comma-splice ")))))
                ;; ——— #_ —————————————————————
                (goto-char (point-min))
                (while (re-search-forward "#_[({[]" nil t)
                  (backward-char 1)
                  (let ((beg (point)))
                    (forward-list 1)
                    (insert ")")
                    (goto-char beg)
                    (delete-char -2)
                    (insert "(ly-raw clojure-reader-comment ")))
                ;; ——— #{ or { or #( or @( or #?( or #?@( ——————————
                (goto-char (point-min))
                (while (re-search-forward "#object\\[\\|#\\?@(\\|@(\\|#(\\|{\\|#{\\|#::{\\|#\\?(" nil t)
                  (let ((class
                         (cond ((string= (match-string 0) "#{")
                                "clojure-set")
                               ((string= (match-string 0) "{")
                                "clojure-map")
                               ((string= (match-string 0) "#(")
                                "clojure-lambda")
                               ((string= (match-string 0) "@(")
                                "clojure-deref-list")
                               ((string= (match-string 0) "#?@(")
                                "clojure-reader-conditional-splice")
                               ((string= (match-string 0) "#?(")
                                "clojure-reader-conditional")
                               ((string= (match-string 0) "#::{")
                                "clojure-namespaced-map")
                               ((string= (match-string 0) "#object[")
                                "clojure-object")
                               (t
                                (error "Unexpected class %s" (match-string 0))))))
                    (unless (lightlispy--in-string-or-comment-p)
                      (backward-char 1)
                      (save-excursion
                        (if (save-match-data
                              (looking-at "((ly-raw string"))
                            (forward-list 1)
                          (with-syntax-table lightlispy--braces-table
                            (forward-list 1)))
                        (delete-char -1)
                        (insert "))"))
                      (delete-region (match-beginning 0) (match-end 0))
                      (insert "(ly-raw " class " ("))))
                ;; ——— #1 —————————————————————
                ;; Elisp syntax for circular lists
                (goto-char (point-min))
                (while (re-search-forward "\\(?:^\\|\\s-\\|\\s(\\)\\(#[0-9]+\\)" nil t)
                  (unless (lightlispy--in-string-p)
                    (replace-match (format "(ly-raw reference %S)"
                                           (substring-no-properties
                                            (match-string 1)))
                                   nil nil nil 1)))
                ;; ——— ' ——————————————————————
                (goto-char (point-min))
                (while (re-search-forward "'" nil t)
                  (unless (lightlispy--in-string-or-comment-p)
                    (backward-char 1)
                    (let ((beg (point))
                          (sxp (ignore-errors (read (current-buffer)))))
                      (when (and (consp sxp)
                                 (eq (car sxp) 'quote))
                        (insert ")")
                        (goto-char beg)
                        (delete-char 1)
                        (insert "(ly-raw quote ")))))
                ;; ——— ` ——————————————————————
                (goto-char (point-min))
                (while (re-search-forward "\\(?:[^\\]\\|^\\)`" nil t)
                  (unless (lightlispy--in-string-or-comment-p)
                    (cond ((looking-at lightlispy-left)
                           (delete-char -1)
                           (insert "(ly-raw \\` ")
                           (forward-list 1)
                           (insert ")")
                           (backward-list 1)
                           (forward-char 7))
                          ((looking-at "\\sw\\|\\s_\\|[,@]")
                           (let ((beg (point)))
                             (forward-sexp 1)
                             (insert "\")")
                             (goto-char (1- beg))
                             (insert "(ly-raw quasiquote \""))))))
                ;; ——— , ——————————————————————
                (lightlispy--replace-regexp-in-code "\\\\," "(ly-raw comma-symbol)")
                (goto-char (point-min))
                (while (re-search-forward "[^\\]?,[^@\"]" nil t)
                  (unless (lightlispy--in-string-or-comment-p)
                    (backward-char 2)
                    (if (memq major-mode lightlispy-clojure-modes)
                        (progn
                          (delete-char 1)
                          (insert "(ly-raw clojure-comma)"))
                      (let ((beg (point))
                            (sxp (ignore-errors (read (current-buffer)))))
                        (when (and (consp sxp)
                                   (eq (car sxp) '\,))
                          (insert ")")
                          (goto-char beg)
                          (delete-char 1)
                          (insert "(ly-raw \\, "))))))
                ;; ——— angle syntax —————————
                ;; used for markers/buffers/windows/overlays
                (goto-char (point-min))
                (while (re-search-forward "#<" nil t)
                  (unless (lightlispy--in-string-or-comment-p)
                    (delete-region (match-beginning 0) (match-end 0))
                    (insert "(ly-raw angle \"")
                    (re-search-forward ">")
                    (backward-delete-char 1)
                    (insert "\")")))
                ;; ——— cons cell syntax ———————
                (lightlispy--replace-regexp-in-code " \\. " " (ly-raw dot) ")
                ;; Racket stuff
                (lightlispy--replace-regexp-in-code "#t" "(ly-raw racket-true)")
                (lightlispy--replace-regexp-in-code "#f" "(ly-raw racket-false)")
                (goto-char (point-min))
                (while (re-search-forward "#:\\(\\(?:\\sw\\|\\s_\\)+\\)" nil t)
                  (unless (lightlispy--in-string-or-comment-p)
                    (replace-match (format "(ly-raw racket-option %s)"
                                           (match-string 1)))))
                ;; Clojure # in a symbol
                (goto-char (point-min))
                (while (re-search-forward "\\_<\\(?:\\sw\\|\\s_\\)+\\_>" nil t)
                  (unless (lightlispy--in-string-p)
                    (when (cl-position ?# (match-string 0))
                      (let* ((bnd (lightlispy--bounds-dwim))
                             (str (lightlispy--string-dwim bnd)))
                        (delete-region (car bnd) (cdr bnd))
                        (insert (format "(ly-raw symbol %S)" str))))))
                ;; Clojure (. object method)
                (goto-char (point-min))
                (while (re-search-forward "(\\.[\t\n ]" nil t)
                  (if (setq cbnd (lightlispy--bounds-string))
                      (goto-char (cdr cbnd))
                    (forward-char -1)
                    (delete-char -1)
                    (insert "(ly-raw clojure-dot)")))
                ;; ———  ———————————————————————
                (buffer-substring-no-properties
                 (point-min)
                 (point-max)))))
    (ignore-errors
      (read str))))

(defun lightlispy--insert (expr)
  "Insert the EXPR read by `lightlispy--read'."
  (let ((start-pt (point))
        beg
        sxp type)
    (prin1 expr (current-buffer))
    (save-restriction
      (narrow-to-region start-pt (point))
      (goto-char (point-min))
      (while (and (re-search-forward "(ly-raw" nil t)
                  (setq beg (match-beginning 0)))
        (goto-char beg)
        (setq sxp (ignore-errors (read (current-buffer))))
        (setq type (cadr sxp))
        (cl-case type
          (newline
           (delete-region beg (point))
           (delete-char
            (- (skip-chars-backward " ")))
           (insert "\n"))
          ((string comment symbol float quasiquote)
           (delete-region beg (point))
           (insert (cl-caddr sxp)))
          (comma-symbol
           (delete-region beg (point))
           (insert "\\,"))
          (ignore
           (delete-region beg (point))
           (backward-delete-char 1))
          (raw
           (delete-region beg (point))
           (prin1 (cons 'ly-raw (cddr sxp))
                  (current-buffer))
           (backward-list)
           (forward-char 7))
          (quote
           (delete-region beg (point))
           (insert "'")
           (let ((it (cl-caddr sxp)))
             (if it
                 (prin1 it (current-buffer))
               (insert "()")))
           (goto-char beg))
          (empty
           (delete-region beg (point))
           (insert "()"))
          (char
           (delete-region beg (point))
           (insert "?" (cl-caddr sxp)))
          (clojure-char
           (delete-region beg (point))
           (insert (cl-caddr sxp)))
          (lisp-char
           (delete-region beg (point))
           (insert (cl-caddr sxp)))
          (lisp-macro
           (delete-region beg (point))
           (insert (cl-caddr sxp)))
          (clojure-gensym
           (delete-region beg (point))
           (insert (cl-caddr sxp)))
          (function
           (delete-region beg (point))
           (insert (format "#'%S" (cl-caddr sxp)))
           (goto-char beg))
          (clojure-dot
           (delete-region beg (point))
           (insert "."))
          (clojure-lambda
           (delete-region beg (point))
           (insert (format "#%S" (cl-caddr sxp)))
           (goto-char beg))
          (clojure-set
           (delete-region beg (point))
           (insert (format "#{%s}" (lightlispy--splice-to-str (cl-caddr sxp))))
           (goto-char beg))
          (clojure-map
           (delete-region beg (point))
           (insert (format "{%s}" (lightlispy--splice-to-str (cl-caddr sxp))))
           (goto-char beg))
          (clojure-object
           (delete-region beg (point))
           (insert (format "#object[%s]" (lightlispy--splice-to-str (cl-caddr sxp))))
           (goto-char beg))
          (clojure-namespaced-map
           (delete-region beg (point))
           (insert (format "#::{%s}" (lightlispy--splice-to-str (cl-caddr sxp))))
           (goto-char beg))
          (clojure-deref-list
           (delete-region beg (point))
           (insert (format "@(%s)" (lightlispy--splice-to-str (cl-caddr sxp))))
           (goto-char beg))
          (clojure-reader-conditional-splice
           (delete-region beg (point))
           (insert (format "#?@(%s)" (lightlispy--splice-to-str (cl-caddr sxp))))
           (goto-char beg))
          (clojure-reader-conditional
           (delete-region beg (point))
           (insert (format "#?(%s)" (lightlispy--splice-to-str (cl-caddr sxp))))
           (goto-char beg))
          (clojure-reader-comment
           (delete-region beg (point))
           (insert (format "#_%S" (cl-caddr sxp)))
           (goto-char beg))
          (clojure-comma
           (delete-region beg (point))
           (delete-horizontal-space)
           (insert ", "))
          (racket-true
           (delete-region beg (point))
           (insert "#t"))
          (racket-false
           (delete-region beg (point))
           (insert "#f"))
          (racket-option
           (delete-region beg (point))
           (insert (format "#:%S" (cl-caddr sxp))))
          (angle
           (delete-region beg (point))
           (insert (format "#<%s>" (cl-caddr sxp)))
           (goto-char beg))
          (reference
           (delete-region beg (point))
           (insert (cl-caddr sxp)))
          (\`
           (if (> (length sxp) 3)
               (progn
                 (goto-char beg)
                 (insert "`")
                 (delete-region (+ (point) 1)
                                (+ (point) 11)))
             (delete-region beg (point))
             (insert "`")
             (prin1 (cl-caddr sxp) (current-buffer)))
           (goto-char beg))
          (\,
           (delete-region beg (point))
           (insert ",")
           (prin1 (cl-caddr sxp) (current-buffer))
           (goto-char beg))
          (comma-splice
           (delete-region beg (point))
           (insert ",@")
           (prin1 (cl-caddr sxp) (current-buffer))
           (goto-char beg))
          (dot
           (delete-region beg (point))
           (insert "."))
          (t (goto-char (1+ beg)))))
      (goto-char (point-min))
      (while (re-search-forward "\\(?:\\s_\\|\\sw\\)\\(\\\\\\?\\)" nil t)
        (replace-match "?" t t nil 1))
      (goto-char (point-min))
      (while (re-search-forward "\\\\\\." nil t)
        (unless (save-match-data
                  (lightlispy--in-string-p))
          (replace-match ".")))
      (goto-char (point-min))
      (while (re-search-forward "[0-9]+\\(\\\\#\\)" nil t)
        (replace-match "#" nil t nil 1))
      (when lightlispy-do-fill
        (goto-char (point-min))
        (while (re-search-forward " " nil t)
          (cond ((lightlispy--in-string-p))

                ((lightlispy--in-comment-p)
                 (fill-paragraph)
                 (goto-char (cdr (lightlispy--bounds-comment))))

                ((> (current-column) fill-column)
                 (newline-and-indent)))))
      (goto-char (point-max))
      (widen)))
  (when (and (lightlispy-right-p)
             (not (lightlispy--in-comment-p)))
    (backward-list)
    (indent-sexp)
    (forward-list)))

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

    ;; From special lispy-mode-map-special
    (lightlispy-define-key map "l" 'lightlispy-right)
    (lightlispy-define-key map "h" 'lightlispy-left)
    (lightlispy-define-key map "f" 'lightlispy-flow)
    (lightlispy-define-key map "j" 'lightlispy-down)
    (lightlispy-define-key map "k" 'lightlispy-up)
    (lightlispy-define-key map "d" 'lightlispy-different)
    (lightlispy-define-key map ">" 'lightlispy-slurp)
    (lightlispy-define-key map "<" 'lightlispy-barf)
    (lightlispy-define-key map "/" 'lightlispy-splice)
    (lightlispy-define-key map "r" 'lightlispy-raise)
    (lightlispy-define-key map "R" 'lightlispy-raise-some)
    (lightlispy-define-key map "+" 'lightlispy-join)

    ;; from lispy-mode-map-base
    (define-key map (kbd "C-k") 'lightlispy-kill)
    (define-key map (kbd "M-d") 'lightlispy-kill-word)
    (define-key map (kbd "M-DEL") 'lightlispy-backward-kill-word) 
    (define-key map (kbd "(") 'lightlispy-parens)
    (define-key map (kbd ";") 'lispy-comment)
    
    ;; from lispy-mode-map-lispy
    (define-key map "[" 'lightlispy-forward)
    (define-key map "]" 'lightlispy-backward)
    (define-key map (kbd "C-d") 'lightlispy-delete)
    (define-key map (kbd "{") 'lightlispy-braces)
    (define-key map (kbd "}") 'lightlispy-brackets)
    (define-key map (kbd "\"") 'lightlispy-quotes)
   
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
