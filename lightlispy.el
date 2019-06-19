;;; light-lispy.el --- Light Lispy -*- lexical-binding: t -*-

;; Copyright (C) 2019 -

;; Author: Vitor <vitorqb@gmail.com>
;; Version: 0.0.1
;; Maintainer: Vitor <vitorqb@gmail.com>
;; Created: 2019-06-18
;; Keywords: lispy structural-editting
;; Homepage: https://github.com/vitorqb/light-lispy

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

(defvar-local lightlispy-outline-header ";;"
  "Store the buffer-local outline start.")

(defsubst lightlispy-looking-back (regexp)
  "Forward to (`looking-back' REGEXP)."
  (looking-back regexp (line-beginning-position)))

(defsubst lightlispy-right-p ()
  "Return t if after variable `lightlispy-right'."
  (looking-back lightlispy-right
                (line-beginning-position)))

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

(defun lispy--in-string-or-comment-p ()
  "Test if point is inside a string or a comment."
  (let* ((sp (syntax-ppss))
         (beg (nth 8 sp)))
    (when (or (eq (char-after beg) ?\")
              (nth 4 sp))
      beg)))

(defun lispy--exit-string ()
  "When in string, go to its beginning."
  (let ((s (syntax-ppss)))
    (when (nth 3 s)
      (goto-char (nth 8 s)))))

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

(defun lightlispy--bounds-comment ()
  "Return bounds of current comment."
  (and (lightlispy--in-comment-p)
       (save-excursion
         (when (lightlispy--beginning-of-comment)
           (let ((pt (point)))
             (while (and (light-lispy--in-comment-p)
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

;;; orgext.el ends here
