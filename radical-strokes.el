;;; radical-strokes.el --- Radical and Strokes of Ideographs  -*- lexical-binding: t; -*-

;; Filename: radical-strokes.el
;; Description: Managing Radicals and Strokes of Kanji Characters
;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; Created: 2014-08-15
;; Keywords: i18n languages
;; URL: https://github.com/kawabata/Radical-Strokes-Chart

;; Copyright (C) 2014 KAWABATA, Taichi

;; Author: KAWABATA, Taichi <kawabata.taichi@lab.ntt.co.jp>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;; Standard Libraries
(require 'cl-lib)
(require 'bytecomp)

;; MELPA libraries
(require 'dash)
(require 'ids-edit)
(require 'ht)

;; Github libraries
(require 'ids-db)

(defvar radstr-directory
  (file-name-directory (or byte-compile-current-file
                           load-file-name
                           buffer-file-name)))
(defvar radstr-variants-file
  (expand-file-name "radical-variants.txt" radstr-directory))
(defvar radstr-cjksrc-file
  (expand-file-name "CJKSrc.txt" radstr-directory))
(defvar radstr-cjksrc2-file
  (expand-file-name "CJKSrc2.txt" radstr-directory))
(defvar radstr-extf-file
  (expand-file-name "extf-ids.txt" radstr-directory))

;;;; load various data from files

(defvar radstr-ids-table nil)

(defvar radstr-table nil
  "Character/Identifer to ((Radical . Strokes)..) table.
Simplified Radical should have X.1 number.")

(defvar radstr-regexp2num-table nil
  "Regular Expression to Radical Number table.")

(defvar radstr-regexp nil
  "Radical Regular expression.")

(defvar radstr-char2num-table nil
  "Radical Char to number table.")

(defvar radstr-num2char-table nil
  "Radical num to char table.")

(defvar radstr-rev-table ()
  "(Radical . Strokes) to (chars/identifiers....) table.")

(defun radstr--addhash (key value table)
  "Add to KEY a VALUE in table TABLE."
  (let ((values (gethash key table)))
    (if (not (member value values))
        (puthash key (cons value values) table))))

(defun radstr-setup ()
  "1st step. Setup."
  (interactive)
  (setq radstr-ids-table (copy-hash-table ids-edit-table))
  (radstr-table-setup)
  (radstr-char2num-table-setup)
  (radstr-extf-setup)
  (radstr-calculate-table)
  (radstr-reverse-table-setup))

(defun radstr-table-setup ()
  "Set up initial `radstr-table'."
  (interactive)
  (setq radstr-table (make-hash-table :test 'equal))
  (with-temp-buffer
    (insert-file-contents radstr-cjksrc-file)
    (insert-file-contents radstr-cjksrc2-file)
    (goto-char (point-min))
    (while (re-search-forward
            "U\\+\\([0-9A-F]+\\)	kRSUnicode	\\([0-9]+\\('\\)?\\)\\.\\([0-9]+\\)"
            nil t)
      (let ((char (string-to-number (match-string 1) 16))
            (rad (+ (string-to-number (match-string 2))
                    (if (match-string 3) 0.1 0)))
            (str (string-to-number (match-string 4))))
        (puthash char `((,rad . ,str)) radstr-table)))))

(defun radstr-char2num-table-setup ()
  "Set up `radstr-char2num-table' and `radstr-regexp2num-table'."
  (interactive)
  (setq radstr-char2num-table (make-hash-table))
  (setq radstr-num2char-table (make-hash-table))
  (setq radstr-regexp2num-table (make-hash-table :test 'equal))
  (cl-do ((char ?⼀ (+ char 1))) ((> char #x2fd5))
    (puthash (cadr (get-char-code-property char 'decomposition))
             (- char ?⼀ -1) radstr-char2num-table)
    (puthash (- char ?⼀ -1)
             (cadr (get-char-code-property char 'decomposition))
             radstr-num2char-table))
  (with-temp-buffer
    (insert-file-contents radstr-variants-file)
    (goto-char (point-min))
    (while (re-search-forward
            "\\(.\\),cjkvi/radical-variant,\\(.\\)" nil t)
      (let ((num (gethash (string-to-char (match-string 1)) radstr-char2num-table))
            (char (string-to-char (match-string 2))))
        (unless num (error "Not radical! %s" num))
        (puthash char num radstr-char2num-table)))
    ;; exception case
    (remhash ?阝 radstr-char2num-table)
    (goto-char (point-min))
    (while (re-search-forward
            "\\(.\\),cjkvi/radical-variant-simplified,\\(.\\)" nil t)
      (let ((num (gethash (string-to-char (match-string 1)) radstr-char2num-table))
            (char (string-to-char (match-string 2))))
        (unless num (error "Not radical! %s" num))
        (puthash char (+ num 0.1) radstr-char2num-table)
        (puthash (+ num 0.1) char radstr-num2char-table)))
    (goto-char (point-min))
    (while (re-search-forward
            "\\(.\\),cjkvi/radical-split,\\(.+\\)" nil t)
      (let ((num (gethash (string-to-char (match-string 1)) radstr-char2num-table))
            (string (substring (match-string 2) 1)))
        (unless num (error "Not radical! %s" num))
        (puthash (concat "^." (replace-regexp-in-string "�" "\\\\(.+\\\\)" string) "$")
                 num radstr-regexp2num-table))))
  (setq radstr-regexp
        (regexp-opt (mapcar 'string (ht-keys radstr-char2num-table)))))

(defun radstr-extf-setup ()
  (interactive)
  (with-temp-buffer
    (insert-file-contents radstr-extf-file)
    (while (re-search-forward
            "^\\([0-9]+F[12]\\)/\\([0-9]+?\\)\\('\\)?\\.\\([0-9]+\\),\\(.+\\)" nil t)
      (let ((f-num (intern (match-string 1)))
            (radical (string-to-number (match-string 2)))
            (simplified (match-string 3))
            (strokes (string-to-number (match-string 4)))
            (ids (match-string 5)))
        (puthash f-num (split-string ids "/") radstr-ids-table)
        (puthash f-num `((,(+ radical (if simplified 0.1 0)) . ,strokes))
                 radstr-table)))))

(defun radstr-strokes-chars (chars)
  "Calculate a list of possible total strokes of CHARS."
  (let* ((uniq (-uniq chars))
         (count-list (-map (lambda (char) (--count (= it char) chars)) uniq))
         (strokes-list (--map (or (gethash it ids-edit-stroke-table) '(0)) uniq))
         (strokes-list (-zip-with (lambda (count strokes) (--map (* count it) strokes))
                                  count-list strokes-list)))
    (apply '-table-flat (cons '+ strokes-list))))

(defun radstr-calculate-table ()
  "Calculate and find implicit radical/strokes pairs for all
characters in `radstr-ids-table'."
  (interactive)
  (maphash
   (lambda (char idses)
     (when (gethash char radstr-table)
       (dolist (ids idses)
         (with-temp-buffer
           (insert ids) (goto-char (point-min))
           (while (re-search-forward radstr-regexp nil t)
             (let* ((radical
                     (gethash (string-to-char (match-string 0)) radstr-char2num-table))
                    (chars
                     (nconc (string-to-list (buffer-substring (point-min) (match-beginning 0)))
                            (string-to-list (buffer-substring (match-end 0) (point-max)))))
                    (strokes (radstr-strokes-chars chars)))
               (dolist (stroke strokes)
                 (radstr--addhash char (cons radical stroke) radstr-table))))
           ;; 阝 case
           (maphash
            (lambda (regexp radical)
              (goto-char (point-min))
              (when (looking-at regexp)
                (dolist (stroke (radstr-strokes-chars
                                 (string-to-list (match-string 1))))
                  (radstr--addhash char (cons radical stroke) radstr-table))))
            radstr-regexp2num-table)))))
   radstr-ids-table))

(defun radstr-reverse-table-setup ()
  (interactive)
  (setq radstr-rev-table (make-hash-table :test 'equal))
  (maphash
   (lambda (char radstrs)
     (dolist (radstr radstrs)
       (radstr--addhash radstr char radstr-rev-table)))
   radstr-table))

;;; Serialization

(defun radstr-serialize-char (char)
  "Serialize CHAR (character, ids or symbol).
If CHAR is symbol, equivalent IDS will be processed.
If CHAR is character, its normalized IDS expansion will be examined.
If IDS is null, then CHAR is returned."
  (let* ((char (if (symbolp char)
                   (car
                    (ids-normalize-shrink
                     (ids-normalize-canonicalize
                      (ids-normalize-structure
                       (copy-tree (car (gethash char radstr-ids-table)))))))
                 char))
         (ids (if (characterp char)
                  (car (copy-tree (gethash char ids-normalize-table))) char))
         (idc (car-safe ids)))
    (if (null idc) char
      (list idc (radstr-serialize-char (elt ids 1))
            (radstr-serialize-char (elt ids 2))))))

;;; Vectorization

(defvar radstr-vectorize-table (make-hash-table :test 'equal))

(defun radstr-vectorize-char (char)
  "Vectorize IDS and cache CHAR."
  (or (gethash char radstr-vectorize-table)
      (let ((vector (radstr-vectorize
                     (radstr-serialize-char char))))
        (puthash
         char
         (if (listp vector) vector (list vector))
         radstr-vectorize-table))))

(defun radstr-vectorize (ids)
  (if (characterp ids) ids
    (let* ((idc (car ids))
           (1st (radstr-vectorize (elt ids 1)))
           (2nd (radstr-vectorize (elt ids 2))))
      (if (or (integerp 2nd)
              (/= idc (car 2nd)))
          (list idc 1st 2nd)
        (cons idc (cons 1st (cdr 2nd)))))))

;;; Comparison of Ideographs

(defun radstr-compare-vectors (vec1 vec2)
  (if (or (null vec1) (null vec2)) vec2
    (let ((car1 (car vec1))
          (car2 (car vec2)))
      (if (or (listp car1) (listp car2))
          (if (not (listp car1)) t
            (if (not (listp car2)) nil
              (if (equal car1 car2)
                  (radstr-compare-vectors (cdr vec1) (cdr vec2))
                (radstr-compare-vectors car1 car2))))
        ;; Neither car1 nor car2 is a list.
        (if (= car1 car2) (radstr-compare-vectors (cdr vec1) (cdr vec2))
          (let ((rs1 (car (last (gethash car1 radstr-table))))
                (rs2 (car (last (gethash car2 radstr-table)))))
            (if (or (null rs1) (null rs2))
                (if (and (null rs1) (null rs2))
                    (< car1 car2) rs2)
              (if (< (car rs1) (car rs2)) t
                (if (and (= (car rs1) (car rs2))
                         (< (cdr rs1) (cdr rs2))) t
                  (if (and (= (car rs1) (car rs2))
                           (= (cdr rs1) (cdr rs2))
                           (< car1 car2))
                      t nil))))))))))

(defun radstr-compare-chars (char1 char2)
  (let ((vec1 (radstr-vectorize-char char1))
        (vec2 (radstr-vectorize-char char2)))
    (if (equal vec1 vec2)
        (if (characterp char1)
            (if (characterp char2)
                (< char1 char2)
              t)
          (if (characterp char2) nil t))
      (radstr-compare-vectors vec1 vec2))))

(provide 'radical-strokes)

;;; radstr-chart.el ends here
