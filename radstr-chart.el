;;; radstr-chart.el --- Creating Radical Strokes Chart  -*- lexical-binding: t; -*-

;; Filename: radstr-chart.el
;; Description: Creating Radical Strokes Chart
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

(setq max-lisp-eval-depth 40000)
(setq max-specpdl-size 100000)

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
(defvar radstr-html-file
  (expand-file-name "radstr.html" radstr-directory))
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

(defun radstr-html-char-figure (char)
  "CHAR to HTML5 figcaption."
  (if (characterp char)
      (if (and (< #x9fcc char) (< char #xa000))
          (let ((file (format "./UNC/%04X.png" char)))
            (message "char=%c file=%s" char file)
            (if (file-exists-p (expand-file-name file))
                (format
                 "<figure><img height='24' src='%s' alt='%5X'/><figcaption>(%05X)</figcaption></figure>"
                 file char char)))
        (format "<figure>%c<figcaption>%05X</figcaption></figure>" char char))
    (let* ((string (symbol-name char))
           (_match (string-match "^\\(.+?\\)\\(F.\\)" string))
           (number (match-string 1 string))
           (f-set (match-string 2 string))
           (file (concat "." ;; radstr-directory
                         (if (equal (match-string 2 string) "F1")
                             "/ExtF1v3/" "/ExtF2v2/")
                         (match-string 1 string) ".png")))
      (when (file-exists-p (expand-file-name file))
        (format "<figure><img height='24' src='%s' alt='%s'/><figcaption>%s(%s)</figcaption></figure>"
                file number number f-set)))))

(defvar radstr-serialize-table (make-hash-table :test 'equal))

(defun radstr-serialize-char (char &optional dir)
  "Serialize CHAR (character, ids or symbol) with DIR.
If CHAR is symbol, equivalent IDS will be processed.
If CHAR is character, its normalized IDS expansion will be examined.
If IDS is null, then CHAR is returned.
If DIR is null, first char of IDS will be used.
Only when IDC is equal to DIR, then last element will be logger."
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
      (unless dir (setq dir idc))
      (if (/= idc dir) char
        (cl-callf radstr-serialize-char (elt ids 2) dir)
        ids))))

(defun radstr-serialize-string (char)
  "Serialize IDS string for comparison of CHAR."
  (or (gethash char radstr-serialize-table)
      (puthash
       char
       (apply 'string (-flatten (radstr-serialize-char char)))
       radstr-serialize-table)))

(defun radstr-compare-chars (x y)
  "Compare two chars X Y by IDS."
  (if (featurep 'ids-db)
      (string< (radstr-serialize-string x)
               (radstr-serialize-string y))
    (string< (car (gethash x radstr-ids-table))
             (car (gethash y radstr-ids-table)))))

(defun radstr-html-output ()
  "Output HTML file."
  (interactive)
  (let ((radstrs (sort (ht-keys radstr-rev-table)
                       (lambda (x y)
                         (or (< (car x) (car y))
                             (and (= (car x) (car y))
                                  (< (cdr x) (cdr y)))))))
        (old-rad 0))
    (with-temp-file radstr-html-file
      (dolist (radstr radstrs)
        (let ((rad (car radstr))
              (str (cdr radstr))
              (chars (sort (copy-sequence (gethash radstr radstr-rev-table))
                           'radstr-compare-chars)))
          (when (/= rad old-rad)
            (insert (format "
</table>

<h1%s>%d (<span class='a'>%c</span>)</h1>
<table>" (if (integerp rad) (format " id='r%03d'" rad) "")
            rad (gethash rad radstr-num2char-table)))
            (setq old-rad rad))
          (insert (format "
<tr><td>%s</td><td>%s</td><tr>"
                          str
                          (mapconcat 'radstr-html-char-figure
                           chars "")))))
      (goto-char (point-min))
      (search-forward "<h1")
      (delete-region (point-min) (match-beginning 0))
      (goto-char (point-min))
      (insert "<html>
<head>
<meta http-equiv='Content-Type' content='text/html; charset=utf-8' />
<link rel='stylesheet' type='text/css' href='pdf.css' />
<style type='text/css'>
  @page {
    @top-center {
      content: string(Chapter);
     }
  }
  figure {
    text-align: center;
    display: inline-block;
    margin : 1px;
    margin-left: 4px;
    margin-right: 4px;
    border: 1px;
    font-family: 'Hanazono Mincho A', 'Hanazono Mincho B';
    font-size: 24px;
  }
  figcaption {
    font-family: Inconsolata;
    font-size:xx-small;
  }
  table,td {
    border-width: 0 0 0 0px;
    border-collapse: collapse;
    border:solid 1px;
    background: none;
    text-align: left;
    text-indent: 0px;
    vertical-align: top;
  }
  .a {font-family: Hanazono Mincho A; }
  h1 {string-set: Chapter self; }
  img {border: 1px solid red;}
</style>
</head>
<body data-type='book'>
  <section data-type='titlepage'>
    <h1>UCS Ideographs Radical-Strokes Chart</h1>
    <h2 data-type='author'>ISO/IEC JTC 1/SC 2/WG 2 IRG</h2>
  </section>
  <nav data-type='toc'>
    <h2>Table of Contents</h2>
<ul>
")
      (cl-do ((rad 1 (+ rad 1))) ((> rad 214))
        (insert (format "<li><a href='#r%03d'>%d (%c)</a></li>\n" rad rad
                        (gethash rad radstr-num2char-table))))
      (insert "
</ul>
</nav>
<section data-type='chapter' id='main'>
")
      (goto-char (point-max))
      (insert "
</table></section></body></html>"))))

(when noninteractive
  (radstr-setup)
  (message "Radical Strokes Setup.")
  (radstr-html-output))

(provide 'radstr-chart)
;;; radstr-chart.el ends here
