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
(require 'ht)

(require 'radical-strokes)

(defvar radstr-html-file
  (expand-file-name "radstr.html" radstr-directory))

;;;; load various data from files

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
