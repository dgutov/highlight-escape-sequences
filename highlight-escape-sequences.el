;;; highlight-escape-sequences.el --- Highlight escape sequences -*- lexical-binding: t -*-

;; Author:   Dmitry Gutov <dgutov@yandex.ru>
;; URL:      https://github.com/dgutov/highlight-escape-sequences
;; Keywords: convenience
;; Version:  0.2

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This global minor mode highlights escape sequences in strings and
;; other kinds of literals with `hes-escape-sequence-face' which
;; inherits from `font-lock-regexp-grouping-construct' face by
;; default and with `hes-escape-backslash-face' which inherits from
;; `font-lock-regexp-grouping-backslash' face by default.

;; It currently supports `ruby-mode', both main JavaScript modes,
;; and C/C++ modes treating the former ones as subsets of the latter.

;; To enable it elsewhere, customize `hes-simple-modes'.

;; Put this in the init file:
;;
;; (hes-mode)

;;; Code:

(defgroup hes-mode nil
  "Highlight escape sequences"
  :group 'convenience)

(defface hes-escape-sequence-face
  '((default :inherit font-lock-regexp-grouping-construct))
  "Face to highlight an escape sequence."
  :group 'hes-mode)

(defface hes-escape-backslash-face
  '((default :inherit font-lock-regexp-grouping-backslash))
  "Face to highlight an escape backslash."
  :group 'hes-mode)

(defconst hes-common-escape-sequence-re
  (concat
   "\\(\\\\\\("
   "[0-3][0-7]\\{0,2\\}\\|[4-7][0-7]?"
   "\\|"
   "x[[:xdigit:]]\\{2\\}"
   "\\|"
   "u[[:xdigit:]]\\{4\\}"
   "\\|"
   "[\'\"\\bfnrtv]"
   "\\)\\)")
  "Regexp to match the most common escape sequences.
Handles octals (\\0-\\377), hexadecimals (\\x00-\\xFF), unicodes
\(\\u0000-\\uFFFF), and backslash followed by one of `bfnrtv'.")

(defconst hes-c/c++-escape-sequence-re
  (concat
   "\\(\\\\\\("
   "[0-3][0-7]\\{0,2\\}\\|[4-7][0-7]?"
   "\\|"
   "x[[:xdigit:]]+"
   "\\|"
   "u[[:xdigit:]]\\{4\\}"
   "\\|"
   "U[[:xdigit:]]\\{8\\}"
   "\\|"
   "[\'\"\?\\abfnrtv]"
   "\\)\\)")
  "Regexp to match C/C++ escape sequences.")

(defconst hes-java-escape-sequence-re
  (concat
   "\\(\\\\\\("
   "[0-3][0-7]\\{0,2\\}\\|[4-7][0-7]?"
   "\\|"
   "u[[:xdigit:]]\\{4\\}"
   "\\|"
   "[\'\"\\bfnrt]"
   "\\)\\)")
  "Regexp to match JavaScript escape sequences.")

(defconst hes-js-escape-sequence-re
  (concat
   "\\(\\\\\\("
   "[0-3][0-7]\\{0,2\\}\\|[4-7][0-7]?"
   "\\|"
   "x[[:xdigit:]]\\{2\\}"
   "\\|"
   "u[[:xdigit:]]\\{4\\}"
   "\\|"
   ;; "[\'\"\\bfnrtv]"
   ;; "\\|"
   "."
   "\\)\\)")
  "Regexp to match JavaScript escape sequences.")

(defconst hes-ruby-escape-sequence-re
  (concat
   "\\(\\\\\\("
   "[0-3][0-7]\\{0,2\\}\\|[4-7][0-7]?"
   "\\|"
   "x[[:xdigit:]]\\{1,2\\}"
   "\\|"
   "u\\(?:"
   "[[:xdigit:]]\\{4\\}"
   "\\|"
   "{"
   "\\(?:10?[[:xdigit:]]\\{0,4\\}\\|0?[[:xdigit:]]\\{1,5\\}\\)"
   "\\(?:[[:space:]]+\\(?:10?[[:xdigit:]]\\{0,4\\}\\|0?[[:xdigit:]]\\{1,5\\}\\)\\)*"
   "}"
   "\\)"
   "\\|"
   ;; "[\'\"\\abefnrstv]"
   ;; "\\|"
   "."
   "\\)\\)")
  "Regexp to match Ruby escape sequences.
Currently doesn't handle \\C-, \\M- etc.")

(defun hes--make-ruby-keywords(re)
  `((,re
     (1 (let* ((state (syntax-ppss))
	       (term (nth 3 state)))
	  (when (or (and (eq term ?')
			 (member (match-string 2) '("\\" "'")))
		    (if (fboundp 'ruby-syntax-expansion-allowed-p)
			(ruby-syntax-expansion-allowed-p state)
		      (memq term '(?\" ?/ ?\n ?` t))))
	    'hes-escape-backslash-face))
	prepend))))

(defun hes--make-simple-keywords(re)
  `((,re
     (1 (when (nth 3 (syntax-ppss))
	  'hes-escape-backslash-face)
	prepend)
     (2 (when (nth 3 (syntax-ppss))
	  'hes-escape-sequence-face)
	prepend))))

(defcustom hes-simple-modes '()
  "Modes where escape sequences can appear in any string literal."
  :type '(repeat function)
  :set (lambda (symbol value)
         (if (bound-and-true-p hes-mode)
             (progn
               (hes-mode -1)
               (set-default symbol value)
               (hes-mode 1))
           (set-default symbol value))))

;;;###autoload
(defun turn-on-hes-mode()
  "Turn on highlighting of escape sequences."
  (interactive)
  (font-lock-add-keywords 'ruby-mode (hes--make-ruby-keywords   hes-ruby-escape-sequence-re)  'append)
  (font-lock-add-keywords 'c-mode    (hes--make-simple-keywords hes-c/c++-escape-sequence-re) 'append)
  (font-lock-add-keywords 'c++-mode  (hes--make-simple-keywords hes-c/c++-escape-sequence-re) 'append)
  (font-lock-add-keywords 'java-mode (hes--make-simple-keywords hes-java-escape-sequence-re)  'append)
  (font-lock-add-keywords 'js-mode   (hes--make-simple-keywords hes-js-escape-sequence-re)    'append)
  (font-lock-add-keywords 'js2-mode  (hes--make-simple-keywords hes-js-escape-sequence-re)    'append)
  (dolist (mode hes-simple-modes)
    (font-lock-add-keywords mode (hes--make-simple-keywords hes-common-escape-sequence-re) 'append)))

;;;###autoload
(defun turn-off-hes-mode()
  "Turn off highlighting of escape sequences"
  (interactive)
  (font-lock-remove-keywords 'ruby-mode (hes--make-ruby-keywords   hes-ruby-escape-sequence-re))
  (font-lock-remove-keywords 'c-mode    (hes--make-simple-keywords hes-c/c++-escape-sequence-re))
  (font-lock-remove-keywords 'c++-mode  (hes--make-simple-keywords hes-c/c++-escape-sequence-re))
  (font-lock-remove-keywords 'java-mode (hes--make-simple-keywords hes-java-escape-sequence-re))
  (font-lock-remove-keywords 'js-mode   (hes--make-simple-keywords hes-js-escape-sequence-re))
  (font-lock-remove-keywords 'js2-mode  (hes--make-simple-keywords hes-js-escape-sequence-re))
  (dolist (mode hes-simple-modes)
    (font-lock-remove-keywords mode (hes--make-simple-keywords hes-common-escape-sequence-re))))

;;;###autoload
(define-minor-mode hes-mode
  "Toggle highlighting of escape sequences."
  :lighter "" :global t
  (if hes-mode
      (turn-on-hes-mode)
    (turn-off-hes-mode)))

(provide 'highlight-escape-sequences)

;;; highlight-escape-sequences.el ends here
