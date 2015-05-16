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
  "Face to highlight an escape sequence.")

(defface hes-escape-backslash-face
  '((default :inherit font-lock-regexp-grouping-backslash))
  "Face to highlight an escape backslash.")

(defconst hes-escape-sequence-re
  "\\(\\\\\\(\\(?:U[[:xdigit:]]\\{8\\}\\)\\|\\(?:u[[:xdigit:]]\\{4\\}\\)\\|\\(?:x[[:xdigit:]]\\{2,\\}\\)\\|\\(?:[0-7]\\{1,3\\}\\)\\|.\\)\\)"
  "Regexp to match an escape sequence.
Currently handles octals (\\123), hexadecimals (\\x12..Inf), unicode
\(\\u1234 or \\U12345678), and backslash followed by anything else.")

(defconst hes-ruby-keywords
  `((,hes-escape-sequence-re
     (1 (let* ((state (syntax-ppss))
               (term (nth 3 state)))
          (when (or (and (eq term ?')
                         (member (match-string 2) '("\\" "'")))
                    (if (fboundp 'ruby-syntax-expansion-allowed-p)
                        (ruby-syntax-expansion-allowed-p state)
                      (memq term '(?\" ?/ ?\n ?` t))))
            'hes-escape-backslash-face))
        prepend))))

(defconst hes-simple-keywords
  `((,hes-escape-sequence-re
     (1 (when (nth 3 (syntax-ppss))
          'hes-escape-backslash-face)
        prepend)
     (2 (when (nth 3 (syntax-ppss))
	  'hes-escape-sequence-face)
	prepend))))

(defcustom hes-simple-modes '(js-mode js2-mode c-mode c++-mode)
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
(define-minor-mode hes-mode
  "Toggle highlighting of escape sequences."
  :lighter "" :global t
  (if hes-mode
      (progn
        (font-lock-add-keywords 'ruby-mode hes-ruby-keywords 'append)
        (dolist (mode hes-simple-modes)
          (font-lock-add-keywords mode hes-simple-keywords 'append)))
    (font-lock-remove-keywords 'ruby-mode hes-ruby-keywords)
    (dolist (mode hes-simple-modes)
      (font-lock-remove-keywords mode hes-simple-keywords))))

(provide 'highlight-escape-sequences)

;;; highlight-escape-sequences.el ends here
