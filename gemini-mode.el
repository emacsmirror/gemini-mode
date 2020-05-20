;;; gemini.el --- A simple highlighting package for text/gemini 

;; Copyright (C) 2020  Jason McBrayer

;; Author: Jason McBrayer <jmcbray@carcosa.net>
;; Created: 20 May 2020
;; Version: 0.1.0
;; Keywords: syntax gemini
;; Homepage: https://git.carcosa.net/jmcbray/gemini.el
;; Package-Requires: ((emacs "24"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(defface gemini-heading-face-1
  '((t :inherit bold :height 1.8))
  "Face for Gemini headings level 1")
(defface gemini-heading-face-2
  '((t :inherit bold :height 1.4))
  "Face for Gemini headings level 2")
(defface gemini-heading-face-3
  '((t :inherit bold :height 1.2))
  "Face for Gemini headings level 3")
(defface gemini-heading-face-rest
  '((t :inherit bold))
  "Face for Gemini headings below level 3")

(defvar gemini-highlights
  (let* ((gemini-heading-3-regexp "^###\s.*$")
         (gemini-heading-2-regexp "^##\s.*$")
         (gemini-heading-1-regexp "^#\s.*$")
         (gemini-heading-rest-regexp "^###+\s.*$")
         (gemini-link-regexp "^=>.*$"))
    `((,gemini-heading-rest-regexp . 'gemini-heading-face-rest)
      (,gemini-heading-3-regexp . 'gemini-heading-face-3)
      (,gemini-heading-2-regexp . 'gemini-heading-face-2)
      (,gemini-heading-1-regexp . 'gemini-heading-face-1)
      (,gemini-link-regexp . 'link)))
  "Font lock keywords for gemini-mode")


(define-derived-mode gemini-mode text-mode "gemini"
  "Major mode for editing text/gemini 'geminimap' documents"
  (setq font-lock-defaults '(gemini-highlights)))

(provide 'gemini-mode)
