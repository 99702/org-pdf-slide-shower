;;; org-pdf-slide-show.el --- Show PDF slides inline in org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Rajan

;; Author: Rajan <rajan99702@proton.me>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.0"))
;; Keywords: hypermedia, multimedia
;; URL: https://github.com/99702/org-pdf-slide-show
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Embed PDF pages in org buffers as inline images.
;;
;; Just drop a link like [[pdfslide:./slides.pdf::3]] and this package
;; will extract that page as a PNG (via pdftoppm) and show it inline.
;; Images are cached so they only get extracted once.
;;
;; You need pdftoppm installed (poppler-utils package on most distros).
;;
;; Quickstart:
;;   (require 'org-pdf-slide-show)
;;   (add-hook 'org-mode-hook #'org-pdf-slide-show-mode)

;;; Code:

(require 'org)
(require 'org-element)
(require 'image)

(declare-function doc-view-goto-page "doc-view" (page))

(defvar org-pdf-slide-show-mode)  ; defined below by `define-minor-mode'

(defgroup org-pdf-slide-show nil
  "Show PDF slides inline in `org-mode'."
  :group 'org
  :prefix "org-pdf-slide-show-")

(defcustom org-pdf-slide-show-dpi 150
  "DPI used when extracting pages from PDFs."
  :type 'integer
  :group 'org-pdf-slide-show)

(defcustom org-pdf-slide-show-image-width 600
  "Width (px) for the inline image display."
  :type 'integer
  :group 'org-pdf-slide-show)

(defcustom org-pdf-slide-show-cache-suffix "-slides"
  "Suffix for the image cache directory.
E.g. for intro.pdf -> intro-slides/"
  :type 'string
  :group 'org-pdf-slide-show)

(defcustom org-pdf-slide-show-pdftoppm-executable "pdftoppm"
  "Path or name of the pdftoppm binary."
  :type 'string
  :group 'org-pdf-slide-show)

;; ---- internal stuff ----

(defun org-pdf-slide-show--parse-link (path)
  "Split PATH into (pdf-file . page-num) or nil if it doesn't match."
  (when (string-match "\\(.*\\.pdf\\)::\\([0-9]+\\)$" path)
    (cons (match-string 1 path)
          (string-to-number (match-string 2 path)))))

(defun org-pdf-slide-show--resolve-pdf (pdf-path)
  "Get absolute path for PDF-PATH relative to current buffer."
  (expand-file-name pdf-path
                    (file-name-directory
                     (or buffer-file-name default-directory))))

(defun org-pdf-slide-show--image-path (pdf-path page)
  "Figure out where the cached PNG should live for PDF-PATH page PAGE."
  (let* ((abs-pdf (org-pdf-slide-show--resolve-pdf pdf-path))
         (dir (file-name-directory abs-pdf))
         (base (file-name-sans-extension (file-name-nondirectory abs-pdf)))
         (cache-dir (expand-file-name (concat base org-pdf-slide-show-cache-suffix) dir)))
    (expand-file-name (format "page-%d.png" page) cache-dir)))

(defun org-pdf-slide-show--extract-page (pdf-path page output-path)
  "Run pdftoppm to pull PAGE out of PDF-PATH, save as OUTPUT-PATH."
  (let ((abs-pdf (org-pdf-slide-show--resolve-pdf pdf-path))
        (cache-dir (file-name-directory output-path)))
    (unless (file-exists-p abs-pdf)
      (user-error "Can't find PDF: %s" abs-pdf))
    (unless (executable-find org-pdf-slide-show-pdftoppm-executable)
      (user-error "Pdftoppm not found -- install poppler-utils"))
    (unless (file-directory-p cache-dir)
      (make-directory cache-dir t))
    ;; pdftoppm -singlefile dumps to <prefix>.png, so we write to a
    ;; temp name and then rename to the real output path
    (let ((tmp (expand-file-name "slide" cache-dir)))
      (call-process org-pdf-slide-show-pdftoppm-executable
                    nil nil nil
                    "-png" "-r" (number-to-string org-pdf-slide-show-dpi)
                    "-f" (number-to-string page)
                    "-l" (number-to-string page)
                    "-singlefile"
                    abs-pdf tmp)
      (let ((tmp-png (concat tmp ".png")))
        (when (file-exists-p tmp-png)
          (rename-file tmp-png output-path t))))))

(defun org-pdf-slide-show--relative-image-path (pdf-path page)
  "Return relative cache path for PDF-PATH page PAGE.
Like `org-pdf-slide-show--image-path' but relative to the org file's dir."
  (let* ((buf-dir (file-name-directory (or buffer-file-name default-directory)))
         (abs-img (org-pdf-slide-show--image-path pdf-path page)))
    (file-relative-name abs-img buf-dir)))

(defun org-pdf-slide-show--collect-links ()
  "Find all pdfslide links in buffer.  Returns list of (beg end pdf page img-path)."
  (let (result)
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (el)
        (when (string= (org-element-property :type el) "pdfslide")
          (let ((parsed (org-pdf-slide-show--parse-link
                         (org-element-property :path el))))
            (when parsed
              (push (list (org-element-property :begin el)
                          (org-element-property :end el)
                          (car parsed) (cdr parsed)
                          (org-pdf-slide-show--image-path (car parsed) (cdr parsed)))
                    result))))))
    (nreverse result)))

(defun org-pdf-slide-show--ensure-images ()
  "Go through buffer, extract any slide images we don't have yet."
  (let ((n 0))
    (dolist (link (org-pdf-slide-show--collect-links))
      (let ((pdf (nth 2 link))
            (pg (nth 3 link))
            (img (nth 4 link)))
        (unless (file-exists-p img)
          (condition-case err
              (progn
                (org-pdf-slide-show--extract-page pdf pg img)
                (cl-incf n))
            (error
             (message "org-pdf-slide-show: couldn't extract %s p%d: %s"
                      pdf pg (error-message-string err)))))))
    (when (> n 0)
      (message "org-pdf-slide-show: extracted %d image(s)" n))))

(defun org-pdf-slide-show--companion-re (rel-path)
  "Build a regexp that matches the auto-generated companion line for REL-PATH."
  (concat "^\\[\\[file:" (regexp-quote rel-path) "\\]\\]$"))

(defun org-pdf-slide-show--update-companion-links ()
  "Insert or update [[file:...]] links below each pdfslide link.
Works bottom-to-top so insertions don't mess up earlier positions.
This is what makes the images show up on GitHub and other org renderers."
  (let ((links (reverse (org-pdf-slide-show--collect-links)))
        (modified nil))
    (save-excursion
      (dolist (link links)
        (let* ((end (nth 1 link))
               (pdf (nth 2 link))
               (pg (nth 3 link))
               (img (nth 4 link))
               (rel (org-pdf-slide-show--relative-image-path pdf pg))
               (companion (format "[[file:%s]]" rel)))
          (when (file-exists-p img)
            (goto-char end)
            ;; skip trailing whitespace on the same line
            (skip-chars-forward " \t")
            ;; check if next line already has our companion link
            (let ((next-line-beg (line-beginning-position 2))
                  (next-line-end (line-end-position 2)))
              (if (and (< next-line-beg (point-max))
                       (string-match-p
                        (org-pdf-slide-show--companion-re rel)
                        (buffer-substring-no-properties next-line-beg next-line-end)))
                  ;; already there, nothing to do
                  nil
                ;; check if next line is a stale companion (file link to our cache dir)
                ;; if so, replace it instead of adding a duplicate
                (let* ((next-text (buffer-substring-no-properties next-line-beg next-line-end))
                       (stale (string-match-p "^\\[\\[file:.*-slides/page-[0-9]+\\.png\\]\\]$" next-text)))
                  (when stale
                    (delete-region next-line-beg (min (1+ next-line-end) (point-max))))
                  ;; insert at end of the pdfslide link line
                  (goto-char end)
                  (end-of-line)
                  (insert "\n" companion)
                  (setq modified t))))))))
    (when modified
      (message "org-pdf-slide-show: updated companion image links"))))

(defun org-pdf-slide-show--remove-overlays ()
  "Clear our overlays."
  (remove-overlays (point-min) (point-max) 'org-pdf-slide-show t))

(defun org-pdf-slide-show--display-overlays ()
  "Put image overlays on all pdfslide links that have cached images."
  (org-pdf-slide-show--remove-overlays)
  (dolist (link (org-pdf-slide-show--collect-links))
    (let ((beg (nth 0 link))
          (end (nth 1 link))
          (img (nth 4 link)))
      (when (file-exists-p img)
        (let ((ov (make-overlay beg end)))
          (overlay-put ov 'org-pdf-slide-show t)
          (overlay-put ov 'display
                       (create-image img 'png nil
                                     :width org-pdf-slide-show-image-width))
          (overlay-put ov 'face 'default)
          (overlay-put ov 'help-echo img))))))

(defun org-pdf-slide-show--before-save ()
  "Hook for before-save: extract images and update companion links.
The companion [[file:...]] links are what make images visible on GitHub."
  (when org-pdf-slide-show-mode
    (org-pdf-slide-show--ensure-images)
    (org-pdf-slide-show--update-companion-links)))

(defun org-pdf-slide-show--after-save ()
  "Hook for after-save: refresh overlays."
  (when org-pdf-slide-show-mode
    (org-pdf-slide-show--display-overlays)))

;;;###autoload
(defun org-pdf-slide-show-refresh ()
  "Extract any missing slide images and redisplay overlays."
  (interactive)
  (org-pdf-slide-show--ensure-images)
  (org-pdf-slide-show--display-overlays)
  (message "org-pdf-slide-show: refreshed"))

;;;###autoload
(defun org-pdf-slide-show-clear ()
  "Remove all slide image overlays."
  (interactive)
  (org-pdf-slide-show--remove-overlays)
  (message "org-pdf-slide-show: overlays cleared"))

;; -- link type --

(defun org-pdf-slide-show--follow (path _)
  "Follow a pdfslide link at PATH, opening the PDF at the right page."
  (let ((parsed (org-pdf-slide-show--parse-link path)))
    (if parsed
        (let ((abs-pdf (org-pdf-slide-show--resolve-pdf (car parsed))))
          ;; prefer pdf-tools if available, fall back to doc-view
          (if (and (featurep 'pdf-tools) (fboundp 'pdf-view-goto-page))
              (progn (find-file abs-pdf)
                     (pdf-view-goto-page (cdr parsed)))
            (find-file abs-pdf)
            (when (derived-mode-p 'doc-view-mode)
              (doc-view-goto-page (cdr parsed)))))
      (user-error "Bad pdfslide link: %s" path))))

(defun org-pdf-slide-show--export (path desc backend _info)
  "Export pdfslide link at PATH with DESC for BACKEND as an image."
  (let* ((parsed (org-pdf-slide-show--parse-link path))
         (img (when parsed
                (org-pdf-slide-show--image-path (car parsed) (cdr parsed))))
         (alt (or desc (format "Page %d" (cdr parsed)))))
    (when (and img (file-exists-p img))
      (pcase backend
        ('html (format "<img src=\"%s\" alt=\"%s\" />" img alt))
        ('latex (format "\\includegraphics{%s}" img))
        (_ nil)))))

(org-link-set-parameters "pdfslide"
                         :follow #'org-pdf-slide-show--follow
                         :export #'org-pdf-slide-show--export)

;; -- minor mode --

;;;###autoload
(define-minor-mode org-pdf-slide-show-mode
  "Toggle inline display of pdfslide links in the current org buffer."
  :lighter " PdfSlide"
  :group 'org-pdf-slide-show
  (if org-pdf-slide-show-mode
      (progn
        (add-hook 'before-save-hook #'org-pdf-slide-show--before-save nil t)
        (add-hook 'after-save-hook #'org-pdf-slide-show--after-save nil t)
        (org-pdf-slide-show--display-overlays))
    (remove-hook 'before-save-hook #'org-pdf-slide-show--before-save t)
    (remove-hook 'after-save-hook #'org-pdf-slide-show--after-save t)
    (org-pdf-slide-show--remove-overlays)))

(provide 'org-pdf-slide-show)
;;; org-pdf-slide-show.el ends here
