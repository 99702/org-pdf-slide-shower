;;; org-pdf-slide-shower.el --- Show PDF slides inline in org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Rajan
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.0"))
;; Keywords: org, pdf, slides
;; URL: https://github.com/rajan/org-pdf-slide-shower

;; This file is not part of GNU Emacs.

;; GPLv3+

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
;;   (require 'org-pdf-slide-shower)
;;   (add-hook 'org-mode-hook #'org-pdf-slide-shower-mode)

;;; Code:

(require 'org)
(require 'image)

(defgroup org-pdf-slide-shower nil
  "Show PDF slides inline in org-mode."
  :group 'org
  :prefix "org-pdf-slide-shower-")

(defcustom org-pdf-slide-shower-dpi 150
  "DPI used when extracting pages from PDFs."
  :type 'integer
  :group 'org-pdf-slide-shower)

(defcustom org-pdf-slide-shower-image-width 600
  "Width (px) for the inline image display."
  :type 'integer
  :group 'org-pdf-slide-shower)

(defcustom org-pdf-slide-shower-cache-suffix "-slides"
  "Suffix for the image cache directory.
E.g. for intro.pdf -> intro-slides/"
  :type 'string
  :group 'org-pdf-slide-shower)

(defcustom org-pdf-slide-shower-pdftoppm-executable "pdftoppm"
  "Path or name of the pdftoppm binary."
  :type 'string
  :group 'org-pdf-slide-shower)

;; ---- internal stuff ----

(defun org-pdf-slide-shower--parse-link (path)
  "Split PATH into (pdf-file . page-num) or nil if it doesn't match."
  (when (string-match "\\(.*\\.pdf\\)::\\([0-9]+\\)$" path)
    (cons (match-string 1 path)
          (string-to-number (match-string 2 path)))))

(defun org-pdf-slide-shower--resolve-pdf (pdf-path)
  "Get absolute path for PDF-PATH relative to current buffer."
  (expand-file-name pdf-path
                    (file-name-directory
                     (or buffer-file-name default-directory))))

(defun org-pdf-slide-shower--image-path (pdf-path page)
  "Figure out where the cached PNG should live for PDF-PATH page PAGE."
  (let* ((abs-pdf (org-pdf-slide-shower--resolve-pdf pdf-path))
         (dir (file-name-directory abs-pdf))
         (base (file-name-sans-extension (file-name-nondirectory abs-pdf)))
         (cache-dir (expand-file-name (concat base org-pdf-slide-shower-cache-suffix) dir)))
    (expand-file-name (format "page-%d.png" page) cache-dir)))

(defun org-pdf-slide-shower--extract-page (pdf-path page output-path)
  "Run pdftoppm to pull PAGE out of PDF-PATH, save as OUTPUT-PATH."
  (let ((abs-pdf (org-pdf-slide-shower--resolve-pdf pdf-path))
        (cache-dir (file-name-directory output-path)))
    (unless (file-exists-p abs-pdf)
      (user-error "Can't find PDF: %s" abs-pdf))
    (unless (executable-find org-pdf-slide-shower-pdftoppm-executable)
      (user-error "pdftoppm not found -- install poppler-utils"))
    (unless (file-directory-p cache-dir)
      (make-directory cache-dir t))
    ;; pdftoppm -singlefile dumps to <prefix>.png, so we write to a
    ;; temp name and then rename to the real output path
    (let ((tmp (expand-file-name "slide" cache-dir)))
      (call-process org-pdf-slide-shower-pdftoppm-executable
                    nil nil nil
                    "-png" "-r" (number-to-string org-pdf-slide-shower-dpi)
                    "-f" (number-to-string page)
                    "-l" (number-to-string page)
                    "-singlefile"
                    abs-pdf tmp)
      (let ((tmp-png (concat tmp ".png")))
        (when (file-exists-p tmp-png)
          (rename-file tmp-png output-path t))))))

(defun org-pdf-slide-shower--collect-links ()
  "Find all pdfslide links in buffer.  Returns list of (beg end pdf page img-path)."
  (let (result)
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (el)
        (when (string= (org-element-property :type el) "pdfslide")
          (let ((parsed (org-pdf-slide-shower--parse-link
                         (org-element-property :path el))))
            (when parsed
              (push (list (org-element-property :begin el)
                          (org-element-property :end el)
                          (car parsed) (cdr parsed)
                          (org-pdf-slide-shower--image-path (car parsed) (cdr parsed)))
                    result))))))
    (nreverse result)))

(defun org-pdf-slide-shower--ensure-images ()
  "Go through buffer, extract any slide images we don't have yet."
  (let ((n 0))
    (dolist (link (org-pdf-slide-shower--collect-links))
      (let ((pdf (nth 2 link))
            (pg (nth 3 link))
            (img (nth 4 link)))
        (unless (file-exists-p img)
          (condition-case err
              (progn
                (org-pdf-slide-shower--extract-page pdf pg img)
                (cl-incf n))
            (error
             (message "org-pdf-slide-shower: couldn't extract %s p%d: %s"
                      pdf pg (error-message-string err)))))))
    (when (> n 0)
      (message "org-pdf-slide-shower: extracted %d image(s)" n))))

(defun org-pdf-slide-shower--remove-overlays ()
  "Clear our overlays."
  (remove-overlays (point-min) (point-max) 'org-pdf-slide-shower t))

(defun org-pdf-slide-shower--display-overlays ()
  "Put image overlays on all pdfslide links that have cached images."
  (org-pdf-slide-shower--remove-overlays)
  (dolist (link (org-pdf-slide-shower--collect-links))
    (let ((beg (nth 0 link))
          (end (nth 1 link))
          (img (nth 4 link)))
      (when (file-exists-p img)
        (let ((ov (make-overlay beg end)))
          (overlay-put ov 'org-pdf-slide-shower t)
          (overlay-put ov 'display
                       (create-image img 'png nil
                                     :width org-pdf-slide-shower-image-width))
          (overlay-put ov 'face 'default)
          (overlay-put ov 'help-echo img))))))

(defun org-pdf-slide-shower--after-save ()
  "Hook for after-save: extract + display."
  (when org-pdf-slide-shower-mode
    (org-pdf-slide-shower--ensure-images)
    (org-pdf-slide-shower--display-overlays)))

;;;###autoload
(defun org-pdf-slide-shower-refresh ()
  "Extract any missing slide images and redisplay overlays."
  (interactive)
  (org-pdf-slide-shower--ensure-images)
  (org-pdf-slide-shower--display-overlays)
  (message "org-pdf-slide-shower: refreshed"))

;;;###autoload
(defun org-pdf-slide-shower-clear ()
  "Remove all slide image overlays."
  (interactive)
  (org-pdf-slide-shower--remove-overlays)
  (message "org-pdf-slide-shower: overlays cleared"))

;; -- link type --

(defun org-pdf-slide-shower--follow (path _)
  "Open the PDF at the right page when you click the link."
  (let ((parsed (org-pdf-slide-shower--parse-link path)))
    (if parsed
        (let ((abs-pdf (org-pdf-slide-shower--resolve-pdf (car parsed))))
          ;; prefer pdf-tools if available, fall back to doc-view
          (if (and (featurep 'pdf-tools) (fboundp 'pdf-view-goto-page))
              (progn (find-file abs-pdf)
                     (pdf-view-goto-page (cdr parsed)))
            (find-file abs-pdf)
            (when (derived-mode-p 'doc-view-mode)
              (doc-view-goto-page (cdr parsed)))))
      (user-error "Bad pdfslide link: %s" path))))

(defun org-pdf-slide-shower--export (path desc backend _info)
  "Export handler -- turns pdfslide links into images for html/latex."
  (let* ((parsed (org-pdf-slide-shower--parse-link path))
         (img (when parsed
                (org-pdf-slide-shower--image-path (car parsed) (cdr parsed))))
         (alt (or desc (format "Page %d" (cdr parsed)))))
    (when (and img (file-exists-p img))
      (pcase backend
        ('html (format "<img src=\"%s\" alt=\"%s\" />" img alt))
        ('latex (format "\\includegraphics{%s}" img))
        (_ nil)))))

(org-link-set-parameters "pdfslide"
                         :follow #'org-pdf-slide-shower--follow
                         :export #'org-pdf-slide-shower--export)

;; -- minor mode --

;;;###autoload
(define-minor-mode org-pdf-slide-shower-mode
  "Toggle inline display of pdfslide links in the current org buffer."
  :lighter " PdfSlide"
  :group 'org-pdf-slide-shower
  (if org-pdf-slide-shower-mode
      (progn
        (add-hook 'after-save-hook #'org-pdf-slide-shower--after-save nil t)
        (org-pdf-slide-shower--display-overlays))
    (remove-hook 'after-save-hook #'org-pdf-slide-shower--after-save t)
    (org-pdf-slide-shower--remove-overlays)))

(provide 'org-pdf-slide-shower)
;;; org-pdf-slide-shower.el ends here
