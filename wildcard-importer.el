;;; wildcard-importer.el --- Insert wildcard imports into your files -*- lexical-binding: t -*-

;; Author: Laurence Warne
;; Maintainer: Laurence Warne
;; Version: 0.1
;; Homepage: https://github.com/LaurenceWarne/wildcard-importer.el
;; Package-Requires: ((emacs "27.0"))

;;; Commentary:

;; Insert wildcard imports into your files

;;; Code:

(defgroup wildcard-importer nil
  "Insert wildcard imports into your files."
  :group 'applications)

(defvar-local wildcard-importer-alist
  '((scala-mode
     ("import scala.math.Ordering.Implicits._" . "induced collection orderings")
     ("import scala.concurrent.duration._" . "1.seconds, "))
    (python-mode
     "from collections import Counter")))

(defun wildcard-importer-import ()
  "Prompt for a wildcard import appropriate for the current major mode."
  (interactive)
  (let* ((import-alist (alist-get major-mode wildcard-importer-alist))
         (import-strings (mapcar #'car import-alist))
         (mx-width (seq-max (mapcar #'length import-strings)))
         (completion-extra-properties
          `(:annotation-function
            ,(lambda (m)
               (let ((helper (alist-get m import-alist nil nil #'string=)))
                 (format "%s    %s"
                         (make-string (- mx-width (length m)) ?\s)
                         helper)))))
         (font-lock-kws font-lock-defaults))
    (completing-read
     "Import: "
     (with-temp-buffer
       (let ((font-lock-defaults font-lock-kws))
         (mapcar (lambda (s)
                   (erase-buffer)
                   (insert s)
                   (font-lock-ensure)
                   (buffer-substring (point-min) (point-max)))
                 import-strings))))))

(provide 'wildcard-importer)

;;; wildcard-importer.el ends here
