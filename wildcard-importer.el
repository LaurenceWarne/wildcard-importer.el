;;; wildcard-importer.el --- Insert wildcard imports into your files -*- lexical-binding: t -*-

;; Author: Laurence Warne
;; Maintainer: Laurence Warne
;; Version: 0.1
;; Homepage: https://github.com/LaurenceWarne/wildcard-importer.el
;; Package-Requires: ((emacs "27.0") (dash "2.17.0"))

;;; Commentary:

;; Insert wildcard imports into your files

;;; Code:

(require 'dash)

(defgroup wildcard-importer nil
  "Insert wildcard imports into your files."
  :group 'applications)

;;;###autoload
(defvar-local wildcard-importer-alist
  '((scala-mode
     ("import scala.math.Ordering.Implicits._" . "induced collection orderings")
     ("import scala.concurrent.duration._" . "1.seconds, comparison ops (for java.util.concurrent.TimeUnit durations.)")
     ("import scala.jdk.CollectionConverters._" . ".asJava .asScala collection conversions"))
    (python-mode
     ("from collections import Counter" . ""))))

;;;###autoload
(defconst wildcard-importer-cats-alist
  '((scala-mode
     ("import cats.implicits._" . "you probably want \"cats.syntax._\"")
     ("import cats.syntax._" . ".some, .asRight, (a, b).mapN, etc"))))

;;;###autoload
(defconst wildcard-importer-cats-strict-alist
  '((scala-mode
     ("import cats.syntax.functor._" . "")
     ("import cats.syntax.foldable._" . ".traverse_, .sliding3")
     ("import cats.syntax.traverse._" . "")
     ("import cats.syntax.option._" . ".some")
     ("import cats.syntax.either._" . ".asRight, .asLeft")
     ("import cats.syntax.apply._" . ".mapN, *>"))))

;;;###autoload
(defconst wildcard-importer-scala3-cats-strict-alist
  '((scala-mode
     ("import cats.syntax.functor.*" . "")
     ("import cats.syntax.foldable.*" . ".traverse_, .sliding3")
     ("import cats.syntax.traverse.*" . "")
     ("import cats.syntax.option.*" . ".some")
     ("import cats.syntax.either.*" . ".asRight, .asLeft")
     ("import cats.syntax.apply.*" . ".mapN, *>"))))

;;;###autoload
(defun wildcard-importer-alist-with-extensions (extensions)
  "Return `wildcard-importer-alist' with EXTENSIONS added."
  (let ((temp wildcard-importer-alist))
    (mapc (lambda (ext)
            (setq temp
                  (--map-when (eq (car it) ext)
                              (append it (alist-get ext extensions))
                              temp)))
          (mapcar #'car extensions))
    temp))

;;;###autoload
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
         (font-lock-kws font-lock-defaults)
         (choice (completing-read
                  "Import: "
                  (with-temp-buffer
                    (let ((font-lock-defaults font-lock-kws))
                      (mapcar (lambda (s)
                                (erase-buffer)
                                (insert s)
                                (font-lock-ensure)
                                (buffer-substring (point-min) (point-max)))
                              import-strings))))))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "\"\"\"" nil t)
        (forward-sexp)
        (forward-line))
      (while (re-search-forward (rx point (or (seq "package" (* nonl) eol)
                                              (seq "import" (* nonl) eol)
                                              eol)) nil t)
        (forward-line)
        (beginning-of-line))
      (forward-line -1)
      (unless (string= (thing-at-point 'line) "\n")
        (forward-line 1))
      (insert choice "\n"))))

(provide 'wildcard-importer)

;;; wildcard-importer.el ends here
