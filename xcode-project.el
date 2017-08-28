;;; xcode-project.el --- A package for reading Xcode project files.

;; Copyright (c) 2017 Olive Toast Software Ltd.

;; Author: John Buckley <john@olivetoast.com>
;; URL: https://github.com/nhojb/xcode-project.git
;; Version: 1.0
;; Keywords: languages, tools
;; Package-Requires: ((emacs "24.4") (json "1.0"))

;; This file is not part of GNU Emacs.

;; MIT License

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This package supports reading Xcode projects.
;;
;; Features:
;;
;; - Parse Xcode project file (project.pbxproj)
;; - Extract information about targets (settings, files etc.)
;;
;; Usage:
;;
;; (xcode-project-read PATH-TO-XCODEPROJ)

;;; Code:

(require 'json)
(require 'subr-x)

;; Parameters

;; Public

(defun xcode-project-read (xcodeproj-path)
  "Read the Xcode project at XCODEPROJ-PATH.
Returns the parsed Xcode project as a json object, or nil on error."
  (let ((pbxproj-path (if (equal (file-name-nondirectory xcodeproj-path) "project.pbxproj")
                          xcodeproj-path
                        (concat (file-name-as-directory xcodeproj-path) "project.pbxproj")))
        (plutil-path (executable-find "plutil")))
    (if (and (file-exists-p pbxproj-path) plutil-path)
        (when-let (output (shell-command-to-string (format "%s -convert json '%s' -o -" plutil-path (expand-file-name pbxproj-path))))
          ;; ensure we read alist type
          ;;(json-object-type 'alist))
          (json-read-from-string output)))))

(defun xcode-project-find-xcodeproj (directory-or-file)
  "Search DIRECTORY-OR-FILE and parent directories for an Xcode project file.
Returns the path to the Xcode project, or nil if not found."
  (if directory-or-file
      (let (xcodeproj
            (directory (if (file-directory-p directory-or-file)
                           directory-or-file
                         (file-name-directory directory-or-file))))
        (setq directory (expand-file-name directory))
        (while (and (eq xcodeproj nil) (not (equal directory "/")))
          (setq xcodeproj (directory-files directory t ".*\.xcodeproj$"))
          (setq directory (file-name-directory (directory-file-name directory))))
        (car xcodeproj))))

(provide 'xcode-project)
;;; xcode-project.el ends here
