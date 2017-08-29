;;; xcode-project.el --- A package for reading Xcode project files.

;; Copyright (c) 2017 Olive Toast Software Ltd.

;; Author: John Buckley <john@olivetoast.com>
;; URL: https://github.com/nhojb/xcode-project.git
;; Version: 1.0
;; Keywords: languages, tools
;; Package-Requires: ((emacs "24.4") (json "1.0"))

;; This file is not part of GNU Emacs.

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

(require 'xcode-parser)
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
        (xcode-parser-read-file (expand-file-name pbxproj-path)))))

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
