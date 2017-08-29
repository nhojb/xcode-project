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
;; - Extract information about targets, build configs, build settings and files etc.
;;
;; Usage:
;;
;; (xcode-project-read PATH-TO-XCODEPROJ)

;;; Code:

(require 'xcode-parser)

;; Parameters

;; Private

(defun xcode-project--objects (project)
  "Return all objects for the specified PROJECT.
Private function."
  (alist-get 'objects project))

(defun xcode-project--objects-isa (project isa &optional keep-ref)
  "Return all objects found in PROJECT of type ISA.

By default the object reference is discarded (the reference is
usually contained in the value alist), but if you want it included
specify KEEP-REF t.

Private function."
  (let (results)
    (dolist (obj (xcode-project--objects project) results)
      (if (equal (alist-get 'isa obj) isa)
          (let ((result (if keep-ref obj
                          ;; drop the reference from the result
                          (cdr obj))))
            (if results
                (setcdr (last results) (list result))
              (setq results (list result))))))))

(defun xcode-project--object-ref (project ref)
  "Return the object in PROJECT matching REF.
Private function."
  (if (stringp ref)
      (setq ref (intern-soft ref)))
  (unless (symbolp ref)
    (error "Object ref must be a symbol"))
  (seq-some (lambda (obj) (if (eq (car obj) ref) obj))
            (xcode-project--objects project)))

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

;; Targets

(defun xcode-project-targets (project &optional key value)
  "Return a list of targets (alist) found for the specified PROJECT object.
Targets are filtered according to the optional KEY VALUE."
  (when-let (targets (xcode-project--objects-isa project "PBXNativeTarget"))
    (if key
        (progn
          (unless (symbolp key)
            (error "Invalid key (should be symbol): %s" key))
          (unless value
            (message "Value not specified"))
          (seq-filter (lambda (target) (equal (alist-get key target) value))
                      targets))
      targets)))

(defun xcode-project-target-names (project)
  "Return a list of target names found for the specified PROJECT object."
  (nreverse (seq-map (lambda (target) (alist-get 'name target))
                     (xcode-project-targets project))))

(defun xcode-target-name (target)
  "Return the name of the specified TARGET."
  (alist-get 'name target))

(defun xcode-target-product-name (target)
  "Return the product name of the specified TARGET."
  (alist-get 'productName target))

(defun xcode-target-type (target)
  "Return the type of the specified TARGET."
  (alist-get 'productType target))

(defun xcode-target-ref (target)
  "Return the reference of the specified TARGET."
  (alist-get 'productReference target))

;; Build Configurations

(defun xcode-project--root-build-configs (project &optional name)
  "Return a list of top level (root) build configurations found in PROJECT.
Optionally filter by configuration NAME.
Private function."
  (let* ((pbxproj (car (xcode-project--objects-isa project "PBXProject")))
         (config-list (xcode-project--object-ref project (alist-get 'buildConfigurationList pbxproj))))
    (if name
        (seq-some (lambda (ref)
                    (let ((config (xcode-project--object-ref project ref)))
                      (if (equal (alist-get 'name config) name)
                          ;; remove config ref (car)
                          (cdr config))))
                  (alist-get 'buildConfigurations config-list))
      (seq-map (lambda (ref)
                 ;; remove config ref (car)
                 (cdr (xcode-project--object-ref project ref)))
               (alist-get 'buildConfigurations config-list)))))

(defun xcode-project-build-config (project name target-name)
  "Return the build configuration in PROJECT matching NAME for TARGET-NAME."
  (let* ((target (car (xcode-project-targets project 'name target-name)))
         (build-config-list (xcode-project--object-ref project (alist-get 'buildConfigurationList target)))
         (target-build-config
          (seq-some (lambda (ref) (let ((config (xcode-project--object-ref project ref)))
                                    (if (equal (alist-get 'name config) name)
                                        ;; copy to avoid mutating the original
                                        (copy-alist config))))
                    (alist-get 'buildConfigurations build-config-list)))
         (root-settings (alist-get 'buildSettings (xcode-project--root-build-configs project name))))
    (when target-build-config
        ;; merge root build settings
        (setf (alist-get 'buildSettings target-build-config)
              (append (alist-get 'buildSettings target-build-config) root-settings))
        target-build-config)))

(defun xcode-project-build-config-names (project)
  "Return a list of build configurations found in PROJECT.
Optionally filtered by NAME."
  (seq-map (lambda (config) (alist-get 'name config))
           (xcode-project--root-build-configs project)))

(defun xcode-build-config-name (config)
  "Return name for the build CONFIG."
  (alist-get 'name config))

(defun xcode-build-config-setings (config)
  "Return build settings for the build CONFIG."
  (alist-get 'buildSettings config))

;; Files


;; Build Phases


(provide 'xcode-project)
;;; xcode-project.el ends here
