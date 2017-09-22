;;; xcode-project-test.el --- Xcode Project: Test cases

;; Copyright (c) 2017 Olive Toast Software Ltd.

;; Author: John Buckley <john@olivetoast.com>
;; URL: https://github.com/nhojb/xcode-project

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

;; Test cases for xcode-project

;;; Code:

(require 'ert)
(require 'xcode-project)

(message "Running xcode-project tests on Emacs %s" emacs-version)

(defconst xcode-project-test-directory
  (let ((filename (if load-in-progress load-file-name (buffer-file-name))))
    ;;(expand-file-name "project.pbxproj" (locate-dominating-file filename "project.pbxproj")))
    (expand-file-name (file-name-directory filename)))
  "Test directory.")

(defconst xcode-project-test-project-path
  (concat xcode-project-test-directory "Test.xcodeproj")
  "Test Xcode project path.")

;; Tests

(ert-deftest xcode-project-test-find-xcodeproj ()
  "Test finding the xcode project path."
  ;; directory
  (should (equal (xcode-project-find-xcodeproj xcode-project-test-directory) (concat xcode-project-test-directory "Test.xcodeproj")))
  ;; file
  (should (equal (xcode-project-find-xcodeproj (concat xcode-project-test-directory "xcode-project-test.el")) (concat xcode-project-test-directory "Test.xcodeproj")))
  )

(ert-deftest xcode-project-test-read-project ()
  "Test reading an xcode project."
  (let* ((project-path xcode-project-test-project-path)
         (project (xcode-project-read project-path)))
    (should (file-directory-p project-path))
    (should project)
    (should (equal (alist-get 'xcode-project-path project) project-path))
    (should (alist-get 'objects project))
    (should (eq (alist-get 'objects project) (xcode-project--objects project)))
    (should (xcode-project-read xcode-project-test-directory)) ; contains 'project.pbxproj'
    (should-not (xcode-project-read (concat xcode-project-test-directory "simple.plist")))
    ))

(ert-deftest xcode-project-test-project-path ()
  "Test extracting the project's path."
  (let* ((project-path xcode-project-test-project-path)
         (project (xcode-project-read project-path)))
    (should project)
    (should-not (xcode-project--objects-isa project "PBXFooBar"))
    ))

(ert-deftest xcode-project-test-objects-isa ()
  "Test extracting objects by type."
  (let* ((project (xcode-project-read xcode-project-test-project-path)))
    (should project)
    (should (xcode-project--objects-isa project "PBXNativeTarget"))
    ;; regex
    (should (xcode-project--objects-isa project "PBX.*BuildPhase"))
    ;; with object reference
    (should (eq (car (car (xcode-project--objects-isa project "PBXNativeTarget" t))) '52EB57891F3E0E74008E3850))
    (should-not (xcode-project--objects-isa project "PBXFooBar"))
    ))

(ert-deftest xcode-project-test-objects-ref ()
  "Test extracting objects by reference."
  (let* ((project (xcode-project-read xcode-project-test-project-path))
         ;; get build-file with its object reference
         (build-file (car (xcode-project--objects-isa project "PBXBuildFile" t)))
         (obj-ref (car build-file)))
    (should build-file)
    (should (eq (cdr build-file) (xcode-project--object-ref project obj-ref)))
    (should (eq (cdr build-file) (xcode-project--object-ref project (symbol-name obj-ref))))
    ;; with reference
    (should (equal build-file (xcode-project--object-ref project obj-ref t)))
    (should-not (xcode-project--object-ref project 'foo))
    ))

;; Targets

(ert-deftest xcode-project-test-targets ()
  "Test extracting targets."
  (let* ((project (xcode-project-read xcode-project-test-project-path)))
    (should (xcode-project-targets project))
    (should (xcode-project-targets project 'productType "com.apple.product-type.application"))
    (should (equal (alist-get 'name (car (xcode-project-targets project 'name "MetalTest"))) "MetalTest"))
    (should-not (xcode-project-targets project 'productType "com.apple.product-type.foo"))
    (should (equal (xcode-project-target-names project) '("MetalTest")))
    ))

(ert-deftest xcode-project-test-targets-for-file ()
  "Test extracting targets for a specific file."
  (let* ((project (xcode-project-read xcode-project-test-project-path)))
    (should (xcode-project-targets-for-file project "AppDelegate.swift"))
    (should (equal (xcode-project-target-names-for-file project "AppDelegate.swift") '("MetalTest")))
    (should (equal (xcode-project-target-names-for-file project "AppDelegate.swift" "PBXSourcesBuildPhase") '("MetalTest")))
    (should (equal (xcode-project-target-names-for-file project "Shaders.metal") '("MetalTest")))
    (should (equal (xcode-project-target-names-for-file project "Assets.xcassets") '("MetalTest")))
    (should (equal (xcode-project-target-names-for-file project "Assets.xcassets" "PBXResourcesBuildPhase") '("MetalTest")))
    (should-not (xcode-project-targets-for-file project "Foo.swift"))
    (should-not (xcode-project-target-names-for-file project "Assets.xcassets" "PBXSourcesBuildPhase"))
    ;; localized files don't resolve (currently)
    (should-not (xcode-project-targets-for-file project "Main.storyboard"))
    ))

;; Build Configs

(ert-deftest xcode-project-test-build-configs ()
  "Test extracting build configurations."
  (let* ((project (xcode-project-read xcode-project-test-project-path)))
    (should (xcode-project--root-build-configs project))
    (should (equal (length (xcode-project--root-build-configs project "Debug")) 1))
    (should (equal (length (xcode-project--root-build-configs project "Release")) 1))
    (should (equal (xcode-project-build-config-names project) '("Debug" "Release")))
    (should (equal (alist-get 'name (xcode-project-build-config project "Debug" "MetalTest")) "Debug"))
    (should (equal (alist-get 'name (xcode-project-build-config project "Release" "MetalTest")) "Release"))
    (should-not (xcode-project-build-config project "Foo" "MetalTest"))
    (should-not (xcode-project-build-config project "Debug" "Foo"))
    ))

;; Build Phases

(ert-deftest xcode-project-test-build-phases ()
  "Test extracting build phases."
  (let* ((project (xcode-project-read xcode-project-test-project-path)))
    (should (xcode-project-build-phases project "MetalTest"))
    (should (equal (length (xcode-project-build-phases project "MetalTest")) 3))
    (should (xcode-project-build-phases project "MetalTest" "PBXSourcesBuildPhase"))
    (should (xcode-project-build-phases project "MetalTest" "PBXResourcesBuildPhase"))
    (should (xcode-project-build-phases project "MetalTest" "PBXFrameworksBuildPhase"))
    (should-not (xcode-project-build-phases project "MetalTest" "Foo"))
    (should-not (xcode-project-build-phases project "Foo" "PBXSourcesBuildPhase"))
    ))

;; Build Files

(ert-deftest xcode-project-test-build-files ()
  "Test extracting build files."
  (let* ((project (xcode-project-read xcode-project-test-project-path)))
    (should (xcode-project-build-files project "MetalTest"))
    (should (equal (length (xcode-project-build-files project "MetalTest")) 6))
    (should (equal (length (xcode-project-build-files project "MetalTest" "PBXSourcesBuildPhase")) 4))
    (should (equal (length (xcode-project-build-files project "MetalTest" "PBXSourcesBuildPhase" (lambda (file) (xcode-project-file-ref-extension-p file "swift")))) 3))
    (should (equal (length (xcode-project-build-files project "MetalTest" nil (lambda (file) (xcode-project-file-ref-extension-p file "swift")))) 3))
    (should (equal (length (xcode-project-build-files project "MetalTest" "PBXResourcesBuildPhase")) 2))
    (should (equal (length (xcode-project-build-files project "MetalTest" "PBXResourcesBuildPhase" (lambda (file) (xcode-project-file-ref-extension-p file "storyboard")))) 1))

    (should (equal (length (xcode-project-build-file-paths project "MetalTest")) 6))
    (should (equal (car (xcode-project-build-file-paths project "MetalTest")) "MetalTest/AppDelegate.swift"))
    (should (equal (car (xcode-project-build-file-paths project "MetalTest" nil nil 'absolute)) (concat default-directory "MetalTest/AppDelegate.swift")))
    ))

(ert-deftest xcode-project-test-file-references ()
  "Test finding PBXFileReference objects."
  (let* ((project (xcode-project-read xcode-project-test-project-path)))
    (should (xcode-project-file-references project "AppDelegate.swift"))
    (should (equal (alist-get 'path (car (xcode-project-file-references project "AppDelegate.swift"))) "AppDelegate.swift"))
    (should (equal (alist-get 'path (car (xcode-project-file-references project "Base.lproj/Main.storyboard"))) "Base.lproj/Main.storyboard"))
    ;; file-name heuristics
    (should (equal (alist-get 'path (car (xcode-project-file-references project "Main.storyboard"))) "Base.lproj/Main.storyboard"))
    ;; absolute paths
    (should (equal (alist-get 'path (car (xcode-project-file-references project (concat default-directory "AppDelegate.swift")))) "AppDelegate.swift"))
    (should-not (xcode-project-file-references project "Foo.swift"))
    ))

(ert-deftest xcode-project-test-serialize ()
  "Test serialization and deserialization."
  (let* ((project (xcode-project-read xcode-project-test-project-path))
         (temp-file (make-temp-file "xcode-project-test")))
    (should (xcode-project-serialize project temp-file))
    (should (equal project (xcode-project-deserialize temp-file)))
    (delete-file temp-file)
    ))

(provide 'xcode-project-test)

;;; xcode-project-test.el ends here
