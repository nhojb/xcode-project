;;; xcode-parser-test.el --- Xcode Parser: Test cases

;; Copyright (c) 2017 Olive Toast Software Ltd.

;; Author: John Buckley <john@olivetoast.com>
;; URL: https://github.com/nhojb/xcode-project

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

;; Test cases Xcode Parser

;;; Code:

(require 'ert)
(require 'xcode-parser)
(require 'xcode-project)

(message "Running tests on Emacs %s" emacs-version)

;; (defconst xcode-parser-test-directory
;;   (let ((filename (if load-in-progress load-file-name (buffer-file-name))))
;;     (expand-file-name "test/" (locate-dominating-file filename "Cask")))
;;   "Test suite directory, for resource loading.")

;; Utilities

(defun xcode-parser-test-should-equal (input expected fun)
  "Test that when passed INPUT the output of function FUN matches the EXPECTED value."
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (should (equal (funcall fun) expected))))

(defun xcode-parser-test-should-not (input fun)
  "Test that when passed INPUT the output of function FUN is nil."
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (should-not (funcall fun))))

(defun xcode-parser-test-should-error (input fun)
  "Test that when passed INPUT function FUN reports an error."
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (should-error (funcall fun))))

(defmacro xcode-parser-measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

;; Strings

(ert-deftest xcode-parser-test-quoted-string ()
  "Tests reading quoted strings."
  (let ((fun 'xcode-parser-read-quoted-string))
    (xcode-parser-test-should-equal "\"foo\"" "foo" fun)
    (xcode-parser-test-should-equal "\"foo with spaces\"" "foo with spaces" fun)
    (xcode-parser-test-should-equal "\"foo with spaces\nand newlines.\"" "foo with spaces\nand newlines." fun)
    (xcode-parser-test-should-equal "'single quoted.'" "single quoted." fun)
    (xcode-parser-test-should-equal "\"embedded single quote's are ok\"" "embedded single quote's are ok" fun)
    (xcode-parser-test-should-equal "\"double quotes should be escaped \\\"\"" "double quotes should be escaped \"" fun)
    (xcode-parser-test-should-equal "\"foo with \\t escape char.\"" "foo with \t escape char." fun)
    (xcode-parser-test-should-equal "\"\"" "" fun) ; empty string
    (xcode-parser-test-should-error "\"foo" fun) ; unescaped quote
    (xcode-parser-test-should-error "no quotes" fun)
  ))

(ert-deftest xcode-parser-test-unquoted-string ()
  "Tests reading unquoted strings."
  (let ((fun 'xcode-parser-read-unquoted-string))
    (xcode-parser-test-should-equal "foo" "foo" fun)
    (xcode-parser-test-should-equal "foo with spaces" "foo" fun)
    (xcode-parser-test-should-equal "foo with spaces\nand newlines." "foo" fun)
    (xcode-parser-test-should-equal "foo-symbols:-and-12345 should also work" "foo-symbols:-and-12345" fun)
    (xcode-parser-test-should-equal "5ABCED" "5ABCED" fun)
    (xcode-parser-test-should-error "" fun)
    ;; Special chars should be ignored
    (let ((special xcode-parser-special-chars))
      (while special
        (let ((schar (string (car special))))
          (xcode-parser-test-should-equal (format "foo%sbar" schar schar) "foo" fun)
          (xcode-parser-test-should-error (format "%sfoobar" schar schar) fun)
          (setq special (cdr special)))))
  ))

;; Numbers

(ert-deftest xcode-parser-test-number ()
  "Tests reading numbers."
  (let ((fun 'xcode-parser-read-number))
    (xcode-parser-test-should-equal "9" 9 fun)
    (xcode-parser-test-should-equal "-2" -2 fun)
    (xcode-parser-test-should-equal "+3" 3 fun)
    (xcode-parser-test-should-equal "1.0" 1.0 fun)
    (xcode-parser-test-should-equal "2." 2 fun)
    (xcode-parser-test-should-equal ".5" 0.5 fun)
    (xcode-parser-test-should-equal "-51.3" -51.3 fun)
    (xcode-parser-test-should-equal "10.7e3" 10.7e3 fun)
    (xcode-parser-test-should-equal "23.06E-3" 23.06e-3 fun)
    ;; unquoted string starting with a numeric digit
    (xcode-parser-test-should-equal "5ABCED" "5ABCED" fun)
    ;; more than one period is not a valid number
    (xcode-parser-test-should-equal "8.3.3" "8.3.3" fun)
    ;; with terminating chars
    (xcode-parser-test-should-equal "1," 1 fun)
    (xcode-parser-test-should-equal "2.3;" 2.3 fun)
    (xcode-parser-test-should-equal "-3.73e9)" -3.73e9 fun)
    (xcode-parser-test-should-equal "4002.76}" 4002.76 fun)
    ))

;; Arrays

(ert-deftest xcode-parser-test-array ()
  "Tests reading arrays."
  (let ((fun 'xcode-parser-read-array))
    (xcode-parser-test-should-equal "()" [] fun)
    (xcode-parser-test-should-equal "(\"quoted\")" ["quoted"] fun)
    (xcode-parser-test-should-equal "(unquoted)" ["unquoted"] fun)
    (xcode-parser-test-should-equal "(1, 2, 3, 4, 5)" [1 2 3 4 5] fun)
    (xcode-parser-test-should-equal "(1, \"foo\", 'bar', frog, 35.4)" [1 "foo" "bar" "frog" 35.4] fun)
    (xcode-parser-test-should-equal "(nested, (arrays, 2), also, ok)" ["nested" ["arrays" 2] "also" "ok"] fun)

    (xcode-parser-test-should-equal "(52ABCE, 67BC, 54)" ["52ABCE" "67BC" 54] fun)

    ;; Trailing comma should be ok
    (xcode-parser-test-should-equal "(1, 2, 3, 4,)" [1 2 3 4] fun)

    ;; Whitespace
    (xcode-parser-test-should-equal "(1, \n  2  ,\n3, 4\n,\n5)" [1 2 3 4 5] fun)
    (xcode-parser-test-should-equal "(\n1AND,2AND,\n3AND,\n4AND,\n5AND\n)" ["1AND" "2AND" "3AND" "4AND" "5AND"] fun)

    ;; Special chars should fail in unquoted strings:
    (xcode-parser-test-should-error "(;unquoted)" fun)
    (xcode-parser-test-should-error "(unq=uoted)" fun)
    (xcode-parser-test-should-error "(unquoted')" fun)

    ;; But they're ok in quoted strings
    (xcode-parser-test-should-equal "(\";quoted\")" [";quoted"] fun)
    (xcode-parser-test-should-equal "(\"q=uoted\")" ["q=uoted"] fun)
    (xcode-parser-test-should-equal "(\"quoted'\")" ["quoted'"] fun)

    ;; Comments
    (xcode-parser-test-should-equal "(/* inline comment */)" [] fun)
    (xcode-parser-test-should-equal "(1, /* inline comment */ 2, 3 /* again */ ,4)" [1 2 3 4] fun)
    (xcode-parser-test-should-equal "(1, /* inline comment */ 2, 3 ,4) // suffix comment" [1 2 3 4] fun)
    (xcode-parser-test-should-equal "(\n1AND /* comment */,\t2AND /* comment */,\n\t3AND /* comment */,\n\t4AND /* comment */,\n\t5AND /* comment */\n)" ["1AND" "2AND" "3AND" "4AND" "5AND"] fun)

    ;; Errors
    (xcode-parser-test-should-error "(" fun)
    (xcode-parser-test-should-error ")" fun)
    (xcode-parser-test-should-error "(,)" fun)
    (xcode-parser-test-should-error "(1 2 3 4)" fun)
    (xcode-parser-test-should-error "(\"1, 2, 3, 4)" fun)
    ))

;; Dictionaries

(ert-deftest xcode-parser-test-dict()
  "Tests reading dictionaries."
  (let ((fun 'xcode-parser-read-dict))
    (xcode-parser-test-should-equal "{}" '() fun)
    (xcode-parser-test-should-equal "{foo=bar;}" '((foo . "bar")) fun)
    (xcode-parser-test-should-equal "{foo = bar; count = 2;}" '((count . 2) (foo . "bar")) fun)

    (xcode-parser-test-should-equal "{foo = bar; numbers = (1, 2, 3, 4);}" '((numbers . [1 2 3 4]) (foo . "bar")) fun)

    (xcode-parser-test-should-equal "{foo = bar; info = {this=that; night=day;}; }" '((info . ((night . "day") (this . "that"))) (foo . "bar")) fun)

    (xcode-parser-test-should-equal "{5APPLES = nice; oranges=64perDay;}; }" '((oranges . "64perDay") (5APPLES . "nice")) fun)

    ;; Whitespace
    (xcode-parser-test-should-equal "{one = 1;\ntwo = 2;\nthree\n = \n 3;}" '((three . 3) (two . 2) (one . 1)) fun)

    ;; Comments

    (xcode-parser-test-should-equal "{foo /* inline comment */ = bar;}" '((foo . "bar")) fun)
    (xcode-parser-test-should-equal "{foo /* inline comment */ = bar /* second comment */; }" '((foo . "bar")) fun)
    (xcode-parser-test-should-equal "{foo /* inline comment */ = bar; } // trailing comment" '((foo . "bar")) fun)
    
    ;; Errors
    (xcode-parser-test-should-error "{" fun)
    (xcode-parser-test-should-error "}" fun)
    (xcode-parser-test-should-error "{foo = 'missing semi-colon'}" fun)
    (xcode-parser-test-should-error "{foo 'missing equals';}" fun)
    (xcode-parser-test-should-error "{foo == bad;}" fun)
    (xcode-parser-test-should-error "{foo = bad, commas = 'not valid'}" fun)
    ))

;; Test files

(xcode-parser-measure-time
 (xcode-parser-read-file "~/Projects/nhojb/xcode-project/test/simple.plist"))

(xcode-parser-measure-time
 (xcode-parser-read-file "~/Projects/nhojb/xcode-project/test/project.pbxproj"))

(xcode-parser-measure-time
 (xcode-parser-read-file "~/Projects/OliveToast/Files/2.0/Files.xcodeproj/project.pbxproj"))

(xcode-parser-measure-time
 (xcode-project-read "~/Projects/nhojb/MetalTest/MetalTest.xcodeproj"))

(xcode-parser-measure-time
 (xcode-project-read "~/Projects/OliveToast/Files/2.0/Files.xcodeproj"))

;; Comments

(ert-deftest xcode-parser-test-comment ()
  "Tests skipping comments."
  (with-temp-buffer
    (insert "/* a c-style comment */")
    (goto-char (point-min))
    (should (equal (xcode-parser-skip-comment) (point-max)))

    (erase-buffer)

    (insert "/* a c-style comment */ FOO = BAR;")
    (goto-char (point-min))
    (should (< (xcode-parser-skip-comment) (point-max)))

    (erase-buffer)

    (insert "/* a c-style comment\ncan be multi-line */")
    (goto-char (point-min))
    (should (equal (xcode-parser-skip-comment) (point-max)))

    (erase-buffer)

    (insert "/* match should be non-greedy */ FOO = BAR /* second comment */")
    (goto-char (point-min))
    (should (< (xcode-parser-skip-comment) (point-max)))

    (erase-buffer)

    (insert "// line comment")
    (goto-char (point-min))
    (should (equal (xcode-parser-skip-comment) (point-max)))

    (erase-buffer)

    (insert "FOO = BAR; // line comment")
    (goto-char (point-min))
    (skip-chars-forward "^/")
    (should (equal (xcode-parser-skip-comment) (point-max)))

    (erase-buffer)

    ;; invalid
    (insert "/* a c-style comment /")
    (goto-char (point-min))
    (should-error (xcode-parser-skip-comment))

    (erase-buffer)

    ;; single '/' should mark an unquoted string
    (insert "/this/could/be/a/path")
    (goto-char (point-min))
    (should-not (xcode-parser-skip-comment))
    ))

;; Whitespace & Comments

(ert-deftest xcode-parser-test-whitespace ()
  "Tests skipping whitespace and comments."
  (with-temp-buffer
    (insert "  /* a c-style comment */  ")
    (goto-char (point-min))
    (should (equal (progn (xcode-parser-skip-whitespace-or-comment) (point)) (point-max)))

    (erase-buffer)

    (insert "  \n\t /* a c-style comment */ FOO = BAR;")
    (goto-char (point-min))
    (should (< (progn (xcode-parser-skip-whitespace-or-comment) (point)) (point-max)))

    (erase-buffer)

    (insert "KEY = VALUE /* a c-style comment */")
    (goto-char (point-min))
    (should (equal (progn (xcode-parser-skip-whitespace-or-comment) (point)) (point-min)))

    (erase-buffer)

    (insert "  KEY = VALUE /* a c-style comment */")
    (goto-char (point-min))
    (should (progn (xcode-parser-skip-whitespace-or-comment) (and (> (point) (point-min)) (< (point) (point-max)))))

    (erase-buffer)

    (insert "  // trailing comment")
    (goto-char (point-min))
    (should (equal (progn (xcode-parser-skip-whitespace-or-comment) (point)) (point-max)))

    (erase-buffer)

    (insert "FOO = BAR // trailing comment")
    (goto-char (point-min))
    (should (equal (progn (xcode-parser-skip-whitespace-or-comment) (point)) (point-min)))
))

(provide 'xcode-parser-test)

;;; xcode-parser-test.el ends here
