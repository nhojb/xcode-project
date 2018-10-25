;;; xcode-parser-test.el --- Xcode Parser: Test cases

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

;; Test cases for xcode-parser

;;; Code:

(require 'ert)
(require 'xcode-parser)

(message "Running xcode-parser tests on Emacs %s" emacs-version)

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

(defmacro xcode-parser-measure-ntimes (ntimes &rest body)
  "Measure the average time it takes to evaluate BODY over NTIMES.
Optionaly perform body NTIMES."
  `(let ((time (current-time))
         (n (or ,ntimes 1)))
     (dotimes (i n)
       ,@body)
     (message "elapsed: %.06f" (/ (float-time (time-since time)) n))))

(defmacro xcode-parser-measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "elapsed: %.06f" (float-time (time-since time)))))

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
    ;; leading period is not a number when followed by non-digit
    (xcode-parser-test-should-equal ".git" ".git" fun)
    (xcode-parser-test-should-equal ".E" ".E" fun)
    ;; with terminating chars
    (xcode-parser-test-should-equal "1," 1 fun)
    (xcode-parser-test-should-equal "2.3;" 2.3 fun)
    (xcode-parser-test-should-equal "-3.73e9)" -3.73e9 fun)
    (xcode-parser-test-should-equal "4002.76}" 4002.76 fun)
    ;; fileRefs e.g. 523682672064504100762896 should always be interpreted as strings
    (xcode-parser-test-should-equal "523682672064504100762896" "523682672064504100762896" fun)
    ;; But a number with 23 sig digits should still be a number
    (xcode-parser-test-should-equal "52368267206450410076289" 52368267206450410076289 fun)
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

;; Performance

(ert-deftest xcode-parser-test-performance ()
  (xcode-parser-measure-ntimes 5
   (xcode-parser-read-file "~/Projects/nhojb/xcode-project/test/simple.plist"))

  (xcode-parser-measure-ntimes 5
   (xcode-parser-read-file "~/Projects/nhojb/xcode-project/test/project.pbxproj"))

  (xcode-parser-measure-ntimes 5
   (xcode-parser-read-file "~/Projects/nhojb/xcode-project/test/medium.pbxproj"))

  (xcode-parser-measure-ntimes 5
   (xcode-parser-read-file "~/Projects/nhojb/xcode-project/test/large.pbxproj"))
  )

(provide 'xcode-parser-test)

;;; xcode-parser-test.el ends here
