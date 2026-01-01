;;; Humanize functions for elisp

;;; Got labels from https://en.wikipedia.org/wiki/Metric_prefix
;;; Summary is as follows:
;;; =======================
;;; quetta Q   10 pow 30       1000000000000000000000000000000
;;; ronna  R   10 pow 27       1000000000000000000000000000
;;; yotta  Y   10 pow 24       1000000000000000000000000
;;; zetta  Z   10 pow 21       1000000000000000000000
;;; exa    E   10 pow 18       1000000000000000000
;;; peta   P   10 pow 15       1000000000000000
;;; tera   T   10 pow 12       1000000000000
;;; giga   G   10 pow 09       1000000000
;;; mega   M   10 pow 06       1000000
;;; kilo   k   10 pow 03        1000

;; Started out with code by github.com/dustin and modified it. See
;; github.com/dustin/snippets/blob/8801dbba7dd167e6bb14da6a46230d5c25318ec5/lisp/humanize.el
(defun humanize-count (count)
  "Humanize COUNT (a positive integer).  Returns a string
representing the humanized form."
  (let ((base 1000)
        (labels '("" "k" "M" "G" "T" "P" "E" "Z" "Y" "R" "Q")))
    (if (< count base)
        ;; if count is less than the base, just convert it to a string
        (number-to-string count)
      (let ((e (floor (log count base))))
        (if (>= e (length labels))
            ;; if count is larger than known suffixes, just convert it to string
            (number-to-string count)
          (let* ((suffix (nth e labels))
                 (val (/ count (expt base e) 1.0)) ;; the 1.0 is to get a float
                 (f (if (< val 10) "%.1f%s" "%.0f%s")))
            (format f val suffix)))))))

;; Tests, intended more to document examples to understand the behaviour.

(defmacro assert (test-form)
  "Reference: https://emacs.stackexchange.com/a/22082/39298

It is an error (assertion failed) if test-form is not `t'.

Examples:

  (assert t)               ;; okay
  (assert nil)             ;; Assertion failed: nil
  (assert (equal t nil))   ;; Assertion failed: (equal t nil)"
  `(when (not ,test-form)
     (error "Assertion failed: %s" (format "%s" ',test-form))))

(defmacro humanize-assert (&rest pairs)
  `(progn
     ,@(mapcar (lambda (pair)
                 `(assert (equal (humanize-count ,(car pair)) ,(cadr pair))))
               pairs)))

(humanize-assert
 (0 "0")        ;; (humanize 0) => "0"
 (0.0 "0.0")    ;; (humanize 0.0) => "0.0"
 (0.1 "0.1")    ;; (humanize 0.1) => "0.1"
 (-0 "0")
 (-0.0 "-0.0")
 (10 "10")
 (100 "100")
 (999 "999")
 (1000 "1.0k")
 (1049 "1.0k")
 (1050 "1.1k")
 (1500 "1.5k")
 (1550 "1.6k")
 (100000 "100k")
 (1000000 "1.0M")
 (6050000 "6.0M")
 (6051000 "6.1M")
 (10000000 "10M")
 (100000000 "100M")
 (1000000000 "1.0G")
 (10000000000 "10G")
 (100000000000 "100G")
 (1000000000000 "1.0T")
 (10000000000000 "10T")
 (100000000000000  "100T")
 (1000000000000000 "1.0P")
 (10000000000000000 "10P")
 (100000000000000000 "100P")
 (1000000000000000000 "1.0E")
 (10000000000000000000 "10E")
 (100000000000000000000 "100E")
 (1000000000000000000000 "1.0Z")
 (10000000000000000000000 "10Z")
 (100000000000000000000000 "100Z")
 (1000000000000000000000000 "1.0Y")
 (10000000000000000000000000 "10Y")
 (100000000000000000000000000 "100Y")
 (1000000000000000000000000000 "1.0R")
 (10000000000000000000000000000 "10R")
 (100000000000000000000000000000 "100R")
 (1000000000000000000000000000000 "1.0Q")
 (10000000000000000000000000000000 "10Q")
 (100000000000000000000000000000000 "100Q")
 (1000000000000000000000000000000000 "1000000000000000000000000000000000"))
