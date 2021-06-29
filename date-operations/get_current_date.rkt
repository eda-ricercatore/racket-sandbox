#lang racket/base

(require srfi/19)

(print "Hello, World!")
(println "Ciao Mondo!!!")



; (let ((date (seconds->date (current-seconds))))
;   (display (date-month month date year)))



;;; The first required format is an ISO-8601 year-month-day format, predefined
;;; as ~1 in date->string
(displayln (date->string (current-date) "~1"))
 
;;; You should be able to see how each of the components of the following format string
;;; work...
;;; ~d is zero padded day of month:
(displayln (date->string (current-date) "~A, ~B ~d, ~Y"))
;;; ~e is space padded day of month:
(displayln (date->string (current-date) "~A, ~B ~e, ~Y"))
; Get the date in the format: Month Day.
(displayln (date->string (current-date) "~B ~e"))
