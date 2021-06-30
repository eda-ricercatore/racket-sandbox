#lang racket/base

#|
Reference:
+ Rosetta Code contributors, "Date format: Haskell," from Rosetta Code, June 8, 2021. Available online from Rosetta Code: Explore: Programming Tasks category: D: Date format: \S47 Haskell at: https://www.rosettacode.org/wiki/Date_format#Racket; June 30, 2021 was the last access date.
|#

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
#|
  References for string equality comparisons.
  \cite[\S4.4.2 String Comparisons]{Flatt2021a}
  Rosetta Code contributors, "String comparison: \S67 Racket," from Rosetta Code, March 25, 2021. Available online from Rosetta Code: Explore: Programming Tasks category: S: String comparison: Racket: at: https://rosettacode.org/wiki/String_comparison#Racket; June 30, 2021 was the last access date.
|#
(string=? "June 30" "June 30")
(string=? "June 29" "June 30")
(string=? "June 10" "June 11")
(let ((birthday "June 30") (currentDay "June 30"))
  (string=? birthday currentDay))
#|
(set! currentDay "June 30")
(string=? (displayln (date->string (current-date) "~B ~e")) "June 30")
;(string=? "June 30" (displayln (date->string (current-date) "~B ~e")))
|#
