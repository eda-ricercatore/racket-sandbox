#lang racket/base

; This is written by Zhiyang Ong to print a birthday greeting on Dr. BLAH's birthday.

(require srfi/19)
(when (let ((currentDay (date->string (current-date) "~B ~e")) (birthday "July  2")) (string=? birthday currentDay))
  (println "Happy Birthday, Dr. BLAH!"))
; Here are some building blocks for this code snippet.
; Determine the current date in the format: Month Day.
(displayln (date->string (current-date) "~B ~e"))
; Compare the current date to the specified birthday.
; If they match, wish Dr. BLAH, "Happy Birthday!"
(let ((currentDay (date->string (current-date) "~B ~e")) (birthday "July  2"))
  (string=? birthday currentDay))
