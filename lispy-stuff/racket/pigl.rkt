#lang racket
(define (vowel? word)
  (member (string-ref word 0)
          '(#\a #\e #\i #\o #\u)))

(define (pigl wd)
  (if (vowel? wd)
      (string-append wd "ay")
      (pigl (string-append (substring wd 1) (substring wd 0 1)))))

