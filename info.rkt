#lang info
(define collection "rsync-racket")
(define deps '("base"
               "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/rsync-racket.scrbl" ())))
(define pkg-desc "GUI widget for rsync transfers")
(define version "0.0")
(define pkg-authors '(jgreco))
