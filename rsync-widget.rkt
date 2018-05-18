#lang racket/gui

(provide rsync-widget%)

(define (read-lines-avail in)
  (define (bytes-til-last-break bstr)
    (define (is-break? x)
      (let ([c (integer->char x)])
        (or (eq? c #\u0a) (eq? c #\u0d)))) ; newline or carriage return

    (let ([i (for/last ([i (in-range (sub1 (bytes-length bstr)) -1 -1)]
                        #:final (is-break? (bytes-ref bstr i)))
                       i)])
      (if (is-break? (bytes-ref bstr i)) (add1 i) #f)))

  (filter non-empty-string?
          (regexp-split #rx"[\r\n]"
                        (bytes->string/utf-8
                          (let* ([peek-buf (make-bytes (* 1024 1024))] ; if it's more than 1MB, we'll get it next time...
                                 [bytes-peeked (peek-bytes-avail! peek-buf 0 #f in)])
                            (if (eof-object? bytes-peeked)
                              #""
                              (let*
                                ([peek-buf (subbytes peek-buf 0 bytes-peeked)]
                                 [to-read (bytes-til-last-break peek-buf)])
                                (if to-read
                                  (read-bytes to-read in)
                                  #""))))))))

(define (start-rsync src-list dst)
  (let* ([cmd (string-append "rsync --out-format=\"OUT: %f %l\" --size-only --progress --partial --append --files-from=- --no-R --no-D / " dst)]
         [proc (process cmd)])

    (for ([l (map path->string (map path->complete-path src-list))])
      (displayln l (second proc)))
    (close-output-port (second proc))

    (list (first proc)
          (fifth proc)
          (λ () (eq?  ((fifth proc) 'status) 'done-ok))
          (λ () (eq?  ((fifth proc) 'status) 'done-error))
          (fourth proc))))

(define rsync-widget%
  (class* vertical-panel% (control<%>)
          (init-field src-list dst)
          (super-new)

          (define rsync-in (start-rsync src-list dst))

          (define text-output (new text%))
          (define completed-list (new editor-canvas%
                                      [parent this]
                                      [editor text-output]
                                      ))
          (define progress (new gauge% [label "Progress:"] [parent this] [range 1000]))
          (define real-range 1000)

          (define (handle-line line)
            (let ([out (regexp-match #rx"OUT: (.*) ([0-9]+)$" line)]
                  [progress-m (regexp-match #rx"^ +([0-9]+)" line)])
              (if out
                (begin
                  (set! real-range (string->number (caddr out)))
                  (send progress set-value 0)
                  (send text-output insert (string-append (cadr out) "\n")))
                (when progress-m
                  (send progress set-value
                        (round (* 1000 (/ (string->number (cadr progress-m))
                                         real-range))))))))
          (define (finished)
            (send timer-update stop)
            (send text-output insert "finished!"))
          (define (print-error)
            (define errlines (read-lines-avail (fifth rsync-in)))
            (for ([l errlines])
              (send text-output insert (string-append l "\n"))))

          (define timer-update (new timer%
                                    [interval 1000]
                                    [notify-callback
                                      (λ ()
                                         (let ([lines (read-lines-avail (first rsync-in))]
                                               [done-ok? ((third rsync-in))]
                                               [done-error? ((fourth rsync-in))])
                                           (cond
                                             [(not (empty? lines))
                                              (for ([line lines])
                                                (handle-line line))]
                                             [(and (empty? lines) done-ok?)
                                              (finished)]
                                             [(and (empty? lines) done-error?)
                                              (print-error)
                                              (finished)])))]))

          (define/public (command e) (void))
          ))


(module* main #f
  (define testframe (new frame% [label "Rsync Widget Test"]))
  (define src-list (sequence->list (sequence-map path->string (in-directory "test/a"))))
  (define dst "test/b/")

  (define rsync-widget (new rsync-widget%
                            [parent testframe]
                            [min-height 200]
                            [src-list src-list]
                            [dst dst]))
  (send testframe show #t))
