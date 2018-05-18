#lang racket/gui

(require racket/match)
(provide rsync-widget%)

(define (read-lines-avail in)
  (define (bytes-til-last-break bstr)
    (define (is-break? x)
      (define c (integer->char x))
      (or (eq? c #\u0a) (eq? c #\u0d))) ; newline or carriage return

    (define i (for/last ([i (in-range (sub1 (bytes-length bstr)) -1 -1)]
                         #:final (is-break? (bytes-ref bstr i)))
                        i))
    (if (is-break? (bytes-ref bstr i)) (add1 i) #f))

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
  (define cmd (string-append "rsync --out-format=\"OUT: %f %l\" --size-only --progress --partial --append --files-from=- --no-R --no-D / " dst))
  (match-define (list proc-stdout proc-stdin pid proc-stderr ctrl)
    (process cmd))

  ; feed input lines to rsync
  ; FIXME for sufficiently long inputs, could rsync ever stop reading stdin,
  ;       causing this to block (and potentially deadlock if rsync itself blocks on stdout)?
  ;       
  ;       if so, writing to rsync's stdin from a seperate thread may be worthwhile
  (for ([l (map path->string (map path->complete-path src-list))])
    (displayln l proc-stdin))
  (close-output-port proc-stdin)

  (list proc-stdout
        proc-stderr
        (λ () (or (eq?  (ctrl 'status) 'done-ok)
                  (eq?  (ctrl 'status) 'done-error)))))

(define rsync-widget%
  (class* vertical-panel% (control<%>)
          (init-field src-list dst)
          (super-new)

          (match-define (list rsync-stdout rsync-stderr rsync-done?)
            (start-rsync src-list dst))

          ;TODO replace this with canvas% and draw something nice on it
          (define text-output (new text%))
          (define completed-list (new editor-canvas%
                                      [parent this]
                                      [editor text-output]))

          (define progress-range 1000)
          (define progress (new gauge% [label "Progress:"] [parent this] [range progress-range]))
          (define file-length 1000)

          (define (handle-line line)
            (match line
              [(regexp #rx"OUT: (.*) ([0-9]+)$" (list _ filename len))
               (set! file-length (string->number len))
               (send progress set-value 0)
               (send text-output insert (string-append filename "\n"))]
              [(regexp #rx"^ +([0-9]+)" (list _ amount-transfered))
               (send progress set-value
                     (round (* progress-range (/ (string->number amount-transfered)
                                                 (max file-length 1)))))]))

          (define (finished)
            (send text-output insert (string-join (read-lines-avail rsync-stderr) "\n"))
            (send timer-update stop)
            (send text-output insert "finished!"))

          (define timer-update (new timer%
                                    [interval 100]
                                    [notify-callback
                                      (λ ()
                                         (define lines (read-lines-avail rsync-stdout))
                                         (cond
                                           [(pair? lines)
                                            (for ([line lines])
                                              (handle-line line))]
                                           [(rsync-done?) (finished)]))]))

          (define/public (command e) (void))))


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
