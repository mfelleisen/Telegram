#lang racket

(require "read-block.rkt")
(module+ test (require rackunit))

;; a solution to Jackson's reformulated telegram problem using functional generators

;                                     
;                            ;        
;                            ;        
;                            ;        
;  ;     ;  ;;;    ;;;;   ;;;;   ;;;  
;  ;     ; ;; ;;   ;;  ; ;; ;;  ;   ; 
;   ; ; ;  ;   ;   ;     ;   ;  ;     
;   ; ; ;  ;   ;   ;     ;   ;   ;;;  
;   ;; ;;  ;   ;   ;     ;   ;      ; 
;   ;; ;;  ;; ;;   ;     ;; ;;  ;   ; 
;    ; ;    ;;;    ;      ;;;;   ;;;  
;                                     
;                                     
;                                     

;; A word is a sequence of chars in a-z, delimited by beginnig of file, end of file, #\space, or Φ.
;; A long word is 10 chars or longer.
(define LONG 10)

(require racket/generator)

#; {-> [Generator Word]}
;; create a generator that turns a series of blocks from a "file" into words, yielding one at a time 
(define [make-read-file-into-words]
  (open-blocked-file)
  (generator ()
    (let while ()
      (define 1block (read-block))
      (cond
        [(eof-object? 1block)
         (close-blocked-file)
         (yield eof)]
        [else
         (define words (string-split 1block))
         (for ([w words]) (yield w))
         (while)]))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (check-equal? (let ()
                  (define read-file-into-words [make-read-file-into-words])
                  (with-input-from-string file1 read-file-into-words))
                word1
                "deal with one word at a time")
  
  (check-equal? (let ()
                  (define read-file-into-words [make-read-file-into-words])
                  (with-input-from-string file1
                    (λ ()
                      (list
                       (read-file-into-words)
                       (read-file-into-words)
                       (read-file-into-words)))))
                (list word1 word2 word3)
                "deal with one word at a time"))

;                                                          
;                                                          
;     ;           ;;;                                      
;     ;             ;                                      
;   ;;;;;   ;;;     ;     ;;;    ;;;;   ;;;;  ;;;;  ;;;;;; 
;     ;    ;;  ;    ;    ;;  ;  ;;  ;   ;;  ;     ; ;  ;  ;
;     ;    ;   ;;   ;    ;   ;; ;   ;   ;         ; ;  ;  ;
;     ;    ;;;;;;   ;    ;;;;;; ;   ;   ;      ;;;; ;  ;  ;
;     ;    ;        ;    ;      ;   ;   ;     ;   ; ;  ;  ;
;     ;    ;        ;    ;      ;; ;;   ;     ;   ; ;  ;  ;
;     ;;;   ;;;;     ;;   ;;;;   ;;;;   ;      ;;;; ;  ;  ;
;                                   ;                      
;                                ;  ;                      
;                                 ;;                       

;; A telegran is a sequence of words ending in zzzz.

(define EOT "zzzz")

#; {-> [Generator [List Natural Natural]]}
;; create a generator that turns a series of words from a generator into information about telegrams,
;; yielding data about one telegram at a time 
(define (make-words-to-telegram)
  (define read-file-into-words [make-read-file-into-words])
  (generator ()
    (let while ([words-in-telegram 0] [long-words-in-telegram 0])
      (define 1word (read-file-into-words))
      (cond
        [(eof-object? 1word) eof]
        [(string=? EOT 1word)
         (yield (list words-in-telegram long-words-in-telegram))
         (while 0 0)]
        [else
         (while
           (+ words-in-telegram 1)
           (+ long-words-in-telegram (if (> (string-length 1word) LONG) 1 0)))]))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (check-equal? (let ()
                  (define words-to-telegram (make-words-to-telegram))
                  (with-input-from-string file1
                    (λ ()
                      [words-to-telegram]
                      [words-to-telegram]
                      [words-to-telegram]
                      [words-to-telegram]
                      [words-to-telegram])))
                (list 0 0)
                "there is an empty telegram at the end of the tile")

  (check-equal? (let ()
                  (define words-to-telegram (make-words-to-telegram))
                  (with-input-from-string file1
                    (λ ()
                      (length
                       (list
                        [words-to-telegram]
                        [words-to-telegram]
                        [words-to-telegram]
                        [words-to-telegram]
                        [words-to-telegram])))))
                5
                "there five telegrams, including the empty one"))

;                              
;                              
;                    ;         
;                              
;  ;;;;;;  ;;;;    ;;;   ; ;;  
;  ;  ;  ;     ;     ;   ;;  ; 
;  ;  ;  ;     ;     ;   ;   ; 
;  ;  ;  ;  ;;;;     ;   ;   ; 
;  ;  ;  ; ;   ;     ;   ;   ; 
;  ;  ;  ; ;   ;     ;   ;   ; 
;  ;  ;  ;  ;;;;   ;;;;; ;   ; 
;                              
;                              
;                              

(define (main)
  (print-header)
  (print-body)
  (print-done))

(define (print-header)
  (printf "TELEGRAM ANALYSIS\n"))

(define (print-done)
  (printf "END ANALYSIS\n"))

#; {-> Void}
;; pull data about telegrants from a generator, yielding one at a time 
(define (print-body)
  (define words-to-telegram (make-words-to-telegram))
  (let while ([i 1])
    (define 1telegram (words-to-telegram))
    (cond
      [(eof-object? 1telegram) (void)]
      [else
       (match-define [list word-count long-word-count] 1telegram)
       (printf "TELEGRAM ~a\n" i)
       (printf " ~a WORDS OF WHICH ~a ARE OVERSIZED\n" word-count long-word-count)
       (while (+ i 1))])))

(with-input-from-string file1 main)
  
