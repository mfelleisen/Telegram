#lang racket

(require "read-block.rkt")
(require racket/generator)
(module+ test (require rackunit))

;; a solution to Jackson's reformulated telegram problem using imperative generators
;; to validate that at most one word and at most one record about telegrams is in flight. 

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

;; ---------------------------------------------------------------------------------------------------
(define *the-word #false)

(define [make-read-file-into-words]
  (open-blocked-file)
  (generator ()
    (let while ()
      (define 1block (read-block))
      (cond
        [(eof-object? 1block)
         (set! *the-word eof)
         (yield 'go)]
        [else
         (define words (string-split 1block))
         (for ([w words])
           (set! *the-word w)
           (yield 'go))
         (while)]))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (check-equal? (let ()
                  (define read-file-into-words [make-read-file-into-words])
                  (with-input-from-string file1 read-file-into-words)
                  *the-word)
                word1
                "deal with one word at a time")
  
  (check-equal? (let ()
                  (define read-file-into-words [make-read-file-into-words])
                  (with-input-from-string file1
                    (λ ()
                      (list
                       (begin
                         (read-file-into-words)
                         *the-word)
                       (begin
                         (read-file-into-words)
                         *the-word)
                       (begin
                         (read-file-into-words)
                         *the-word)))))
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

;; ---------------------------------------------------------------------------------------------------
(define *the-telegram #false)

(define (make-words-to-telegram)
  (define read-file-into-words [make-read-file-into-words])
  (generator ()
    (let while ([words-in-telegram 0] [long-words-in-telegram 0])
      (read-file-into-words)
      (define 1word *the-word)
      (cond
        [(eof-object? 1word)
         (set! *the-telegram eof)]
        [(string=? EOT 1word)
         (set! *the-telegram (list words-in-telegram long-words-in-telegram))
         (yield 'go)
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
                      (begin [words-to-telegram] *the-telegram))))
                (list 0 0)
                "there is an empty telegram at the end of the tile")

  (check-equal? (let ()
                  (define words-to-telegram (make-words-to-telegram))
                  (with-input-from-string file1
                    (λ ()
                      (length
                       (list
                        (begin [words-to-telegram] *the-telegram)
                        (begin [words-to-telegram] *the-telegram)
                        (begin [words-to-telegram] *the-telegram)
                        (begin [words-to-telegram] *the-telegram)
                        (begin [words-to-telegram] *the-telegram))))))
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

(define (print-telegrams)
  (print-header)
  (print-body)
  (print-done))

(define (print-header)
  (printf "TELEGRAM ANALYSIS\n"))

(define (print-done)
  (printf "END ANALYSIS\n"))

(define (print-body)
  (define words-to-telegram (make-words-to-telegram))
  (let while ([i 1])
    (words-to-telegram)
    (define 1telegram *the-telegram)
    (cond
      [(eof-object? 1telegram) (void)]
      [else
       (printf "TELEGRAM ~a\n" i)
       (printf " ~a WORDS OF WHICH ~a ARE OVERSIZED\n" (first 1telegram) (second 1telegram))
       (while (+ i 1))])))

(with-input-from-string file1 print-telegrams)
  
