#lang racket

(require "read-block.rkt")
(module+ test (require rackunit))

;; a solution to Jackson's reformulated telegram problem using functional continuaations
;; this is in principle what Jackson writes up as "program inversion" 

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

#; {type RFIW = {fix K : [Word [K -> K] -> Empty]} -> Empty}
(define [read-file-into-words yield0]
  (open-blocked-file)
  (let while ([yield yield0])
    (define 1block (read-block))
    (cond
      [(eof-object? 1block)
       (close-blocked-file)
       (yield eof void)]
      [else
       (define words (string-split 1block))
       (let fold ([yield yield] [words words])
         (match words
           ['() (while yield)]
           [(cons w words*) (yield w  (λ (y) (fold y words*)))]))])))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (check-equal? (let-values ([(w _)
                              (with-input-from-string file1 (λ () [read-file-into-words values]))])
                  w)
                word1
                "deal with one word at a time")

  (check-equal? (with-input-from-string file1
                  (λ ()
                    (read-file-into-words
                     (λ (word1 read-file-into-words)
                       (read-file-into-words
                        (λ (word2 read-file-into-words)
                          (read-file-into-words
                           (λ (word3 read-file-into-words)
                             [list word1 word2 word3]))))))))
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

;; The information needed about a telegram is a record of its number
;; of words and the number of overly long words.

#; {type Record = [List Natural Natural]}

#; {type WTT = RFIW {fix K : [Record [K -> K] -> Empty]} -> Empty}
(define (words-to-telegram R0 yield0)
  (let while ([words-in-telegram 0] [long-words-in=telegram 0] [R R0] [yield yield0])
    (R
     (λ (1word R)
       (cond
         [(eof-object? 1word) (yield eof void)]
         [(string=? EOT 1word)
          (yield (list words-in-telegram long-words-in=telegram)
                 (λ (yield) (while 0 0 R yield)))]
         [else
          (while
            (+ words-in-telegram 1)
            (+ long-words-in=telegram (if (> (string-length 1word) LONG) 1 0))
            R
            yield)])))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (check-equal? (with-input-from-string file1
                  (λ ()
                    [words-to-telegram
                     read-file-into-words
                     (λ (record1 words-to-telegram)
                       [words-to-telegram
                        (λ (record2 words-to-telegram)
                          [words-to-telegram
                           (λ (record3 words-to-telegram)
                             [words-to-telegram
                              (λ (record4 words-to-telegram)
                                [words-to-telegram
                                 (λ (record5 _)
                                   record5)])])])])]))
                (list 0 0)
                "there is an empty telegram at the end of the tile"))
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
  (print-body (λ (k) (words-to-telegram read-file-into-words k)))
  (print-done))

(define (print-header)
  (printf "TELEGRAM ANALYSIS\n"))

(define (print-done)
  (printf "END ANALYSIS\n"))

#; {(fix W : (Telegram W) -> Empty)-> Void}
;; pull data about telegrants from a generator, yielding one at a time 
(define (print-body W0)
  (let while ([i 1][W W0])
    (W
     (λ (1telegram W)
       (cond
         [(eof-object? 1telegram) (void)]
         [else
          (match-define [list word-count long-word-count] 1telegram)
          (printf "TELEGRAM ~a\n" i)
          (printf " ~a WORDS OF WHICH ~a ARE OVERSIZED\n" word-count long-word-count)
          (while (+ i 1) W)])))))

(with-input-from-string file1 main)
