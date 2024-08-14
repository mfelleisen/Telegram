#lang racket

(require "read-block.rkt")
(require racket/stream)
(module+ test (require rackunit))

;; a solution to Jackson's reformulated telegram problem using streams as intermediate data
;; unlike the list solution this one merely creates a promise for computing the rest so it
;; essentially like the generator solution(s).  

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

#; {-> [Streamof Word]}
(define [read-file-into-words]
  (open-blocked-file)
  (let while ()
    (define 1block (read-block))
    (cond
      [(eof-object? 1block)
       (close-blocked-file)
       '()]
      [else
       (define words (string-split 1block))
       (stream-append words (while))])))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (check-equal? (stream-first (with-input-from-string file1 read-file-into-words))
                word1
                "deal with one word at a time")
  
  (check-equal? (stream->list (stream-take (with-input-from-string file1 read-file-into-words) 3))
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

#; {[Streamof Word] -> [Streamof [List Natural Natural]]}
(define (words-to-telegram word*0)
  (let while ([word* word*0][words-in-telegram 0] [long-words-in-telegram 0])
    (cond
      [(stream-empty? word*) '()]
      [else
       (define 1word (stream-first word*))
       (cond
         [(string=? EOT 1word)
          (stream-cons (list words-in-telegram long-words-in-telegram)
                       (while
                         (stream-rest word*)
                         0
                         0))]
         [else
          (while
            (stream-rest word*)
            (+ words-in-telegram 1)
            (+ long-words-in-telegram (if (> (string-length 1word) LONG) 1 0)))])])))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (define T (λ () (words-to-telegram (read-file-into-words))))
  (check-equal? (stream-ref (with-input-from-string file1 T) 4)
                (list 0 0)
                "there is an empty telegram at the end of the tile")

  (check-equal? (length (stream->list (with-input-from-string file1 T)))
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
  (print-body (words-to-telegram [read-file-into-words]))
  (print-done))

(define (print-header)
  (printf "TELEGRAM ANALYSIS\n"))

(define (print-done)
  (printf "END ANALYSIS\n"))

#; {[Streamof [List Natural Natural]] -> Void}
;; pull data about telegrants from a generator, yielding one at a time 
(define (print-body telegram*0)
  (let while ([telegram* telegram*0][i 1])
    (cond
      [(stream-empty? telegram*) (void)]
      [else
       (define 1telegram (stream-first telegram*))
       (match-define [list word-count long-word-count] 1telegram)
       (printf "TELEGRAM ~a\n" i)
       (printf " ~a WORDS OF WHICH ~a ARE OVERSIZED\n" word-count long-word-count)
       (while (stream-rest telegram*) (+ i 1))])))

(with-input-from-string file1 main)

