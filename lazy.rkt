#lang lazy

(require "read-block.rkt")
(require racket/string)
(module+ test (require rackunit))

;; a solution to Jackson's reformulated telegram problem using lists as intermediate data
;; violating the specified constraint 

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
       (append words (while))])))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (check-equal? (! (parameterize ([current-input-port (open-input-string file1)])
                     (!! (first (read-file-into-words)))))
                word1
                "deal with one word at a time")
  
  (check-equal? (! (parameterize ([current-input-port (open-input-string file1)])
                     (!! (take 3 (read-file-into-words)))))
                (!! (list word1 word2 word3))
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
      [(empty? word*) '()]
      [else
       (define 1word (first word*))
       (cond
         [(string=? EOT 1word)
          (cons (list words-in-telegram long-words-in-telegram)
                (while
                  (rest word*)
                  0
                  0))]
         [else
          (define 1word (first word*))
          (while
            (rest word*)
            (+ words-in-telegram 1)
            (+ long-words-in-telegram (if (> (string-length 1word) LONG) 1 0)))])])))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (define T (λ () (words-to-telegram (read-file-into-words))))
  (check-equal? (! (parameterize ([current-input-port (open-input-string file1)])
                     (!! (fifth (T)))))
                (!! (list 0 0))
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
      [(empty? telegram*) (void)]
      [else
       (define 1telegram (first telegram*))
       (define word-count (first 1telegram))
       (define long-word-count (second 1telegram))
       (printf "TELEGRAM ~a\n" i)
       (printf " ~a WORDS OF WHICH ~a ARE OVERSIZED\n" word-count long-word-count)
       (while (rest telegram*) (+ i 1))])))

(! (parameterize ([current-input-port (open-input-string file1)])
     (!! (main))))

