#lang racket

(provide read-block open-blocked-file close-blocked-file)

(provide word1 word2 word3 file1)

(module+ test (require rackunit))

;                                            
;   ;                           ;            
;   ;      ;;;                  ;            
;   ;        ;                  ;            
;   ;;;;     ;     ;;;    ;;;   ;  ;    ;;;  
;   ;; ;;    ;    ;; ;;  ;;  ;  ;  ;   ;   ; 
;   ;   ;    ;    ;   ;  ;      ; ;    ;     
;   ;   ;    ;    ;   ;  ;      ;;;     ;;;  
;   ;   ;    ;    ;   ;  ;      ; ;        ; 
;   ;; ;;    ;    ;; ;;  ;;     ;  ;   ;   ; 
;   ;;;;      ;;   ;;;    ;;;;  ;   ;   ;;;  
;                                            
;                                            
;                                            

;; Φ is the end of block chracter.

#; {Char -> Boolean : Φ}
(define (eob? ch)
  (eq? ch #\Φ))

;; A block is a sequence of chars in a-z plus #\space ending in Φ.
;; A block is at most 100 chars long.

(define MAX-BLOCK 100)

;; The given file is a sequence of blocks that contains an empty telegram just before EOF.

;; ---------------------------------------------------------------------------------------------------
;; EXAMPLE

(define word1 "this")
(define word2 "is")
(define word3 "a")

(define block0
  (string-append word1 " " word2 " " word3 " simple zzzz short block.  Φ"))

(define file1
  (string-append
   block0 
   #<< block
  It is followed by zzzz a muchmuchmuchmuch longer block zzzz
 that happens to start with Φ spaces and that
  happens to end in spaces.   Φ
 and here is a final one. zzzz zzzz 
 block
   ))

;                                                                 
;                            ;  ;                           ;     
;                            ;  ;      ;;;                  ;     
;                            ;  ;        ;                  ;     
;    ;;;;   ;;;   ;;;;    ;;;;  ;;;;     ;     ;;;    ;;;   ;  ;  
;    ;;  ; ;;  ;      ;  ;; ;;  ;; ;;    ;    ;; ;;  ;;  ;  ;  ;  
;    ;     ;   ;;     ;  ;   ;  ;   ;    ;    ;   ;  ;      ; ;   
;    ;     ;;;;;;  ;;;;  ;   ;  ;   ;    ;    ;   ;  ;      ;;;   
;    ;     ;      ;   ;  ;   ;  ;   ;    ;    ;   ;  ;      ; ;   
;    ;     ;      ;   ;  ;; ;;  ;; ;;    ;    ;; ;;  ;;     ;  ;  
;    ;      ;;;;   ;;;;   ;;;;  ;;;;      ;;   ;;;    ;;;;  ;   ; 
;                                                                 
;                                                                 
;                                                                 

(define *returned-last-block #false)

(define (open-blocked-file) (set! *returned-last-block #false))

#; {-> String}
;; reads a block into a string
(define (read-block)
  (cond
    [*returned-last-block eof]
    [else 
     ;; `so-far` could be eliminated with imperative updates to pre-allocated 100char array
     (let while ([so-far '()])
       (define 1char (read-char))
       (cond
         [(eof-object? 1char)
          (set! *returned-last-block #true)
          (apply string (reverse so-far))]
         [(eob? 1char)
          (when (>= (length so-far) MAX-BLOCK) (error 'read-block "too long"))
          (apply string (reverse so-far))]
         [else (while (cons 1char so-far))]))]))

(define (close-blocked-file) (set! *returned-last-block #false))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (check-equal? (begin
                  (open-blocked-file)
                  (with-input-from-string file1 read-block))
                (substring block0 0 (- (string-length block0) 1))
                "simple read block test")

  (check-equal? (begin
                  (open-blocked-file)
                  (with-input-from-string file1
                    (λ ()
                      (let while ()
                        (define next (read-block))
                        (if (eof-object? next) 0 (add1 (while)))))))
                4
                "reading all blocks in a file"))

