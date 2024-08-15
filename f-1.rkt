#lang htdp/isl+

;; the Telegram problem for Fundamentals I 

(define EOB "Φ")
(define (eob? x) (equal? EOB x))

(define EOT "zzzz")
(define (eot? x) (equal? EOT x))

;; A BlockedFile is one of:
;; -- '()
;; -- (cons EOB BlockedFile)
;; -- (cons EOT BlockedFile)
;; -- (cons Word BlockedFile)
;;
;; A Word is a String consisting of just the letters a through z.
;; NOTE EOT is a Word.
;; A long Word exceeds 10 characters.

(define LONG 10)
(define (long? w)
  (>= (string-length w) LONG))

;; 
;; A Block is one of:
;; -- '()
;; -- (cons Word Block)
;;
;; A Telegram is a Block that does not contain EOT. 

;; ---------------------------------------------------------------------------------------------------
;; DATA EXAMPLES

(define block1 `["this" "is" "a" "simple" "block"])
(define pblock `[,EOT "followed" "by" "more" "and" "a" "long-word-ish"])
(define qblock `["words"])
(define rfile  (append pblock `[,EOB] qblock))
(define bfile1 (append block1 `[,EOB] rfile))

(define word*  (append block1 pblock qblock))

;; the statistics (number of words, number of long words) in the telegrams of bfile1 
(define bresult1 (list (list (length block1) 0) (list (length pblock) 1)))

;; ---------------------------------------------------------------------------------------------------
;; MAIN (plan it first, write it last)

#; {BlockedFule -> [Listof Record]}
;; gather statistics about the telegrams contained in bf
(define (main bf)
  (words-to-telegram
   (file-to-words bf)))

(check-expect (main bfile1) bresult1)

;; ---------------------------------------------------------------------------------------------------
;; If this is a library, the rest can be done with strutural accumulators,
;; which could be taught before Part VI. 

#; {BlockedFile -> [List Block BlockedFile]}
;; read the first block from bf0 and deliver the rest of the file too 
(define (read-block bf0)
  (local (#; {BlockedFile Block -> [List Block BlockedFile]}
          ;; ACCUMULATOR accu is the reverse of the words between s0 and s, not including a EOB
          (define (read-block/aux bf r-block-so-far)
            (cond
              [(empty? bf) [list (reverse r-block-so-far) '()]]
              [(eob? (first bf)) (list (reverse r-block-so-far) (rest bf))]
              [else (read-block/aux (rest bf) (cons (first bf) r-block-so-far))])))
    (read-block/aux bf0 '())))

(check-expect (read-block bfile1) (list block1 rfile))
(check-expect (read-block '[]) (list '[] '[]))

;; ---------------------------------------------------------------------------------------------------
#; {BlockedFile -> [Listof Word]}
;; read a blocked file until the end and collect all the words in a list 
(define (file-to-words bf)
  (cond
    [(empty? bf) '()]
    [else
     (local ((define block+rest (read-block bf))
             (define 1block     (first block+rest))
             (define remainder  (second block+rest)))
       (append 1block (file-to-words remainder)))]))

(check-expect (file-to-words bfile1) word*)

;; ---------------------------------------------------------------------------------------------------
#; {[Listof Word] -> [Listof Record]}
;; turns a list of words into a list of properties about telegrams 
(define (words-to-telegram w*0)
  (local (#; {[Listof Word] [Listof Word] [Listof Record] -> [Listof Record]}
          ;; ACCUMULATOR 1 telegram-so-far represents the telegram between t0 and r-records
          ;; ACCUMULATOR 2 r-records are the reverse records about the telegrams between w*0 and w*
          (define (words-to-telegram/accu w* telegram-so-far r-record*)
            (cond
              [(empty? w*)
               (reverse (cons (record telegram-so-far) r-record*))]
              [(eot? (first w*))
               (words-to-telegram/accu (rest w*) '() (cons (record telegram-so-far) r-record*))]
              [else
               (words-to-telegram/accu (rest w*) (cons (first w*) telegram-so-far) r-record*)])))
    (words-to-telegram/accu w*0 '[] '[])))

#; {Telegram -> Record}
;; gather the relevant statistics about a telegram (number of words, number of long words)
(define (record t0)
  (local (#; {Telegram Natural Natural -> Record}
          ;; ACCUMULATOR words are the number of words betwee t0 and t
          ;; ACCUMULATOR long-words are the number of long words betwee t0 and t
          (define (record/accu t words long-words)
            (cond
              [(empty? t) (list words long-words)]
              [else
               (record/accu (rest t) (+ words 1) (+ long-words (if (long? (first t)) 1 0)))])))
    (record/accu t0 0 0)))

(check-expect (words-to-telegram word*) bresult1)
