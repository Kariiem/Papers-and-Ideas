;; Trampolined Style
;; T : a type constructor
;; T a : a is the type of result
;; a thread of type (T a) is an
;; intermediate state of a computation returning a
;; value of type a

;; data T a = Return a | Bounce (Unit -> T a)

;; return :: a -> T a
;; return a = Return a       ;; inl

;; bounce :: (Unit -> T a) -> T a
;; bounce u2ta = Bounce u2ta ;; in r

;; schedule :: T a -> a
;; schedule (Return a) = a
;; schedule (Bounce u2a) = schedule (u2a unit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test programs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define fact-acc
  (lambda (n acc)
    (cond
     [(zero? n) (return acc)]
     [else
      (bounce (lambda ()
                (fact-acc
                 (sub1 n)
                 (* acc n))))])))
(define (fact n) (fact-acc n 1))

(define mem?
  (lambda (n ls)
    (cond
     [(null? ls) (return #f)]
     [(= (car ls) n) (return #t)]
     [else
      (bounce
       (lambda ()
         (mem? n (cdr ls))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trivial defintions
(define (return x) x)
(define (bounce thunk) ;; bounce recieves a thunk (parameter-less lambda),
  (thunk))             ;; here, a simple defintion, bounce applies the thunk

(fact 5)
(mem? 2 '(1 2 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abstractions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-done
  (lambda (value)
    `(done ,value)))

(define make-doing
  (lambda (thunk)
    `(doing ,thunk)))

(define return
  (lambda (value)
    (make-done value)))

(define bounce
  (lambda (thunk)
    (make-doing thunk)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheduler for one thread
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pogo-stick
  (lambda (thread)
    (record-case thread
      [done (value)
            value]
      [doing (thunk)
             (pogo-stick (thunk))])))

(pogo-stick (fact 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheduler for two threads
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define seesaw
  (lambda (down-thread up-thread)
    (record-case down-thread
      [done (down-value) down-value]
      [doing (down-thunk)
             (seesaw up-thread (down-thunk))])))

(seesaw (fact -1)
        (mem? 5 '(1 2 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheduler for N threads
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define trampoline
  (lambda (thread-queue)
    (record-case (car thread-queue)
      [done (value)
            value]
      [doing (thunk)
             (trampoline (append (cdr thread-queue)
                                 (list (thunk))))])))
(trampoline (list (fact -1)
                  (fact -1)
                  (mem? 120 '(100 110 120 130))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(pogo-stick
 (mem?
  (pogo-stick (fact 5))
  '(100 110 120 130)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sequence :: T a -> (a -> T a) -> T a
(define sequence
  (lambda (f thread)
    (record-case thread
      [done (value) (f value)]
      [doing (thunk)
             (bounce (lambda ()
                       (sequence f (thunk))))])))
