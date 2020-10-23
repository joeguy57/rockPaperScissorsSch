;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; readLine() --> line (as String) 
;; (Rosanna)
;; Read one line from standard input, not including the newline
;; but eliminating it. This is wrapper for the recursive method
;; that does the work (readLoop). 
;; 
(define (readLine) 
 (readLoop (read-char (current-input-port)) '())) ;do wait for one char

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; readLoop(currentCharacter line) --> line (as String) 
;; (Rosanna)
;; This recursive method reads a character at a time from the
;; current input port (assuming Scheme's "Interaction Window")
;; until it finds the newline (i.e. enter). It builds the characters
;; into a string which is returned at the end. Newline is not part
;; of the string, but is eliminated from the input 
;; 
(define (readLoop curChar line) 
 (cond 
 ((char=? #\newline curChar) (list->string line)) 
 (else (readLoop (read-char (current-input-port))  
 (append line (list curChar))))))

;;
;;
;;
(define (errIn) 
                (display "Invalid input. Try Again\n")
                )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;
(define (humanPrt x)       
                     (cond ((equal? (car x) #\r) 'ROCK)
                           ((equal? (car x) #\p) 'PAPER)
                           ((equal? (car x) #\s) 'SCISSORS)
                           ((equal? (car x) #\R) 'ROCK)
                           ((equal? (car x) #\P) 'PAPER)
                           ((equal? (car x) #\S) 'SCISSORS)
                           (else (errIn) (humanPrt (string->list (readLine))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
(define (comp int)
    (cond ((equal? int 0) 'ROCK)
      ((equal? int 1) 'PAPER)
      ((equal? int 2) 'SCISSORS)
      (else (display "You Shouldnt Be Here!"))
      )
  )

;;
;;
;;
(define(play)
  (startup)
  (game (quote (0 0 0)) 10)
  )

;;
;;
;;
(define (startup)
  (display "WELCOME TO ROCK PAPER SCISSORS\n")
  (display "======================================\n")
  (newline)
  )


;;
;;
;;
(define game (lambda(scoreCheck rounds)
               (cond ((equal? rounds 0) (endGame scoreCheck))
                     (else (display "Enter ROCK PAPER SCISSORS: \n")
                            (game
                                  (gameCheck (humanPrt (string->list (readLine)))
                                             (comp 1)
                                             scoreCheck)
                                             (- rounds 1))))
              ))

;;
;;
;;
(define (endGame scoreCheck) 
                              (cond ((and (> (caddr scoreCheck) (car scoreCheck)) (> (caddr scoreCheck) (cadr scoreCheck)) (printChamp scoreCheck "draw")))
                                    (( > (car scoreCheck) (cadr scoreCheck)) (printChamp scoreCheck "YOU"))
                                    (else (printChamp scoreCheck "COMPUTER")))
                              )
;;
;;
;;
(define (printChamp scoreCheck indexState)
  (newline)
  (display "Game Over! You won")
  (display (car scoreCheck))
  (display " rounds, computer won")
  (display (cadr scorCheck))
  (display " rounds, tied")
  (display (caddr scoreCheck))
  (display " rounds\n Champion: ")
  (display indexState)
  )
;;
;;
;;
(define (gameCheck x y scoreCheck) 
                  (cond ((equal? x y) (gameCase x y scoreCheck "TIE"))
                        ((and (equal? x 'ROCK) (equal? y 'SCISSORS) (gameCase x y scoreCheck "Winner is YOU")))
                        ((and (equal? x 'PAPER) (equal? y 'ROCK) (gameCase x y scoreCheck "Winner is YOU")))
                        ((and (equal? x 'SCISSORS) (equal? y 'PAPER) (gameCase x y scoreCheck "Winner is YOU")))
                        (else (gameCase x y scoreCheck "Winner is COMPUTER"))
                  )
                  )
;;
;;
;;
(define (Wins scoreCheck indexState) 
                           (cond ((equal? indexState "Winner is You")
                                  (list
                                   (+ (car scoreCheck) 1) (cadr scoreCheck) (caddr scoreCheck)))
                                 ((equal? indexState "Winner is COMPUTER")
                                  (list
                                   (car scoreCheck) (+ (cadr scoreCheck) 1) (caddr scoreCheck)))
                                 ((equal? indexState "TIE")
                                  (list
                                   (car scoreCheck) (cadr scoreCheck) (+ (caddr scoreCheck) 1)))))

;;
;;
;;
(define (gameCase x y scoreCheck indexState)
                                 (display "You entered ")
                                 (display (upperCase x))
                                 (display "   Computer chose ")
                                 (display (upperCase y))
                                 (display " ")
                                 (display indexState)
                                 (newline)
                                 (Wins scoreCheck indexState)
                                 )


;;
;;
;;
(define (upperCase val)
  (cond ((equal? val 'ROCK) "ROCK")
        ((equal? val 'PAPER) "PAPER")
        ((equal? val 'SCISSORS) "SCISSORS")
  ))

 