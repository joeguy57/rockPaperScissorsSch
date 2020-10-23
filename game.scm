;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Using scheme to program a functional game of 
;; Rock Paper Scissors 
;;
;; Created by Joseph Menezes (Using available code from Rosanna Heise)
;; Created on the 20/10/2020
;; Last Edited on 22/10/2020

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; errIn -> error message (output string)
;;
;;returns when a user input an invalid move
(define (errIn) 
                (display "Invalid input. Try Again\n")
                (display "Enter ROCK PAPER SCISSORS: ")
                (humanPrt (string->list (readLine)))
                )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; humanPrt(inputLst) -> users input (as atom)
;;
;; Checks to see what the user has inputted to match
;; the first character of the list inputLst.
;; inputLst - a list of what the user inputted.
(define (humanPrt inputLst)       
                     (cond ((equal? inputLst '()) (errIn))
                           ((equal? (car inputLst) #\r) 'ROCK)
                           ((equal? (car inputLst) #\p) 'PAPER)
                           ((equal? (car inputLst) #\s) 'SCISSORS)
                           ((equal? (car inputLst) #\R) 'ROCK)
                           ((equal? (car inputLst) #\P) 'PAPER)
                           ((equal? (car inputLst) #\S) 'SCISSORS)
                           (else (errIn))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;comp(int) -> computersMove (as an atom)
;;
;;Depending on the input int the choice is being made as to how the computer
;; chooses to play
;; int - a random value between 0-2 in order to decide users move
(define (comp int)
    (cond ((equal? int 0) 'ROCK)
      ((equal? int 1) 'PAPER)
      ((equal? int 2) 'SCISSORS)
      (else (display "You Shouldnt Be Here!"))
      )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(play)-> endGame
;;
;;
;; A central hub to intialise the loop (recursion) and introduce the user
;; to the game.
(define(play)
  (startup)
  (game (quote (0 0 0)) 10)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;startup() -> line (as atom)
;;
;; An introduction to the user (maybe in future could add the rules)
;;
(define (startup)
  (display "WELCOME TO ROCK PAPER SCISSORS\n")
  (display "======================================\n")
  (newline)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; game (scoreCheck rounds) -> a list of the scores at the end of each round (as a list)
;;
;; Loops through game until round reaches zero then declares champion
;; Here comp and humanPrt are intialised obtaining the scoreCheck from
;; when intialised in play
;; scoreCheck is set up as a list where 
;; index 0= user's wins
;; index 1 = computer's winse
;; index 2 = draws
;; rounds is the is the number of loops in each game
(define game (lambda(scoreCheck rounds)
               (cond ((equal? rounds 0) (endGame scoreCheck))
                     (else (display "Enter ROCK PAPER SCISSORS: ")
                            (game ;; Recursively calls the the input and computer Move along with previous games score
                                  (gameCheck (humanPrt (string->list (readLine)))
                                             (comp 1)
                                             scoreCheck)
                                             (- rounds 1))))
              ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gameCheck(x y score) -> a list of score at the end of a round 
;; check to see if it is a win or loss
;;
;; x - an atom of the users input
;; y - an atom of the computers moce
;; scoreCheck - a list of score values
;;
(define (gameCheck x y scoreCheck) 
                  (cond ((equal? x y) (gameCase x y scoreCheck "TIE"))
                        ((and (equal? x 'ROCK) (equal? y 'SCISSORS) (gameCase x y scoreCheck "Winner is YOU")))
                        ((and (equal? x 'PAPER) (equal? y 'ROCK) (gameCase x y scoreCheck "Winner is YOU")))
                        ((and (equal? x 'SCISSORS) (equal? y 'PAPER) (gameCase x y scoreCheck "Winner is YOU")))
                        (else (gameCase x y scoreCheck "Winner is COMPUTER"))
                  )
                  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wins(scoreCheck indexState) -> a list with one index incremented by one
;; This function depending on who wins returns a list of scores.
;;
;; scoreCheck - list of score for wins, loss and draws [indexed in that order]
;; indexState - a string which identifies who is the winner
;;
(define (Wins scoreCheck indexState) 
                           (cond ((string=? indexState "Winner is YOU")
                                  (list
                                   (+ (car scoreCheck) 1) (cadr scoreCheck) (caddr scoreCheck)))
                                 ((string=? indexState "Winner is COMPUTER")
                                  (list
                                   (car scoreCheck) (+ (cadr scoreCheck) 1) (caddr scoreCheck)))
                                 ((string=? indexState "TIE")
                                  (list
                                   (car scoreCheck) (cadr scoreCheck) (+ (caddr scoreCheck) 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gameCase(x y scoreCheck indexState) -> a list of values with a manipulated score
;; This function displays output message on the outcome of the rounds
;; then passes information in order to be incremented.
;; 
;; x - user's input
;; y - computers move
;; scoreCheck - a list of the previous scores
;; indexState - is able to to tell who the winner is.
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; endGame(scoreCheck) -> the outcome of the game (as atom)
;;
;; checks to see what the end score results too, and who is the winner.
;; scoreCheck- a list of the final list of scores.
(define (endGame scoreCheck) 
                              (cond ((and (> (caddr scoreCheck) (car scoreCheck)) (> (caddr scoreCheck) (cadr scoreCheck)) (printChamp scoreCheck "DRAW")))
                                    (( > (car scoreCheck) (cadr scoreCheck)) (printChamp scoreCheck "YOU"))
                                    (else (printChamp scoreCheck "COMPUTER")))
                              )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;printChamp(scoreCheck indexState) -> output message (as an atom))
;;
;;
;; prints out the final statement, giving the layout of the outcomes of all 10 rounds
;; indexState refers to who the champion is in the game, as dervied from the outcome
;; of the conditionals from endGame.
;; scoreCheck- a list of the final score
;; indexState - identifies who the winner of all rounds is.
(define (printChamp scoreCheck indexState)
  (newline)
  (display "Game Over! You won ")
  (display (car scoreCheck))
  (display " rounds, computer won ")
  (display (cadr scoreCheck))
  (display " rounds, tied ")
  (display (caddr scoreCheck))
  (display " rounds\n Champion: ")
  (display indexState)
  (newline)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uperCase(val) -> an uppercase version of an output (as a String)
;;
;; This function takes the atom of val and returns its value into String
;; val - an atom of the move.
;;
(define (upperCase val)
  (cond ((equal? val 'ROCK) "ROCK")
        ((equal? val 'PAPER) "PAPER")
        ((equal? val 'SCISSORS) "SCISSORS")
  ))

 