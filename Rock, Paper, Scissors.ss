;;leapYear
;;
;;by Jinzhe Li
;;22 October 2018
;;
;;Run program
;;> (ROCK PAPER SCISSORS)
;;
;;A rock, paper, scissors game, which has
;;two players: user and computer. 
;;Computer will make its random choice.
;;The game have a total of 10 rounds.
;;Who wins more rounds is the winner.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;welcomeMsg
;;Displays the welcome message
;;
(define (welcomeMsg)
  (display "Welcome to ROCK PAPER SCISSORS game\n")
  (display "------------------------------\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;roundMsg
;;Displays the round message
;;
(define (roundMsg)
  (display "Please enter your choice: ")
  (display "\"Rock\", \"Paper\", or \"Scissors\" ")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;every round, decides who wins or tie game
;;and print it out, return a value to count
;;the times of results of the whole game.
;;zero indicates the user wins
;;one indicates the computer wins
;;two indicates tie game.
(define (round user computer)
  (cond
    ((equal? user 'ROCK)
    (cond
      ((equal? computer 'ROCK) (display "You entered Rock  Computer chose Rock  TIE\n\n") 2)
      ((equal? computer 'PAPER) (display "You entered Rock  Computer chose Paper  Winner is COMPUTER\n\n") 1)
      ((equal? computer 'SCISSORS) (display "You entered Rock  Computer chose Scissors Winner is YOU\n\n") 0)
      ))
    ((equal? user 'PAPER)
    (cond
      ((equal? computer 'ROCK) (display "You entered Paper  Computer chose Rock  Winner is YOU\n\n") 0)
      ((equal? computer 'PAPER) (display "You entered Paper  Computer chose Paper  TIE\n\n") 2)
      ((equal? computer 'SCISSORS) (display "You entered Paper  Computer chose Scissors  Winner is COMPUTER\n\n") 1)
      ))
    ((equal? user 'SCISSORS)
    (cond
      ((equal? computer 'ROCK) (display "You entered Scissors  Computer chose Rock  Winner is COMPUTER\n\n") 1)
      ((equal? computer 'PAPER) (display "You entered Scissors  Computer chose Paper  Winner is YOU\n\n") 0)
      ((equal? computer 'SCISSORS) (display "You entered Scissors  Computer chose Scissors  TIE\n\n") 2)
      ))
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;count function, not working somehow
;;userNum is times of rounds that user wins
;;comNum is times of rounds that computer wins
;;tie is the times of tie games
;(define (times num userNum comNum tie)
;  (cond
;    ((zero? num) (display "Game Over!"))
;    (else 
;      (cond
;        ((equal? (passing) 0) (times (- num 1) (+ userNum 1) (+ comNum 0) (+ tie 0)))
;        ((equal? (passing) 1) (times (- num 1) (+ userNum 0) (+ comNum 1) (+ tie 0)))
;        ((equal? (passing) 2) (times (- num 1) (+ userNum 0) (+ comNum 0) (+ tie 1)))
;       ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;let the game play 10 rounds.                                                        
(define (times num)
  (cond
    ((zero? num) (display "Game Over! You won  rounds, computer won  rounds, tied  rounds"))
    (else (passing) (times (- num 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;passing the parameters of user's and 
;;computer's choice to round function
(define (passing)
  (round (userChoice) (comChoice)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;get computer's choice
(define (comRockPaperOrS comLetter)
  (cond
    ((equal? comLetter 0) 'ROCK) 
    ((equal? comLetter 1) 'PAPER) 
    ((equal? comLetter 2) 'SCISSORS)))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; passing the parameter of random number to
;;(comRockPaperOrS) function
(define(comChoice)
  (comRockPaperOrS (getRand)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;get a random number from 0 to 2
(define (getRand) (random 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;get user's choice
(define (rockPaperOrS firstLetter) 
  (cond
    ( (or (equal? (car firstLetter) '#\r) (equal? (car firstLetter) '#\R)) 'ROCK) 
    ( (or (equal? (car firstLetter) '#\p) (equal? (car firstLetter) '#\P)) 'PAPER) 
    ( (or (equal? (car firstLetter) '#\s) (equal? (car firstLetter) '#\S)) 'SCISSORS) 
    (else (display "Not sure of your selection. Try again.\n") (rockPaperOrS (getInputToList)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;pass the parameter of user's input to
;;(rockPaperOrS) function
(define (userChoice)
  (rockPaperOrS (getInputToList)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;show round massage and get input and
;;change input from string to list
(define (getInputToList)
  (roundMsg)
  (string->list (readLine)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; readLine() --> line (as String)
;;
;; by Rosanna Heise
;;
;; Read one line from standard input, not including the newline
;; but eliminating it. This is wrapper for the recursive method
;; that does the work (readLoop).
;;
(define (readLine)
 (readLoop (read-char (current-input-port)) '())) ;do wait for one char

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; readLoop(currentCharacter line) --> line (as String)
;;
;; by Rosanna Heise
;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;main function
;;
(define(main)
  (welcomeMsg)
  (times 10))