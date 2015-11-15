;
; CS161 HW3: Sokoban
;
; *********************
;    READ THIS FIRST
; *********************
;
; All functions that you need to modify are marked with 'EXERCISE' in their
; header comments. This file also contains many helper functions. You may call
; any of them in your functions.
;
; Do not modify a-star.lsp.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any
; node. That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your
; heuristic functions.  Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on
; memory. So, it may crash on some hard sokoban problems and there is no easy
; fix (unless you buy Allegro). Of course, other versions of Lisp may also crash
; if the problem is too hard, but the amount of memory available will be
; relatively more relaxed. Improving the quality of the heuristic will mitigate
; this problem, as it will allow A* to solve hard problems with fewer node
; expansions. In either case, this limitation should not significantly affect
; your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition
; (which will affect your score).
;  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time.
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.

; Inputs: state
; Output: Goal state true or NIL
(defun goal-test (s)
	(cond ((null s) t)
		((atom s) (NOT (equal s 2)))
		(t (and (goal-test (car s)) (goal-test (cdr s))))
	)
 )
; Base case: Null tree, return true
; If atom, check if equals 2. If so, return NIL
; Else, go through car of s and cdr of s to make sure doesn't contain a 2

; EXERCISE: Modify this function to return the list of
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
;
; If you want to use it, you will need to set 'result' to be
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
;
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
;

; Input: state
; Output: list of possible next moves

(defun next-states (s)
  (let* ((result (list (try-move s 'up) (try-move s 'right) (try-move s 'down) (try-move s 'left))))
    (cleanUpList result)
   )
 )
; Calls try-move on each possible move (up, right, down, left), and appends the lists

; Inputs: state, row
; Output: The list of the state at that row
(defun get-row (s r)
 	(cond ((null s) wall)
	 	((equal r 0) (car s))
	 	(t (get-row (cdr s) (- r 1)))
	)
)
; Go through the list, decrementing r until it hits 0. Then return that row.

; Inputs: state (row), column 
; Output: Item in the state that you're searching for
(defun get-column (s c)
 	(cond ((null s) wall)
	 	((equal c 0) (car s))
	 	(t (get-column (cdr s) (- c 1)))
	)
)
; Go through the list, decrementing c until it hits 0. Then return that element.

; Input: state, row, column
; Output: The content of that row and column of the state
(defun get-square (s r c)
	(let ((row (get-row s r)))
		(cond ((equal row wall) wall)
			(t (get-column row c))
		)
	)
)
; Call get-row to find the row, then pass that row to get-col to get the content

; Input: state (row), column, value
; Output: state (row) with the value replaced in column
(defun replace-item (s c v)
	(cond ((null s) NIL)
	 	((equal c 0) (cons v (cdr s)))
	 	(t (cons (car s) (replace-item (cdr s) (- c 1) v)))
	)
)

; Input: state, row, column, value
; Output: state with the updated value
(defun set-square (s r c v)
 	(cond ((null s) NIL)
	 	((equal r 0) (cons (replace-item (car s) c v) (cdr s)))
	 	(t (cons (car s) (set-square (cdr s) (- r 1) c v)))
	)
)
; Find the right row. Append everything before
; Find the right column spot. Append everything before
; Add in v
; Append everything in column after v
; Append all rows left


;Write a function try-move that takes in a state S and a move direction D. This function should
; return the state that is the result of moving the keeper in state S in direction D. NIL should be
; returned if the move is invalid (e.g., there is a wall in that direction). How you represent a move
; direction is up to you. Remember to update the content of every square to the right value. Refer to
; Table 1 for the values of different types of square.

; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
;

; The whole game (the warehouse) is on a grid. In each step, the keeper can move in any of the four basic
; directions (up, down, left, right). The keeper cannot walk into a wall or into a box. However, the keeper
; can push a box if the square next to the box (in the pushing direction) is either a goal or an empty square.
; Only one box can be pushed in any given step. In the case of multiple goals, there is no specific goal that
; a box has to be in. Boxes can be placed in goals in any order. A box can also be pushed out of a goal if
; needed (to make way for other moves). The game ends as soon as every box is in a goal position (even if
; there are more goals than boxes). In general, the minimum number of moves is not strictly required by the
; actual Sokoban game, because even finding a solution to the problem is already hard for a human player.
; Nevertheless, it is an objective of this assignment.

; A square may contain one of the following:
; - Nothing (empty floor)
; - A wall
; - A box
; - The keeper
; - A goal
; - A box on top of a goal
; - The keeper on top of a goal.

; getkeeperposition returns c r

; Input: state, direction
; Output: state (if possible move), else NIL
(defun try-move (s D)
	(let* ((keeperPosition (getKeeperPosition s 0)) ; Get the keeper position
		(r (cadr keeperPosition))
		(c (car keeperPosition))
		(wherekeeper (get-square s r c)) ; Get the value of where the keeper is
		(s1 (if (equal wherekeeper keeperstar) (set-square s r c star) (set-square s r c blank))))
			; if the keeper is on a keeperstar, set the square to star, else blank

		(cond ((equal D 'up) 
			(let* ((ret (get-square s1 (- r 1) c)) ; See if keeper can move up
				(s2 (if (equal ret boxstar) (set-square s1 (- r 1) c keeperstar) (set-square s1 (- r 1) c keeper)))) 
				(cond ((equal ret wall) NIL) ; If wall, keeper can't move
					((equal ret blank) s2) ; If not moving a box, just return current state
					((equal ret star) (set-square s2 (- r 1) c keeperstar)) ; If keeper on a goal, it is a keeperstar
					((OR (equal ret box) (equal ret boxstar)) ; Else, check to see if keeper would be moving a box
					(let ((boxret (get-square s2 (- r 2) c)))
						(cond ((equal boxret blank) (set-square s2 (- r 2) c box)) ; Move box to blank
							((equal boxret star) (set-square s2 (- r 2) c boxstar)) ; Move box to goal state
							(t NIL))))
					(t NIL)) 
			))
		; Same logic as above for all of the directions
			((equal D 'down) 
				(let* ((ret (get-square s1 (+ r 1) c))
				(s2 (if (equal ret boxstar) (set-square s1 (+ r 1) c keeperstar) (set-square s1 (+ r 1) c keeper)))) 
				(cond ((equal ret wall) NIL) 
					((equal ret blank) s2) 
					((equal ret star) (set-square s2 (+ r 1) c keeperstar))
					((OR (equal ret box) (equal ret boxstar))
					(let ((boxret (get-square s2 (+ r 2) c)))
						(cond ((equal boxret blank) (set-square s2 (+ r 2) c box))
							((equal boxret star) (set-square s2 (+ r 2) c boxstar))
							(t NIL))))
					(t NIL)) 
			))
			((equal D 'right)
				(let* ((ret (get-square s1 r (+ c 1)))
				(s2 (if (equal ret boxstar) (set-square s1 r (+ c 1) keeperstar) (set-square s1 r (+ c 1) keeper)))) 
				(cond ((equal ret wall) NIL) 
					((equal ret blank) s2) 
					((equal ret star) (set-square s2 r (+ c 1) keeperstar))
					((OR (equal ret box) (equal ret boxstar))
					(let ((boxret (get-square s2 r (+ c 2))))
						(cond ((equal boxret blank) (set-square s2 r (+ c 2) box))
							((equal boxret star) (set-square s2 r (+ c 2) boxstar))
							(t NIL))))
					(t NIL)) 
			))
			((equal D 'left) 
				(let* ((ret (get-square s1 r (- c 1)))
				(s2 (if (equal ret boxstar) (set-square s1 r (- c 1) keeperstar) (set-square s1 r (- c 1) keeper)))) 
				(cond ((equal ret wall) NIL) 
					((equal ret blank) s2) 
					((equal ret star) (set-square s2 r (- c 1) keeperstar))
					((OR (equal ret box) (equal ret boxstar))
					(let ((boxret (get-square s2 r (- c 2))))
						(cond ((equal boxret blank) (set-square s2 r (- c 2) box))
							((equal boxret star) (set-square s2 r (- c 2) boxstar))
							(t NIL))))
					(t NIL)) 
			))
		)
	)
)
; If it returns a 1, return NIL
; If it returns a 2 or a 5, we have to make sure we can move the box in the same direction

; Invalid moves: 
	; keeper on a wall
	; keeper on a box
	; keeper push box NOT onto a goal or empty square
; NOTE: may be more goals than boxes

; up = r - 1
; down = r + 1
; right = c + 1
; left = c - 1

; EXERCISE: Modify this function to compute the trivial
; admissible heuristic.
;

; Input: state
; Output: 0
(defun h0 (s)
	0
  )

; Input: state
; Output: number of 2's (boxes) in that state (row)
(defun count-col (s)
	(cond ((null s) 0)
		((if (equal box (car s)) (+ 1 (count-col (cdr s))) (count-col (cdr s))))
	)
)

; This function IS admissible because it is always less than or equal to the optimal solution 
; Input: state
; Output: number of boxes in the state
(defun h1 (s)
 	(cond ((null s) 0)
	 	(t (+ (count-col (car s)) (h1 (cdr s))))
	)
)
; Pass each row in the state to count-col to count the number of boxes in that row

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this
; function to compute an admissible heuristic value of s.
;
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the
; running time of a function call.
;

; Input: full state, row, row count, column count
; Output: Number of 
(defun attwo (row r c)
	(cond ((null row) 0)
		((equal box (car row))
			(if (isStar (get-square row r (+ c 1)))
			(+ 1 (attwo row (cdr row) r (+ c 1)))
			(+ 2 (attwo row (cdr row) r (+ c 1)))))
		(t (attwo (cdr row) r (+ c 1)))
	)
)
; 

; Input: full state, row, row count
; Output: Sum of the number of moves it will take 
(defun heuristicrow (s r)
	(cond ((null s) 0)
	 	(t (+ (attwo (car s) r 0) (heuristicrow (cdr s) (+ r 1))))
	)
)
; For each row, it calls attwo to find two's in the row
; Adds the value of each row to the recursive call

; Input: State
; Output: Admissible heuristic to calculate h(n)
(defun h604177148 (s)
	(heuristicrow s 0)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.  Each problem can be visualized by calling
 | (printstate <problem>).  Problems are ordered roughly by their difficulties.
 | For most problems, we also privide 2 additional number per problem: 1) # of
 | nodes expanded by A* using our next-states and h0 heuristic.  2) the depth of
 | the optimal solution.  These numbers are located at the comments of the
 | problems. For example, the first problem below was solved by 80 nodes
 | expansion of A* and its optimal solution depth is 7.
 |
 | Your implementation may not result in the same number of nodes expanded, but
 | it should probably give something in the same ballpark. As for the solution
 | depth, any admissible heuristic must make A* return an optimal solution. So,
 | the depths of the optimal solutions provided could be used for checking
 | whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible
 | to solve without a good heuristic!
 |
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
