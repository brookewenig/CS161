; N Queens Problem
; Implemented using DFS with backtracking

; Input: N (size of chessboard and number of queens)
; Returns a valid list of column numbers for a queen to occupy
(defun QUEENS (n)
	(moveQueens NIL 1 1 n)
)

; Input: List of valid columns thus far, front column value, column start value, and size of board (n)
; Returns the list of valid column positions for the queens
(defun moveQueens (front fcol col n)
	; Try to move a queen
	(let* ((queen (moveQueen front col n))) 
		(cond ((equal (length front) n) front) ; Solution! Able to fit all n queens
			; No solution for this problem - front col number greater than board size
			((> fcol n) NIL) 
			; Not a valid state
			((equal queen NIL) NIL) 
			; Try to recursively call and get a valid state with the current queen arrangment
			(t (let* ((validmove (moveQueens queen (+ fcol 1) 1 n))) 
					; If it's a valid state, return it
					(cond (validmove validmove) 
						; Else, if the column number is valid, but invalid state, increment column counter
						((< col n) (moveQueens front fcol (+ col 1) n))
						; Else, go back and modify, starting with a higher count for that column position 
						(t (moveQueens front (+ fcol 1) 1 n)))))
		)
	)
)

; Input: List of valid columns thus far, counter, and size of board (n)
; Returns a list of valid columns with the counter appended
(defun moveQueen (front counter n)
	(let* 
		; Makes a temporary new list
		((tempfront (append front (list counter)))) 
		; If the counter is greater than the size of the board, then it is invalid
		(cond ((> counter n) NIL) 
			((OR (duplicate tempfront counter) (diagonals (car tempfront) (cdr tempfront))) ; Check if there are any duplicate or diagonals
				 (moveQueen front (+ counter 1) n)) ; If there are, try to place a queen again, starting with a higher counter
			; Else, the move worked and return the new list
			(t tempfront)) 
	)
)

; Input: List, number just appended to list
; Returns true (there is a duplicate) if the number that was just appended occurs more than once
(defun duplicate (L added)
	(cond ((null L) NIL)
		(( > (count added L) 1) t)
	)
)

; Input: head of list, rest of List
; Returns true if there is a diagonal
(defun diagonals (h L)
	(cond ((null L) NIL)
		((diagonal h L 1) t) ; Check for diagonal in that row
		(t (diagonals (car L) (cdr L))) ; Recursively check and move head forward by one
	)
)

; Input: head of list, rest of List, size of board
; Returns true if there is a diagonal
(defun diagonal (h L n)
	(cond ((null L) NIL) 
		((equal (+ h n) (car L)) t) ; Diagonal from top to bottom
		((equal (- h n) (car L)) t) ; Diagonal from bottom to top
		(t (diagonal h (cdr L) (+ n 1))) ; Recursively call until reach end of row
	)
)
; NOTE: If two elements are the same distance apart as their values are different, then it is a diagonal.
