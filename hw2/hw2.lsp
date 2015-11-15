; *****************************************************************************

; Inputs: Tree. 
; Outputs: A list of nodes, ordered by their DFS traversal

(defun DFS (TREE)
	(cond ((null TREE) nil)
		((atom TREE) (list TREE))
		(t (append (DFS (car TREE)) (DFS (cdr TREE))))
	)
)

; Base case: if tree is empty, return nil
; If there is only one element in the tree, turn it into a list and return
; Else, append the result of using DFS on the car of the tree with the cdr of the tree

; *****************************************************************************

; Inputs: Tree, n/level (max depth to which the tree should be traversed)
; Note: DFIDS is a helper function to DFID

(defun DFIDS (TREE level)
	(cond ((null TREE) NIL)
		((atom TREE) (list TREE))
		((equal level 0) NIL)
		(t (append (DFIDS (car TREE) (- level 1)) (DFIDS (cdr TREE) level)))
	)
)
; Base case: if tree is empty, return nil
; If there is only one element in the tree, turn it into a list and return
; Else, do DFS for all the nodes up to that level

(defun DFID (TREE n)
	(cond ((equal n 0) NIL)
		(t (append (DFID TREE (- n 1)) (DFIDS TREE n)))
	)
)
; If number of levels to traverse is 0, return nil
; Else, start at last level and prune until hit level 0, then append to end of results

; *****************************************************************************

; These functions implement a depth-first solver for the missionary-cannibal
; problem. In this problem, three missionaries and three cannibals are trying to
; go from the east side of a river to the west side. They have a single boat
; that can carry two people at a time from one side of the river to the other.
; There must be at least one person in the boat to cross the river. There can
; never be more cannibals on one side of the river than missionaries. If there
; are, the cannibals eat the missionaries.

; In this implementation, a state is represented by a single list (MISSIONARIES
; CANNIBALS SIDE). SIDE represents which side the boat is currently on, and is T
; if it is on the east side and NIL if on the west side. MISSIONARIES and
; CANNIBALS represent the number of missionaries and cannibals on the same side
; as the boat. Thus, the initial state for this problem is (3 3 T) (three
; missionaries, three cannibals, and the boat are all on the east side of the
; river) and the goal state is (3 3 NIL).

; The main entry point for this solver is the function MC-DFS, which is called
; with the initial state to search from and the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, MC-DFS returns NIL.

; To call MC-DFS to solve the original problem, one would call (MC-DFS '(3 3 T)
; NIL) -- however, it would be possible to call MC-DFS with a different initial
; state or with an initial path.

; Examples of calls to some of the helper functions can be found after the code.

; *****************************************************************************

; FINAL-STATE takes a single argument (S), the current state, and returns T if
; it is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
	(equal s '(3 3 NIL))
)
; Check if current state equals (3 3 NIL)

; *****************************************************************************

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), a number of
; missionaries to move (M), and a number of cannibals to move (C). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).

(defun next-state (s m c)
	(let ((state (cons (+ (- 3 (first s)) m) (cons (+ (- 3 (second s)) c) (cons (NOT (third s)) NIL)))))
		(cond 
			; If missionary count on that side is 0, can move cannibals over
			((AND (equal (first state) 0) (NOT (< (second state) c))) (list state))
			((AND (equal (- (first s) m) 0) (NOT (< (second s) c))) (list state))
			((< (first state) (second state)) NIL) ; More cannibals than missionaries on curr side
			((< (- (first s) m) (- (second s) c)) NIL) ; More cannibals than missionaries on prev side
			((< (first s) m) NIL) ; Trying to move more missionaires than are on this side of the river
			((< (second s) c) NIL) ; Trying to move more cannibals than are on this side of the river
			(t (list state)) ; Else, return the state
		)
	)
)

; First compute what the state would be:
	; Subtract the current number of cannibals and missionaries from 3 to get the number 
	; on the other side, then add the number of cannibals/missionaries that you will send to that side
; Check that the state is valid:
	; Allowed to have more cannibals than missionaries on a side if missionary count = 0
	; But if more cannibals than missionaries on curr or prev side, that's not allowed
	; Nor is trying to move more missionaires/cannibals than are on this side of the river
	; Else, return the state

; *****************************************************************************

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (S), which encodes the current state, and
; returns a list of states that can be reached by applying legal operators to
; the current state.

(defun succ-fn (s)
	(append 
		(next-state s 1 1)
		(next-state s 1 0)
		(next-state s 2 0)
		(next-state s 0 1)
		(next-state s 0 2)
	)
)
; Calls next-state on each of the 5 possible moves (either 2 missionaries, 2 cannibals, 1 missionary, 
	; 1 cannibal, or one missionary and one cannibal)
; Appends the results/lists and returns it

; *****************************************************************************

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by MC-DFS (STATES). It returns T if S is a member of
; STATES and NIL otherwise.

(defun on-path (s states)
	(cond ((null states) NIL)
		(t (OR (equal (first states) s) (on-path s (cdr states)))) ;  
	)
)

; Base case: if states is null, return nil
; Else, check if the first state is equal to the current state, and if not, recursively
	; call on the rest of the states

; *****************************************************************************

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: the path
; from from the initial state to the current state (PATH), and the legal
; successor states to the last state on PATH (STATES). PATH is a first-in
; first-out list of states; that is, the first element is the initial state for
; the current search and the last element is the most recent state explored.
; MULT-DFS does a depth-first search on each element of STATES in turn. If any
; of those searches reaches the final state, MULT-DFS returns the complete path
; from the initial state to the goal state. Otherwise, it returns NIL.

(defun mult-dfs (states path)
	(cond ((null states) NIL)
		((on-path (first states) path) (mult-dfs (rest states) path))
		((final-state (first states)) (append path (list (first states))))
		((mult-dfs (succ-fn (first states)) (append path (list (first states)))) (mult-dfs (succ-fn (first states)) (append path (list (first states)))))
		(t (mult-dfs (rest states) path))
	)
)
; Go through each state and see if it reaches final state
	; Append path as go through
; Check if any of the states are goal state
	; If so, append goal state to path, return.
	; Else, see if there is a path from this state to the end
; Else, recursively call mult-dfs with the rest of the states

; *****************************************************************************

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.

(defun mc-dfs (s path)
	(cond ((final-state s) (append path (list s)))
		(t (AND (NOT (on-path s path)) (mult-dfs (succ-fn s) (append path (list s)))))
	)
)

; Check if the current state is the final state. 
	; If so, return the path appended to the current state
; Else, check that you have not already visited that state by invoking on-path
	; Then find the path to the end state via mult-dfs, appending it to s

; *****************************************************************************

; Function execution examples

; Applying this operator would result in an invalid state, with more cannibals
; than missionaries on the east side of the river.
; (next-state '(3 3 t) 1 0) -> NIL

; Applying this operator would result in one cannibal and zero missionaries on
; the west side of the river, which is a legal operator. (note that NEXT-STATE
; returns a LIST of successor states, even when there is only one successor)
; (next-state '(3 3 t) 0 1) -> ((0 1 NIL))

; SUCC-FN returns all of the legal states that can result from applying
; operators to the current state.
; (succ-fn '(3 3 t)) -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
; (succ-fn '(1 1 t)) -> ((3 2 NIL) (3 3 NIL))
