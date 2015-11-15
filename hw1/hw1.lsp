;Inputs: a number N and a list tree. Checks whether N appears in list TREE. Returns a boolean.
(defun TREE-CONTAINS (N TREE)
	(cond ((NULL TREE) NIL)
		((atom TREE) (equal N TREE))
		((equal N (second TREE)))
		(t (OR (TREE-CONTAINS N (first TREE)) (TREE-CONTAINS N (third TREE))))
	)
)

; If the tree is empty, return NIL
; If there is only one item in the tree, see if it is equal to N
; Recursively break up the left and right trees to compare individual elements to N


;Inputs: an ordered tree TREE. Returns the maximum number appearing in the ordered tree TREE
(defun TREE-MAX (TREE)
	(cond ((NULL TREE) NIL)
		((atom TREE) TREE)
		(t (let ((lastElem (first (last TREE))))
			(cond ((atom lastElem) lastElem)
				(t (TREE-MAX (last lastElem))))))
	)	
)

; If list is empty, return NIL
; If it only has one item, then return that item
; Otherwise, store the last element in lastElem
	; If it is an atom, return it
	; Else, recursively call TREE-MAX with the end of lastElem


;Inputs: an ordered tree TREE. Returns an in-ordered list of the numbers appearing in the ordered tree TREE.
(defun TREE-ORDER (TREE)
	(cond ((NULL TREE) NIL)
		((atom TREE) (cons TREE '()))
		(t (append (TREE-ORDER (first TREE)) (cons (second TREE) (TREE-ORDER (third TREE)))))
	)
)

; If the tree is empty, return NIL
; If there is only a single element, turn it into a list
; Append the result of combining the middle element and list made from the right subtree with the list made from the left subtree

;Inputs: list L and two non-negative integers START and LEN. Returns the sub-list of L starting at position START and having length LEN. 
;Assume that first element of L has position 0.
(defun SUB-LIST (L START LEN)
	(cond ((equal LEN 0) NIL)
		((> START (length L)) NIL)
		((equal START 0) (cons (car L) (SUB-LIST (cdr L) START (- LEN 1))))
		(t (SUB-LIST (cdr L) (- START 1) LEN))
	)	
)

; Base case: Stop when the list is empty
; When you reach the starting spot, then construct the list until LEN is 0
; If not at starting spot, keep incrementing START until reach starting spot

;Inputs: list L. Returns a list of two lists L1 and L2 (L is the result of appending L1 and L2)
	;NOTE: Length of L2 minus length of L1 is 0 or 1.
(defun SPLIT-LIST (L)
		(cond ((equal (length L) 0) NIL) 
		((oddp (length L)) ;5
		  (let* (
		  	(L1 (SUB-LIST L 0 (/ (- (length L) 1) 2))) ; 
		  	(L2 (SUB-LIST L (length L1) (- (length L) (length L1))))
		  )
			(cons L1 (cons L2 NIL)))
		)
		(t 		  
			(let* (
		  	(L1 (SUB-LIST L 0 (/ (length L) 2)))
		  	(L2 (SUB-LIST L (length L1) (- (length L) (length L1))))
		  )
			(cons L1 (cons L2 NIL))))
		)
)

; Compute the length of the list
; If it is odd, subtract one from the length before splitting
; The first half of the list is L1, so call sublist with half the length 
; The second half of the list is L2, so call sublist with the remaining half
; Combine the two together

;Inputs: a binary tree TREE. Returns the height of TREE
(defun BTREE-HEIGHT (TREE)
	(cond ;((NULL TREE) NIL)
		((atom TREE) 0)
		(t 
			(let ((left (BTREE-HEIGHT (first TREE)))
				(right (BTREE-HEIGHT (second TREE))))

				(cond ((> left right) (+ 1 left))
					(t (+ 1 right)))
			))))

; Base case: single element -> 0
; Else, split tree into left and right
; Add one to whichever of the sides of the tree is longer

; Inputs: non-empty list of atoms LEAVES. Returns a binary tree such that the tree leaves are the elements of LEAVES
; For any internal (non-leaf) node in the tree, the number of leaves in its right branch minus left branch is 0 or 1
(defun LIST2BTREE (LEAVES)
	(cond ((equal (length LEAVES) 1) (car LEAVES))
		((equal (length LEAVES) 2) LEAVES)
		(t (let ((split (SPLIT-LIST LEAVES)))
				(list (LIST2BTREE (car split)) (LIST2BTREE (cadr split)))
			)
		)
	)
)

; If only one element - return that single element (not in a list)
; If only two elements - return the list that contains those two elements
; Otherwise, recursively split the list, appending the results as you go along, until everything is a list of length two or atom

; Inputs: binary tree TREE. Returns a list of atoms (inverse of List2BTree)
(defun BTREE2LIST (TREE)
	(cond ((NULL TREE) NIL)
		((atom TREE) (cons TREE '()))
		(t (append (BTREE2LIST (car TREE)) (BTREE2LIST (cadr TREE))))
	)
)

; If only one element, make it a list with that element
; Otherwise, continuously go through the list and find all the root elements and append them together








