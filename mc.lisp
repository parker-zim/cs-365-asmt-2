;;; =============================================================
;;;  CMPU-365, Spring 2019
;;;  Asmt. 2
;;;  Parker Zimmerman
;;;  FILE:  mc.lisp
;;; =============================================================
;;;  Implementation of the MISSIONARIES AND CANNIBALS WORLD
;;; =============================================================

(defconstant TOTAL-CANNIBALS 3)
(defconstant TOTAL-MISSIONARIES 3)

;;;  mc-state data structure
;;; ---------------------------
;;;   M-LEFT -- number of missionaries on the left
;;;   M-RIGHT -- number of misionaries on the right
;;;   C-LEFT -- number of cannibals on the left
;;;   C-RIGHT -- number of cannibals on the right
;;;   BOAT-LOC -- location of the boat

(defstruct (mc-state 
    (:print-function print-mc-state))
    m-left
    m-right
    c-left
    c-right
    boat-loc)


;;;  PRINT-MC-STATE
;;; ---------------------------------------------------
;;;  To be linked to the MC-STATE struct, this function is required
;;;  to take the following three inputs:
;;;   INPUTS:  STATE, a vacuum-world state struct
;;;            STR, an output stream to print to (usually t)
;;;            DEPTH, printing depth (we'll ignore this)
;;;   OUTPUT:  None
;;;   SIDE-EFFECT:  Displays the given MC-STATE struct.

(defun print-mc-state (state str depth)
;; Tell the compiler not to warn us about the fact that we 
;; don't make use of DEPTH
  (declare (ignore depth))
  (let 
    ( (m-left 
        (mc-state-m-left state))
      (m-right 
        (mc-state-m-right state))
      (c-left 
        (mc-state-c-left state))
      (c-right 
        (mc-state-c-right state))
      (boat-loc 
        (mc-state-boat-loc state)))
    (format str "M-Left: ~A, " m-left)
    (format str "M-Right: ~A, " m-right)
    (format str "C-Left: ~A, " c-left)
    (format str "C-Right: ~A, " c-right)
    (format str "Boat-Loc: ~A~%" boat-loc)))


;;;  MC-STATE-EQUAL?
;;; ---------------------------------------------------
;;;   INPUT:  STATE1, STATE2 -- Two MC-WORLD structs
;;;   OUTPUT:  T if the given MC-STATES have the same contents, NIL otherwise.

(defun mc-state-equal? (state1 state2)
  (and
    ;; missionaries on left are equal
    (equal 
      (mc-state-m-left state1)
      (mc-state-m-left state2))
    ;; missionaries on right are equal
    (equal 
      (mc-state-m-right state1)
      (mc-state-m-right state2))
    ;; cannibals on left are equal
    (equal 
      (mc-state-c-left state1)
      (mc-state-c-left state2))
    ;; cannibals on right are equal
    (equal 
      (mc-state-c-right state1)
      (mc-state-c-right state2))
    ;; boat is in the same location
    (equal 
      (mc-state-boat-loc state1)
      (mc-state-boat-loc state2)))) 


;;;  MC-GOAL?
;;; ----------------------------------------------------
;;;   INPUT:  STATE, a MC-STATE struct
;;;   OUTPUT:  T if STATE is a goal-state.

(defun mc-goal? (state)
  (and (equal (mc-state-m-right state) 3)
       (equal (mc-state-c-right state) 3)))

;;;  VALID-MC
;;; --------------------------
;;;  A helper function for the DO-MC-MOVE function.
;;;  INPUTS:  M-LEFT, M-RIGHT, C-LEFT, C-RIGHT -- number of missionaries and cannibals 
;;;           on the left and the right
;;;  OUTPUT:  T if combination of missionaries and cannibals on each side is acceptable
;;;  Note:    missionaries and cannibals must always equal 3 and you cannot have negative
;;;           missionaries or cannibals on either side

(defun valid-mc (m-left m-right c-left c-right)
  (and (or (equal m-right 0)
           (equal m-left 0)
           (and (>= m-right c-right)
                (>= m-left c-left)))
       (eq (+ m-left m-right) TOTAL-MISSIONARIES)
       (eq (+ c-left c-right) TOTAL-CANNIBALS)
       (>= m-right 0)
       (>= m-left 0)
       (>= c-right 0)
       (>= c-left 0)))

;;;  DO-MOVE
;;; -------------------------------------------------
;;;   A generic move operator.  Various values of DELTA-X and
;;;   DELTA-Y determine which way robbie moves.  Returns the
;;;   new child state, or NIL.
;;;     INPUTS:  ORIG-STATE, original state of the vacuum world
;;;              DELTA-M-L, DELTA-M-R -- amount the missionaries on each side changes
;;;              DELTA-C_R, DELTA-C-L -- amount the cannibals on each side changes
;;;     OUTPUT:  The resulting state (if the move was legal)
;;;              NIL if the move was not legal.

(defun do-mc-move (orig-state delta-m-l delta-m-r delta-c-l delta-c-r)
  ;; given the move change the numbers of missionairies and cannibals on each side
  ;; accordingly and move the boat
  (let* 
    ( (new-m-l 
        (+ (mc-state-m-left orig-state) delta-m-l))
      (new-m-r 
        (+ (mc-state-m-right orig-state) delta-m-r))
      (new-c-l 
        (+ (mc-state-c-left orig-state) delta-c-l))
      (new-c-r 
        (+ (mc-state-c-right orig-state) delta-c-r))
      (new-boat-loc 
        (cond
          ((eq (mc-state-boat-loc orig-state) -1)
            1)
          (T
            -1))))
    ;; if move would be valid (valid numbers of ms and cs and the boat was in the
    ;; correct location)...
    (if 
      (and (valid-mc new-m-l new-m-r new-c-l new-c-r)
        (or (and (or  (> delta-m-l 0)
                      (> delta-c-l 0))
                 (eq (mc-state-boat-loc orig-state) 1))
            (and (or  (> delta-m-r 0)
                      (> delta-c-r 0))
                 (eq (mc-state-boat-loc orig-state) -1))))
;; then create a new child state and return it...
;; NOTE: We use SAME dirt-locations list, not a copy.
;;       This is okay because we're non-destructive!
      (make-mc-state :m-left new-m-l
                     :m-right new-m-r
                     :c-left new-c-l
                     :c-right new-c-r
                     :boat-loc new-boat-loc)
;; otherwise, return nil
      nil)))

;;;  The ONE-M-LEFT, ONE-M-RIGHT, ONE-C-LEFT, ONE-C-RIGHT, TWO-M-LEFT
;;;  TWO-M-RIGHT, TWO-C-LEFT, TWO-C-RIGHT, BOTH-LEFT  and BOTH-RIGHT
;;;  move operators (they use DO-MOVE)
;;; --------------------------------------------------------------------

(defun one-m-left 
  (orig-state) 
  (do-mc-move orig-state 1 -1 0 0))

(defun one-m-right 
  (orig-state) 
  (do-mc-move orig-state -1 1 0 0))

(defun one-c-left 
  (orig-state) 
  (do-mc-move orig-state 0 0 1 -1))

(defun one-c-right 
  (orig-state) 
  (do-mc-move orig-state 0 0 -1 1))

(defun two-m-left 
  (orig-state) 
  (do-mc-move orig-state 2 -2 0 0))

(defun two-m-right 
  (orig-state) 
  (do-mc-move orig-state -2 2 0 0))

(defun two-c-left 
  (orig-state) 
  (do-mc-move orig-state 0 0 2 -2))

(defun two-c-right 
  (orig-state) 
  (do-mc-move orig-state 0 0 -2 2))

(defun both-left 
  (orig-state) 
  (do-mc-move orig-state 1 -1 1 -1))

(defun both-right 
  (orig-state) 
  (do-mc-move orig-state -1 1 -1 1))

;;; ===============================================================
;;;   Helper functions for testing.
;;;   When ready to test, call:  (DO-MC-DEPTH) or (DO-MC-BREADTH).
;;; ===============================================================

;;;  MAKE-MC-PROBLEM
;;; ----------------------------------------------------
;;;   Defines an instance of a search problem for the Vacuum World.
;;;   Use a simple problem instance until you get things working.

(defun make-mc-problem ()
  (make-search-problem 
    :init-state (make-mc-state :m-left 3
                               :m-right 0
                               :c-left 3
                               :c-right 0
                               :boat-loc -1)
                               :actions (list #'one-m-left #'one-m-right #'one-c-left #'one-c-right 
                                              #'two-m-left #'two-m-right #'two-c-left #'two-c-right
                                              #'both-left #'both-right)
                               :goal-test-func #'mc-goal?
                               :state-eq-func #'mc-state-equal?))


;;  DO-MC-DEPTH
;; ------------------------------------------
;;  INPUTS:  None.
;;  OUTPUT:  A RESULTS struct that contains info about what
;;           happened from running Depth-First Search on the
;;           Vacuum World problem.

(defun do-mc-depth ()
  (depth-first-search 
    (make-mc-problem)))

;;  DO-MC-BREADTH
;; ------------------------------------------
;;  INPUTS:  None.
;;  OUTPUT:  A RESULTS struct that contains info about what
;;           happened from running Breadth-First Search on the
;;           Vacuum World problem.

(defun do-mc-breadth ()
  (breadth-first-search 
    (make-mc-problem)))