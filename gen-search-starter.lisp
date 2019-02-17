;;; ========================================
;;;   CMPU-365, Spring 2019
;;;   Asmt. 2
;;;   Parker Zimmerman
;;; ========================================
;;;   FILE:  gen-search-starter.lisp
;;; ========================================
;;;   General tree-search algorithm.  Special cases
;;;   include breadth-first and depth-first search.

;;;  The "basic-defns" file definethe PROBLEM and NODE
;;;  data structures and some related low-level functions.

(load "basic-defns" :verbose nil)

;;;   CYCLE? 
;;; -------------------------------------------------------------
;;;  INPUTS:  STATE, a problem state
;;;           NODE, a search node
;;;           STATE-EQ-FUNC, a function that determines whether
;;;             two states are equal
;;;  OUTPUT:  T if the given STATE is the same as the state of
;;;    the given NODE or the state of any of NODE's ancestors.
;;;    Uses STATE-EQ-FUNC to determine whether two states are equal.
;;;    Otherwise returns NIL. 
;;;    NOTE:  If NODE is NIL it returns NIL.

(defun cycle? (state node state-eq-func)
  (cond
   ;; BASE CASE: we have made through the whole branch
   ((null node)
    NIL)
   ;; BASE CASE 2: we have found a node in the branch with the
   ;; same state
   ((funcall state-eq-func state (node-state node))
    T)
   ;; RECURSIVE CASE: have not found a node with the same state
   (T
    (cycle? state (node-parent node) state-eq-func))))

;;;  MAKE-ROOT-NODE 
;;; ---------------------------------------
;;;  INPUT:  PROB, a search problem
;;;  OUTPUT:  A search NODE that will be the ROOT NODE of a
;;;           brand new search tree.

(defun make-root-node (prob)
  (make-node :state (search-problem-init-state prob)))

;;;  EXPAND 
;;; ---------------------------------
;;;  INPUTS:  NODE, a search node
;;;           ACTS, a list of actions
;;;           ST-EQ-FUNC, a function for testing whether two
;;;              states are equal
;;;  OUTPUT:  A list of child nodes obtained by applying the
;;;           given list of actions to NODE's state.  However, it
;;;           does *NOT* include child nodes whose states already
;;;           appear on the path from the root node to NODE.
;;;           (Uses CYCLE? to determine this.)

(defun expand (node acts st-eq-func)
  ;; You are encouraged to use LABELS to define an accumulator-based
  ;; recursive helper function, called EXPAND-ACC.  By defining your
  ;; helper function within the body of EXPAND, you will have access
  ;; to NODE, ACTS and ST-EQ-FUNC, should you need them.  Alternatively,
  ;; you could define EXPAND-ACC as a stand-alone helper function.
  ;; In that case, you may need to provide it with additional inputs.
  (labels ((expand-acc (node rem-acts st-eq-func acc)
               (cond
                ;; BASE CASE: have performed all of the actions
                ((null rem-acts)
                 acc)
                ;; RECURSIVE CASE 1: in this state the first action
                ;; is an illegal action
                ((null (funcall (first rem-acts)
                                (node-state node)))
                  (expand-acc node (rest rem-acts) st-eq-func acc))
                ;; RECURSIVE CASE 2: in this state performing the first
                ;; action would cause a cycle
                ((cycle?  (funcall (first rem-acts)(node-state node))
                          node
                          st-eq-func)
                 (expand-acc node (rest rem-acts) st-eq-func acc))
                ;; RECURSIVE CASE 3: add the node created by performing the
                ;; action to the list of child nodes
                (T
                 (expand-acc node
                             (rest rem-acts)
                             st-eq-func
                             (cons (make-node :state (funcall (first rem-acts)
                                                              (node-state node))
                                              :parent node
                                              :action (first rem-acts)
                                              :depth (+ (node-depth node) 1))
                                   acc))))))
    (expand-acc node acts st-eq-func NIL)))


;;; --------------------------------------------------------
;;;  GEN-SEARCH-GUTS
;;; ---------------------------------------------------------------
;;;  INPUTS: PROBLEM, a search problem
;;;          QUEUE-FN, a queuing function (used to insert newly
;;;            created nodes into the search queue).  The queuing
;;;            function determines which kind of search we're doing.
;;;          REZZY, a RESULTS struct
;;;  OUTPUT: The RESULTS struct, with its fields updated to include
;;;          info about the search (e.g., num nodes explored; the goal
;;;          node, if found; and timing information).
;;; ---------------------------------------------------------------
;;;  This function performs the indicated search problem using
;;;  the search strategy determined by QUEUE-FN.

(defun gen-search-guts (problem queue-fn rezzy)
  (labels ((gen-search-helper (queue)
             (let* (( curr-node (first queue)))
	       (cond
		        ;;BASE CASE 1: the search queue is empty
		        ((null queue)
		          nil)
		        ;; BASE CASE 2: we have found the goal node
		        ((funcall (search-problem-goal-test-func problem) (node-state curr-node))
		          (setf (results-goal-node rezzy) curr-node))
		        ;; RECURSIVE CASE: still searching for goal node
		        (T               
		          ;; create children by expanding the current node
		          ;; add the children to the queue
		        (let* ((child-list (expand curr-node
					                        (search-problem-actions problem)
					                        (search-problem-state-eq-func problem))))
		          ;; update the results struct adding the number of 
		          ;; children to the number of nodes                
		          (setf (results-num-nodes rezzy)
		                (+ (results-num-nodes rezzy)
			                 (length child-list)))
		          (gen-search-helper (funcall queue-fn (rest queue) child-list))))))))
              ;; create a root node to add to the queue
              (setf root (make-root-node problem))
              (setf (results-num-nodes rezzy) 1)
              (gen-search-helper (cons root nil))))


