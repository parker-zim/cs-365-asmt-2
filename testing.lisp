;;; --------------------------------------------
;;;  CMPU-365, Spring 2019
;;;  Asmt. 2 
;;;  Testing File for Parker Zimmerman
;;; --------------------------------------------
;;;  To compile and load all files and run a few tests,
;;;  simply load this file:  (load "testing.lisp" :verbose nil)


(load "asmt-helper.lisp" :verbose nil)

(header "Parker Zimmerman" 2)

;; The following expressions ensure that tail-recursive function calls 
;; are handled appropriately/efficiently by the compiler.  

(setq compiler:tail-call-self-merge-switch t)
(setq compiler:tail-call-non-self-merge-switch t) 

(defparameter *my-files* (list "basic-defns"
			       "gen-search-starter"
			       "vw"
			       "test-vw"
			       "mc"
			       ))

;; COMPILE and LOAD all of the RELEVANT files

(tester '(maker *my-files*))
(newline)

;; --------------------------------
(problem "Gen Search Testing")
;; --------------------------------

(setf problem (make-search-problem 
   				:init-state (make-vw-state :rob-loc '(0 0)
			    						   :dirt-locations '((0 0) (0 3) (1 2) (3 1)))
   			    :actions (list #'north #'south #'east #'west #'vacuum)
   				:goal-test-func #'vw-goal-test
   				:state-eq-func #'vw-state-equal?))
(setf root (make-root-node problem))

(setf node-one (make-node :state (make-vw-state :rob-loc '(0 0)
			    						 :dirt-locations '((0 3) (1 2) (3 1)))
						  :parent root))

(setf node-two (make-node :state (make-vw-state :rob-loc '(0 1)
			    						 :dirt-locations '((0 3) (1 2) (3 1)))
						  :parent node-one))

(setf node-three (make-node :state (make-vw-state :rob-loc '(0 2)
			    						 :dirt-locations '((0 3) (1 2) (3 1)))
						  :parent node-two))


(tester '(cycle? (make-vw-state :rob-loc '(0 1)
			    	   :dirt-locations '((0 3) (1 2) (3 1)))
		node-three 
		(search-problem-state-eq-func problem)))


(tester '(cycle? (make-vw-state :rob-loc '(1 2)
			    	   :dirt-locations '((0 3) (1 2) (3 1)))
		node-three 
		(search-problem-state-eq-func problem)))
(tester '(expand node-three (search-problem-actions problem) 
							(search-problem-state-eq-func problem)))
;; ---------------------------------
(problem "Vacuum World Testing")
;; ---------------------------------

(tester '(do-vw-depth))
(newline)
(tester '(do-vw-breadth))

;; ---------------------------------
(problem "Missionaries and Cannibals Testing")
;; ---------------------------------

(tester '(do-mc-depth))
(newline)
(tester '(do-mc-breadth))
