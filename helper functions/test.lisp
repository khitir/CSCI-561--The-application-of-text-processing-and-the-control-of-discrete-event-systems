;;;;;;;;;;;;;;;;;;;
;;;; Utilities ;;;;
;;;;;;;;;;;;;;;;;;;

(defun hash-table-keys (hash-table)
  "Return the hash table's keys as a list."
  (let ((result nil))
    (maphash (lambda (key value)
               (declare (ignore value))
               (push key result))
             hash-table)
    result))

(defun make-symbol-hash-table ()
  "Convenience function to make a hash table for FA symbols."
  (make-hash-table :test #'equal))

(defun fold-left (function initial-value list)
  "Convenience function for fold-left with an initial value."
  (reduce function list :initial-value initial-value))

(defun TODO (thing)
  "Placeholder for code to implement."
  (error "Unimplemented: ~A" thing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; STARTER DEFINITIONS FOR FINITE AUTOMATA ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A structure type for finite automata
(defstruct finite-automaton
  "A Finite Automaton."
  states    ; state set as a list
  alphabet  ; input alphabet as a list
  edges     ; list of edges: (state-0 input-symbol state-1)
  delta     ; transition function : state-0 * input-symbol -> state-1
  start     ; start state
  accept)   ; accept set as a list

(defun make-fa (edges start accept)
  "Convenience constructor for finite automata"
  (flet ((add-state (hash state)
           (setf (gethash state hash) t)
           hash)
         (add-edge-states (hash edge)
           (destructuring-bind (state-0 input-symbol state-1) edge
             (declare (ignore input-symbol))
             (setf (gethash state-0 hash) t
                   (gethash state-1 hash) t))
           hash)
         (add-edge-input-symbol (hash edge)
           (destructuring-bind (state-0 input-symbol state-1) edge
             (declare (ignore state-0 state-1))
             (setf (gethash input-symbol hash) t))
           hash)
         (add-edge-transition (hash edge)
           (destructuring-bind (state-0 input-symbol state-1) edge
             (push state-1 (gethash (cons state-0 input-symbol) hash)))
           hash))
    (let ((state-hash (fold-left #'add-edge-states
                                 (fold-left #'add-state
                                            (add-state (make-symbol-hash-table)
                                                       start)
                                            accept)
                                 edges))
          (alphabet-hash (fold-left #'add-edge-input-symbol
                                    (make-symbol-hash-table)
                                    edges))
          (edge-hash (fold-left #'add-edge-transition
                                (make-symbol-hash-table)
                                edges)))
      (make-finite-automaton
       :states (hash-table-keys state-hash)
       :alphabet (hash-table-keys alphabet-hash)
       :edges edges
       :delta (lambda (state-0 input-symbol)
                (gethash (cons state-0 input-symbol) edge-hash))
       :start start
       :accept accept))))


;;; Higher-order conveience functions for Finite Automata ;;;

(defun map-fa-states (function fa)
  "Map FUNCTION over the FA's states."
  (map 'list function (finite-automaton-states fa)))

(defun map-fa-accept (function fa)
  "Map FUNCTION over the FA's accept states."
  (map 'list function (finite-automaton-accept fa)))

(defun fold-fa-states (function initial-value fa)
  "Fold FUNCTION over the FA's states."
  (fold-left function initial-value (finite-automaton-states fa)))

(defun fold-fa-alphabet (function initial-value fa)
  "Fold FUNCTION over the FA's alphabet."
  (fold-left function initial-value (finite-automaton-alphabet fa)))

(defun map-fa-edges (function fa)
  "Map FUNCTION over the FA's edges."
  (map 'list
       (lambda (edge)
         (destructuring-bind (state-0 input-symbol state-1) edge
           (funcall function state-0 input-symbol state-1)))
       (finite-automaton-edges fa)))

;;; Graphviz Output ;;;

(defun fa-dot (fa place)
  "Output a Graphviz dot file of FA."
  (let ((hash (make-symbol-hash-table)))
    ;; number the states
    (fold-fa-states (lambda (i state)
                      (setf (gethash state hash) i)
                      (1+ i))
                    0 fa)
    ;; Output the Graphviz dot file
    (labels ((state-number (state)
               (gethash state hash))
             (dot-symbol (thing) ; Pretty-print Greek letters
               (case thing
                 (:alpha "&alpha;")
                 (:beta "&beta;")
                 (:gamma "&gamma;")
                 (:delta "&delta;")
                 (:epsilon "&epsilon;")
                 (:zeta "&zeta;")
                 (:eta "&eta;")
                 (:theta "&theta;")
                 (:iota "&iota;")
                 (:kappa "&kappa;")
                 (:lambda "&lambda;")
                 (:mu "&mu;")
                 (:nu "&nu;")
                 (:xi "&xi;")
                 (:omicron "&omicron;")
                 (:pi "&pi;")
                 (:rho "&rho;")
                 (:sigma "&sigma;")
                 (:tau "&tau;")
                 (:upsilon "&upsilon;")
                 (:phi "&phi;")
                 (:chi "&chi;")
                 (:omega "&omega;")
                 (t thing)))
             (helper (stream)
               ;; output
               (format stream "~&digraph { ~%")
               ;; state labels
               (format stream "~:{~&  ~A[label=\"~A\"];~}"
                       (map-fa-states (lambda (state)
                                        (list (state-number state)
                                              state))
                                      fa))
               ;; start state
               (format stream "~&  start[shape=none];")
               (format stream "~&  start -> ~A;"
                       (state-number (finite-automaton-start fa)))
               ;; accept state
               (format stream "~:{~&  ~A [ shape=~A ];~}"
                       (map-fa-accept (lambda (q)
                                        (list (state-number q) "doublecircle"))
                                      fa))
               ;; edges
               (format stream "~:{~&  ~A -> ~A [fontsize=~D,label=\"~A\"];~%~}"
                       (map-fa-edges (lambda (state-0 input-symbol state-1)
                                       (list (state-number state-0)
                                             (state-number state-1)
                                             12 (dot-symbol input-symbol)))
                                     fa))
               ;; end
               (format stream "~&}~%")))
      (cond
        ((streamp place)
         (helper place))
        ((eq place t)
         (helper *standard-output*))
        ((null place)
         (with-output-to-string (s)
           (helper s)))
        ((or (stringp place)
             (pathnamep place))
         (with-open-file (stream place
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
         (helper stream)))
        (t (error "Unrecognized output type: ~A" place))))))



#+sbcl
(defun fa-pdf (fa filename)
  "Create a PDF of FA."
  (with-input-from-string (text (fa-dot fa nil))
    (sb-ext:run-program "dot" (list "-Tpdf")
                        :search t
                        :input text
                        :output filename
                        :if-output-exists :supersede)))


;;; Finite Automata Helper Functions ;;;

(defun fa-transition (fa state-0 input-symbol)
  "Return the list of successors of STATE-0 on INPUT-SYMBOL."
  (funcall (finite-automaton-delta fa)
           state-0 input-symbol))

(defun dfa-transition (fa state-0 input-symbol)
  "Return the successor of STATE-0 on INPUT-SYMBOL."
  (assert (not (eq input-symbol :epsilon)))
  (let ((successors (fa-transition fa state-0 input-symbol)))
    ;; DFA cannot have multiple successors, or it would be
    ;; nondeterministic
    (assert (or (null successors)
                (null (cdr successors))))
    (car successors)))

(defun newstate (&optional (arg "Q-"))
  "Construct a unique state for a finite automaton."
  (gensym arg))

(defun dfa-p (fa)
  "Is FA is deterministic?"
  (labels ((rec (hash edges)
             ;; Recurse over the edges and check for nondeterministic
             ;; transitions.
             ;; hash : (CONS state-0  input-symbol) -> (or T NIL)
             (if edges
                 (destructuring-bind ((state-0 input-symbol state-1) &rest edges) edges
                   (declare (ignore state-1))
                   (let ((key (list state-0 input-symbol)))
                     (unless (or (eq input-symbol :epsilon)
                                 (gethash key hash))
                       (setf (gethash key hash) t)
                       (rec hash edges))))
                 t)))
    (and (finite-automaton-p fa)
         (rec (make-symbol-hash-table)
              (finite-automaton-edges fa)))))

(defun dfa-add-reject (dfa &optional (alphabet (finite-automaton-alphabet dfa)))
  "Add a reject state to DFA."
  (assert (dfa-p dfa))
  ;; Index non-dead-state edges
  ;; hash : (CONS state-0 input-symbol) -> (OR T NIL)
  (let ((dead-state (newstate "dead")))
    ;; Create edges to a dead-state
    (let ((edges (fold-left (lambda (edges input-symbol) ; fold over alphabet
                              (fold-left (lambda (edges state) ; fold over states
                                           (if (fa-transition dfa state input-symbol)
                                               edges
                                               (cons (list state input-symbol dead-state)
                                                     edges)))
                                         ;; dead-state self transition
                                         (cons (list dead-state input-symbol dead-state)
                                               edges)
                                         (finite-automaton-states dfa)))
                            (finite-automaton-edges dfa)
                            alphabet)))
      ;; Result
      (make-fa  edges
                (finite-automaton-start dfa)
                (finite-automaton-accept dfa)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; COMPLETE THE FUNCTIONS BELOW ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 0: DFA Simulation ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Finite Automata Lecture: Algorithm 1
(defun dfa-simulate (dfa sequence)
  "True if DFA accepts SEQUENCE."
  (assert (dfa-p dfa))
  (labels ((edelta (current-state remaining-sequence)
             (if (null remaining-sequence)
                 current-state
               (let* ((next-symbol (first remaining-sequence))
                      (next-state (dfa-transition dfa current-state next-symbol)))
                 (if next-state
                     (edelta next-state (rest remaining-sequence))
                     nil)))))
    (let ((final-state (edelta (finite-automaton-start dfa)
                               (coerce sequence 'list))))
      (if (and final-state (find final-state (finite-automaton-accept dfa) :test #'equal))
          t
          nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 1: NFA Subset Construction ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun e-closure (nfa s c)
  "Compute the epsilon-closure of a set of states S in NFA."
  (let ((c (or c '())))
    (labels ((visit (q)
               (unless (member q c :test #'equal)
                 (push q c)
                 (dolist (next (fa-transition nfa q :epsilon))
                   (visit next)))))
      (dolist (q s)
        (visit q))
      c)))





#|
(defun move-e-closure (nfa s a)
  "Compute the set of states reachable from S on input symbol A, including epsilon-closures."
  ;; First, find all the states reachable from S on input A
  (let ((next-states '()))
    (dolist (q s)
      (dolist (next (fa-transition nfa q a))
        (unless (member next next-states :test #'equal)
          (push next next-states))))
    ;; Then compute the epsilon-closure of these next-states
    (e-closure nfa next-states nil)))
|#

(defun move-e-closure (nfa s a)
  ;; s is the set of states
  ;; a is the symbol
  (labels 
    (
      ;; Define helper function nfa-next
      ;; N.B: nfa declared as input to outer nfa-simulate function, so is defined here by virtue of that
      (nfa-next (curr-states sym next-states)
        (if curr-states ;if curr-states has any states in it
          ; apply nfa-next again to remaining curr-states after joining all the states reachable on symbol sym to the next-states 
          (let* 
            (
              (remain-states (cdr curr-states)) ; the states remaining after traversing from the symbol for the first state

              (reach-states ; the states one can reach is the union of the current next-states and the states that can be moved to from the current state on the given symbol
                  (union 
                    next-states 
                    (fa-transition nfa (car curr-states) sym)
                  )
              )
            )

            (nfa-next remain-states sym reach-states)
          )

          ; Return next-states as there is no further states left in current-states to search
          next-states
        )
      )
    )

    (e-closure nfa (nfa-next s a '()) '())
  )
)


(defun nfa-simulate (nfa sequence)
  "True if NFA accepts SEQUENCE."
  (labels ((edelta (subset list)
             (TODO 'nfa-simulate)))
    (let* ((q0 (finite-automaton-start nfa))
           (f (finite-automaton-accept nfa))
           (u (e-closure nfa (list q0) nil))
           (list (coerce sequence 'list))
           (final-subset (edelta u list)))
      (if (intersection final-subset f
                        :test #'equal)
          t
          nil))))

(defun state-predicate-atom (a b)
  "Predicate function to compare atomic states."
  (etypecase a
    ((or symbol string)
     (etypecase b
       ((or symbol string)
        (string-lessp a b))
       (number nil)))
    (number
     (etypecase b
       ((or symbol string)
        t)
       (number (<= a b))))))

(defun state-predicate (a b)
  "Predicate function to compare FA states."
  (etypecase a
    (atom (etypecase b
            (atom (state-predicate-atom a b))
            (list t)))
    (cons (etypecase b
            (atom nil)
            (cons (if (equal (car a) (car b))
                      (state-predicate (cdr a)
                                       (cdr b))
                      (state-predicate (car a)
                                       (car b))))))))
#|
(defun nfa->dfa (nfa)
  "Convert a nondeterministic finite automaton to a deterministic finite automaton."
  ;; Create a hash table to keep track of visited subsets and initialize alphabet without epsilon transitions.
  (let ((visited-hash (make-symbol-hash-table))
        (alphabet (remove :epsilon (finite-automaton-alphabet nfa))))
    ;; Define helper functions for processing subsets and transitions.
    (labels 
        ;; Sort subsets for consistent ordering, making subset comparisons simpler and efficient for hash table storage.
        ((sort-subset (u)
           ;; Sort subsets for easy testing of previously visited subsets.
           (sort u #'state-predicate))
         
         ;; Process each input symbol to find the set of reachable states (successor subset) from the current subset.
         (visit-symbol (edges subset-0 input-symbol)
           ;; Calculate the states reachable from subset-0 on input-symbol, including epsilon closures.
           (let* ((next-states (move-e-closure nfa subset-0 input-symbol))
                  (sorted-next-states (sort-subset next-states)))
             ;; If sorted-next-states hasn't been visited, recursively process it.
             (unless (gethash sorted-next-states visited-hash)
               (visit-subset edges sorted-next-states))
             ;; Add the transition to the DFA's edge set.
             (push (list subset-0 input-symbol sorted-next-states) edges)
             edges))
         
         ;; Visit each subset to add its possible transitions based on each symbol in the alphabet.
         (visit-subset (edges subset)
           ;; Mark subset as visited by storing it in the visited-hash.
           (setf (gethash (sort-subset subset) visited-hash) t)
           ;; Process each symbol in the alphabet to explore all possible transitions for the subset.
           (dolist (input-symbol alphabet)
             (setf edges (visit-symbol edges subset input-symbol)))
           edges))
      
      ;; Begin the subset construction algorithm from the initial state of the NFA.
      (let* ((q0 (finite-automaton-start nfa))
             ;; Compute the ε-closure of the start state to determine the initial DFA state.
             (initial-subset (e-closure nfa (list q0) nil))
             (sorted-initial-subset (sort-subset initial-subset))
             ;; Recursively visit all subsets reachable from the initial subset to build the DFA's transition structure.
             (edges (visit-subset nil sorted-initial-subset))
             ;; Extract all unique DFA states from the visited hash table.
             (dfa-states (hash-table-keys visited-hash))
             ;; Identify the DFA's accept states, which are subsets that contain any of the NFA's accept states.
             (dfa-accept (remove-if-not (lambda (subset)
                                          (intersection subset (finite-automaton-accept nfa) :test #'equal))
                                        dfa-states)))
        
        ;; Create and return a new DFA with the generated states, alphabet, edges, and transition function.
        (make-finite-automaton
         :states dfa-states
         :alphabet alphabet
         :edges edges
         :delta (lambda (state-0 input-symbol)
                  ;; Find the transition in the DFA that matches the input state and symbol.
                  (let ((transition (find (list state-0 input-symbol) edges :test #'equal :key #'(lambda (edge) (list (first edge) (second edge))))))
                    (third transition)))
         :start sorted-initial-subset
         :accept dfa-accept)))))

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun nfa->dfa (nfa)
  "Convert a nondeterministic finite automaton to a deterministic finite automaton."
  (let ((visited-hash (make-symbol-hash-table))
        (alphabet (remove :epsilon (finite-automaton-alphabet nfa))))
    ;; Define helper functions for processing subsets and transitions.
    (labels 
        ((sort-subset (u)
           ;; Sort subsets for easy testing of previously visited subsets.
           (sort u #'state-predicate))
         
         (visit-symbol (edges subset-0 input-symbol)
           ;; Calculate the states reachable from subset-0 on input-symbol, including epsilon closures.
           (let* ((next-states (move-e-closure nfa subset-0 input-symbol))
                  (sorted-next-states (sort-subset next-states)))
             ;; Print the current transition attempt.
             ;(format t "Processing transition: ~A --'~A'--> ~A~%" subset-0 input-symbol sorted-next-states)
             ;; Print the current state of the hash table.
             ;(format t "Visited hash table keys: ~A~%" (hash-table-keys visited-hash))
             ;; If sorted-next-states hasn't been visited, recursively process it.
             (unless (gethash sorted-next-states visited-hash)
               (setf edges (visit-subset edges sorted-next-states)))
             ;; Add the transition to the DFA's edge set and print updated edges.
             (push (list subset-0 input-symbol sorted-next-states) edges)
             ;(format t "Updated edges after push: ~A~%" edges)
             edges))
         
         (visit-subset (edges subset)
           ;; Print the subset being visited.
           ;(format t "Visiting subset: ~A~%" subset)
           ;; Mark subset as visited by storing it in the visited-hash.
           (setf (gethash (sort-subset subset) visited-hash) t)
           ;; Process each symbol in the alphabet to explore all possible transitions for the subset.
           (dolist (input-symbol alphabet)
             (setf edges (visit-symbol edges subset input-symbol)))
           ;; Print edges after visiting subset
           ;(format t "Edges after visiting subset ~A: ~A~%" subset edges)
           edges))
      
      ;; Begin the subset construction algorithm from the initial state of the NFA.
      (let* ((q0 (finite-automaton-start nfa))
             ;; Compute the ε-closure of the start state to determine the initial DFA state.
             (initial-subset (e-closure nfa (list q0) nil))
             (sorted-initial-subset (sort-subset initial-subset))
             ;; Print the initial subset.
             ;(format t "Initial subset (start state ε-closure): ~A~%" sorted-initial-subset)
             ;; Recursively visit all subsets reachable from the initial subset to build the DFA's transition structure.
             (edges (visit-subset nil sorted-initial-subset))
             ;; Extract all unique DFA states from the visited hash table.
             ;;(dfa-states (hash-table-keys visited-hash))
             ;; Extract all unique DFA states from the visited hash table, excluding NIL.
             (dfa-states (remove nil (hash-table-keys visited-hash)))
             ;; Identify the DFA's accept states, which are subsets that contain any of the NFA's accept states.
             (dfa-accept (remove-if-not (lambda (subset)
                                          (intersection subset (finite-automaton-accept nfa) :test #'equal))
                                        dfa-states)))
        
        ;; Print the final DFA details before returning.
        ;(format t "Final DFA States: ~A~%" dfa-states)
        ;(format t "Final DFA Accept States: ~A~%" dfa-accept)
        ;(format t "Final DFA Edges: ~A~%" edges)
        
        ;; Create and return a new DFA with the generated states, alphabet, edges, and transition function.
        (make-finite-automaton
         :states dfa-states
         :alphabet alphabet
         :edges edges
         :delta (lambda (state-0 input-symbol)
                  ;; Find the transition in the DFA that matches the input state and symbol.
                  (let ((transition (find (list state-0 input-symbol) edges :test #'equal :key #'(lambda (edge) (list (first edge) (second edge))))))
                    (third transition)))
         :start sorted-initial-subset
         :accept dfa-accept)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 2: Regular Expressions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We represent regular expressions as S-expressions, using the
;; following operators:
;;
;; - :union
;; - :concatenation
;; - :kleene-closure
;;
;; The union and concatenation operators are n-ary.  Kleene-closure
;; takes a single argument.
;;
;; We can represent any regular language with a regular expression
;; using only the operators :union, :concatenation, :kleene-closure.
;; However, it is often convenient (and common practice) to define
;; additional operators. The SIMPLIFY-REGEX function will take as
;; input a regular expression with such additional operators and
;; return a simplified expression that contains only the operators
;; :union, :concatenation, ang :kleene-closure.  Specifically, it will
;; simplify the following operators:
;;
;; - :.     -> (:union alphabet...)
;; - (:? X) -> (:union X :epsilon)
;; - (:+ X) -> (:concatenation X (:kleene-closure X))

(defun simplify-regex (regex &optional alphabet)
  "Convert :., :?, :+ to only :union, :concatenation, :kleene-closure"
  (labels ((h (regex)
             (cond
               ((eq regex :.)
                (assert alphabet)
                `(:union ,@alphabet))
               ((atom regex)
                regex)
               (t (destructuring-bind (operator &rest args) regex
                    (TODO 'simplify-regex))))))
    (h regex)))

;;; The functions FA-CONCATENATE, FA-UNION, and FA-REPEAT apply the
;;; corresponding regular operation (union, concatenation, and
;;; kleene-closure, respectively) to finite automata.  We will next
;;; use these functions as subroutines to convert a regular expression
;;; to an NFA.

;; Regular Expression Lecture: Concatenation.
;; Provided in complete form as an example
(defun fa-concatenate (nfa-1 nfa-2)
  "Find the concatenation of NFA-1 and NFA-2."
  (assert (not (intersection (finite-automaton-states nfa-1)
                             (finite-automaton-states nfa-2))))
  (let ((start (newstate))
        (accept (newstate)))
    (make-fa (append (list (list start :epsilon (finite-automaton-start nfa-1)))
                     (map 'list (lambda (x)
                                  (list x :epsilon (finite-automaton-start nfa-2)))
                          (finite-automaton-accept nfa-1))
                     (map 'list (lambda (x)
                                  (list x :epsilon accept))
                          (finite-automaton-accept nfa-2))
                     (finite-automaton-edges nfa-1)
                     (finite-automaton-edges nfa-2))
             start
             (list accept))))


;; Regular Expression Lecture: Union
(defun fa-union (nfa-1 nfa-2)
  "Find the union of NFA-1 and NFA-2."
  (assert (not (intersection (finite-automaton-states nfa-1)
                             (finite-automaton-states nfa-2))))
  ;; Create a new start state
  (let ((start (newstate)))
    ;; Build the new NFA
    (make-fa
     ;; Edges include epsilon-transitions from the new start state to both NFA starts,
     ;; plus all the edges from both NFAs
     (append (list (list start :epsilon (finite-automaton-start nfa-1))
                   (list start :epsilon (finite-automaton-start nfa-2)))
             (finite-automaton-edges nfa-1)
             (finite-automaton-edges nfa-2))
     ;; The new start state
     start
     ;; Accept states are the union of the accept states of both NFAs
     (append (finite-automaton-accept nfa-1)
             (finite-automaton-accept nfa-2)))))


;; Regular Expression Lecture: Kleene-Closure
(defun fa-repeat (nfa)
  "Find the repetition / Kleene-closure of NFA."
  (TODO 'fa-repeat))

;; Convert a regular expression to a nondeterministic finite
;; automaton.
;;
;; The following are examples of possible regular expressions.
;;
;; - (:concatenation a b c)
;; - (:union a b c :epsilon)
;; - (:union)
;; - (:kleene-closure a)
;; - (:concatenation (:union a b) (:kleene-closure c))
(defun regex->nfa (regex)
  "Convert a regular expression to an NFA."
  (cond
    ((null regex) ; Base case for empty set
     (make-fa nil (newstate) (list (newstate))))
    ;; TODO: other base cases
    (t
      (TODO 'regex->nfa))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 3: Regular Decision and Closure Properties ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lecture: Decision Properties of Regular Languages, Emptiness
(defun fa-empty (fa)
  "Does FA represent the empty set?"
  (TODO 'fa-empty))












;; Lecture: Closure Properties of Regular Languages, State Minimization
(defun fa-reverse (fa)
  "Return the reversal of FA (DFA to NFA), with simplified states and edges."
  ;; Create a new start state
  (let ((new-start (newstate))  ;; Generate a new unique state to be the start
        ;; The new accept states are the original start state of the DFA
        (new-accept (list (finite-automaton-start fa))))
    
    ;; Reverse the edges of the DFA to create the NFA
    (let ((new-edges
           (append
            ;; Add epsilon transitions from the new start state to each original accept state
            (mapcar (lambda (state)
                      (list new-start :epsilon state))
                    (finite-automaton-accept fa))
            ;; Reverse all other edges, flipping source and destination
            (remove-duplicates
             (mapcar (lambda (edge)
                       (destructuring-bind (state-0 input-symbol state-1) edge
                         (list state-1 input-symbol state-0)))  ;; Flip each edge direction
                     (finite-automaton-edges fa))
             :test #'equal)))) ;; Remove duplicate transitions in case of redundancy

      ;; Print out the states and edges for debugging purposes
      (format t "Original DFA Start State: ~A~%" (finite-automaton-start fa))
      (format t "Original DFA Accept States: ~A~%" (finite-automaton-accept fa))
      (format t "New Start State (for reversed NFA): ~A~%" new-start)
      (format t "New Accept States (for reversed NFA): ~A~%" new-accept)
      (format t "Reversed NFA Edges (after deduplication):~%")
      (dolist (edge new-edges)
        (format t "  ~A --'~A'--> ~A~%" (first edge) (second edge) (third edge)))

      ;; Build and return the reversed NFA
      (make-fa new-edges new-start new-accept))))



(defun dfa-minimize (dfa)
  "Return an equivalent DFA with minimum state using Brzozowski's Algorithm."
  ;; Step 1: Reverse the DFA to create an NFA
  (let* ((reversed-fa (fa-reverse dfa))
         ;; Step 2: Convert the reversed NFA to a DFA
         (dfa-from-reversed (nfa->dfa reversed-fa))
         ;; Step 3: Reverse this new DFA, resulting in another NFA
         (second-reversed-fa (fa-reverse dfa-from-reversed))
         ;; Step 4: Convert the second-reversed NFA to a minimized DFA
         (minimized-dfa (nfa->dfa second-reversed-fa)))

    minimized-dfa))















;; Lecture: Closure Properties of Regular Languages, Intersection
(defun dfa-intersection (dfa-0 dfa-1)
  "Return the intersection FA."
  (TODO 'dfa-intersection))

;; Lecture: Decision Properties of Regular Languages, Equivalence
(defun dfa-equivalent (dfa-0 dfa-1)
  "Do DFA-1 and DFA-2 recognize the same language?"
  (TODO 'dfa-equivalent))







;; TEST            TEST                  TEST


;;; Test Functions for e-closure and move-e-closure
(defun test-e-closure ()
  "Test the e-closure function with a sample NFA for all possible states."
  (let* ((nfa (make-fa '((q0 0 q0)
                         (q0 :epsilon q1)
                         (q1 1 q1)
                         (q1 :epsilon q2)
                         (q2 2 q2))
                       'q0
                       '(q2))))
    (dolist (state (finite-automaton-states nfa))
      (let ((closure (e-closure nfa (list state) nil)))
        (format t "e-closure of (~a): ~a~%" state closure)))))


;;; Run Test
;(test-e-closure)





(defun test-move-e-closure ()
  "Test the move-e-closure function with a sample NFA."
  (let* ((nfa (make-fa '((q0 0 q0)
                          (q0 :epsilon q1)
                          (q1 1 q1)
                          (q1 :epsilon q2)
                          (q2 2 q2))
                        'q0
                        '(q2))))
      (dolist (state (finite-automaton-states nfa))  ;; Iterate over all states in the NFA
        (dolist (symbol (finite-automaton-alphabet nfa))  ;; Iterate over all input symbols in the NFA
          (let ((closure (move-e-closure nfa (list state) symbol)))
            (format t "move-e-closure of (~a) on input '~a': ~a~%" state symbol closure))))))


;;; Run Test
;(test-move-e-closure)









;;; Test code for nfa->dfa function
(defun test-nfa->dfa ()
  (format t "Testing nfa->dfa function...~%")
  
  ;; Define the NFA
  (let* ((nfa (make-fa
               '(
                 ;; NFA transitions
                 (q0 :epsilon q1)
                 (q0 :epsilon q2)
                 (q1 0 q2)
                 (q1 0 q4)
                 (q2 :epsilon q3)
                 (q2 1 q1)
                 (q3 1 q4)
                 (q4 :epsilon q3)
                 )
               'q0   ; Start state
               '(q4)))  ; Accept state
         ;; Convert the NFA to a DFA
         (dfa (nfa->dfa nfa)))

    ;; Display the DFA structure for inspection
    (format t "DFA States: ~A~%" (finite-automaton-states dfa))
    (format t "DFA Start State: ~A~%" (finite-automaton-start dfa))
    (format t "DFA Accept States: ~A~%" (finite-automaton-accept dfa))
    (format t "DFA Transitions:~%")
    
    ;; Print all possible DFA transitions
    (mapcar (lambda (state)
              (dolist (input-symbol (finite-automaton-alphabet dfa))
                (let ((target-state (fa-transition dfa state input-symbol)))
                  (when target-state
                    (format t "  ~A --'~A'--> ~A~%" state input-symbol target-state)))))
            (finite-automaton-states dfa))
  ))

;(test-nfa->dfa)


#| nfa-dfa test with  walkthrough example from lecture pdf 4a

  (let* ((nfa (make-fa
               '(
                 ;; NFA transitions
                 (1 :epsilon 2)
                 (1 a 3)
                 (2 a 3)
                 (2 a 4)
                 (2 :epsilon 5)
                 (3 b 4)
                 (4 a 5)
                 (4 a 3)
                 )
               '1   ; Start state
               '(5)))  ; Accept state
         ;; Convert the NFA to a DFA
         (dfa (nfa->dfa nfa)))

|#










;;; Test code for fa-union

;;; Test code for fa-union


(defun test-fa-union ()
  (format t "Testing fa-union function...~%")

  ;; Define two sample NFAs
  ;; NFA 1 accepts strings containing "a"
  (let* ((nfa1 (make-fa
                '((q0 'a q1))
                'q0
                '(q1)))
         ;; NFA 2 accepts strings containing "b"
         (nfa2 (make-fa
                '((p0 'b p1))
                'p0
                '(p1)))
         ;; Create union NFA that accepts strings with either "a" or "b"
         (union-nfa (fa-union nfa1 nfa2)))

    ;; Display the union NFA structure for manual inspection
    (format t "Union NFA States: ~A~%" (finite-automaton-states union-nfa))
    (format t "Union NFA Start State: ~A~%" (finite-automaton-start union-nfa))
    (format t "Union NFA Accept States: ~A~%" (finite-automaton-accept union-nfa))
    (format t "Union NFA Transitions:~%")
    (map-fa-edges
     (lambda (state-0 input-symbol state-1)
       (format t "  ~A --'~A'--> ~A~%" state-0 input-symbol state-1))
     union-nfa)))
    

;; run Test
;(test-fa-union)


    #|
    ;; Define test strings and expected results
    (let ((test-strings
           '((:string ("a") :expected t)
             (:string ("b") :expected t)
             (:string ("a" "b") :expected t)
             (:string ("c") :expected nil)
             (:string ("a" "c") :expected t)
             (:string ("b" "c") :expected t)
             )))
  
      ;; Run tests on the Union NFA
      (dolist (test test-strings)
        (let* ((string (getf test :string))
               (expected (getf test :expected))
               (result (nfa-simulate union-nfa string)))
          (format t "Testing string ~A on Union NFA: ~A~%" string result)
          ;; Check correctness and print error if mismatched
          (unless (equal result expected)
            (format t "Test failed for input ~A on Union NFA. Expected: ~A, Got: ~A~%"
                    string expected result))))))
  (format t "fa-union tests completed successfully.~%~%"))



|#








;; Test code for DFA-minimize


(defun test-dfa-minimize ()
  (format t "Testing dfa-minimize function...~%")

  ;; Define a sample DFA with redundant states
    (let* ((dfa (make-fa
               '(
                 ;; DFA that accepts strings ending in "ab"
                 (0 a 1)
                 (0 b 2)
                 (1 a 3)
                 (1 b 4)
                 (2 a 4)
                 (2 b 3)
                 (3 b 3)
                 (3 a 3)
                 (4 a 4)
                 (4 b 4)
                 )
               '0
               '(3 4)))
         ;; Minimize the DFA
         (minimized-dfa (dfa-minimize dfa)))

    ;; Display the minimized DFA structure
    (format t "Minimized DFA States: ~A~%" (finite-automaton-states minimized-dfa))
    (format t "Minimized DFA Start State: ~A~%" (finite-automaton-start minimized-dfa))
    (format t "Minimized DFA Accept States: ~A~%" (finite-automaton-accept minimized-dfa))
    (format t "Minimized DFA Transitions:~%")
    (map-fa-edges
     (lambda (state-0 input-symbol state-1)
       (format t "  ~A --'~A'--> ~A~%" state-0 input-symbol state-1))
     minimized-dfa)
  ))

    #|
    ;; Define test strings and expected results for both original and minimized DFAs
    (let ((test-strings
           '((:string ("a" "a" "b") :expected t)
             (:string ("a" "b") :expected t)
             (:string ("a" "a" "a") :expected nil)
             (:string ("b" "a" "b") :expected t)
             (:string ("b" "a") :expected nil)
             (:string ("a" "b" "a" "b") :expected t)
             )))
      ;; Run tests on the minimized DFA
      (dolist (test test-strings)
        (let* ((string (getf test :string))
               (expected (getf test :expected))
               (result (dfa-simulate minimized-dfa string)))
          (format t "Testing string ~A on Minimized DFA: ~A~%" string result)
          ;; Check correctness and print error if mismatched
          (unless (equal result expected)
            (format t "Test failed for input ~A on Minimized DFA. Expected: ~A, Got: ~A~%"
                    string expected result))))))
  (format t "dfa-minimize tests completed successfully.~%~%"))
|#




#|
  (let* ((dfa (make-fa
               '(
                 ;; DFA that accepts strings ending in "ab"
                 (0 a 1)
                 (1 b 4)
                 (1 d 2)
                 (2 b 4)
                 (2 d 2)
                 (3 b 4)
                 (3 d 2)
                 (4 c 3)
                 )
               '0
               '(1 2 3)))


    (let* ((dfa (make-fa
               '(
                 ;; DFA that accepts strings ending in "ab"
                 (0 a 1)
                 (0 b 2)
                 (1 a 3)
                 (1 b 4)
                 (2 a 4)
                 (2 b 3)
                 (3 b 3)
                 (3 a 3)
                 (4 a 4)
                 (4 b 4)
                 )
               '0
               '(3 4)))





|#



;; Test dfs-minimize
(test-dfa-minimize)






(defun run-all-tests ()
  (test-nfa->dfa))

;(run-all-tests)


;; to run, cd to file where lisp file is
;; in terminal: sbcl
;;              (load "test.lisp")
