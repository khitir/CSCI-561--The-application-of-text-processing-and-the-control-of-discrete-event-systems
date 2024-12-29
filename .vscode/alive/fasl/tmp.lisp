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

    (e-closure nfa (nfa-next (e-closure nfa s '()) a '()) '())
  )
)

(defun nfa-simulate (nfa sequence)
  "True if NFA accepts SEQUENCE."
  (labels 
    (
      ; Define edelta function
      (edelta 
        
        ; edelta function arguments
        ; subset is the subset of states to start from when traversing according to a symbol
        ; sequ is the input sequence of symbols
        (subset sequ)
        
        ; edelta function body
        (if sequ ; if there is symbol(s) in the sequence and not just an empty list 
          ; if not empty list, then for each state in subset, construct the set of states it goes to on the given symbol
          ; then take the union of all such sets ('unified-set')
          ; then take the epsilon closure of the unified-set ('output-set')
          ; then call edelta on the output-set and the rest of the symbols
          (let*
            ; Sequential variable declaration
            (
              (sym (car sequ))
              (unified-set (move-e-closure nfa subset sym)) ; the set of all states that can be reached when transitioning from the current set of states on the given symbol including a prior and post e-closure
            ) 
              
            (edelta unified-set (cdr sequ)) ; Final expression
          )
            
          ; if empty list then the set of possible accept states is the set of states the NFA is at
          subset
        )
      )
    )

    ; Use edelta to return whether the simulation on an NFA accepts or not
    (let* 
      (
        (q0 (finite-automaton-start nfa))
        (f (finite-automaton-accept nfa)) 
        (states (e-closure nfa (list q0 nil) nil)) ; Assign the states to move from as the epsilon closure of the starting states of the NFA
        (sequ (coerce sequence 'list))
        (final-subset (edelta states sequ))
      )
      
      ; Test whether the final subset and accept states share any states; if they do the if will evaluate to true (as what is provided is not nil but a list of shared states) and if they share nothing the if empty list will evaluate to nil
      (if (intersection final-subset f :test #'equal)
          t
          nil
      )
    )
  )
)

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

;; Subset Construction Lecture: Algorithm 5
(defun nfa->dfa (nfa)
  "Convert a nondeterministic finite automaton to a deterministic finite automaton."
  (let ((visited-hash (make-hash-table :test #'equal))
        (alphabet (remove :epsilon (finite-automaton-alphabet nfa))))
    (labels 
        ((sort-subset (u)
           ;; Sort subsets for consistent testing
           (sort u #'state-predicate))
         
         (visit-symbol (edges subset-0 input-symbol)
           ;; Find states reachable from subset-0 on input-symbol, including epsilon closures.
           (let* ((next-states (or (move-e-closure nfa subset-0 input-symbol) ()))  ; Use empty set instead of NIL
                  (sorted-next-states (sort-subset next-states)))
             ;(format t "Processing transition: ~A --'~A'--> ~A~%" subset-0 input-symbol sorted-next-states) ; Debug
             ;; Only process and add non-empty transitions
             (when (not (null sorted-next-states))
               ;; Process new subset if it hasn't been visited
               (unless (gethash sorted-next-states visited-hash)
                 ;(format t "Visiting new subset: ~A~%" sorted-next-states) ; Debug
                 (setf edges (visit-subset edges sorted-next-states)))
               ;; Add the transition to edges if it's non-empty
               (push (list subset-0 input-symbol sorted-next-states) edges))
             edges))
         
         (visit-subset (edges subset)
           ;; Visit each subset and mark it as visited
           (setf (gethash (sort-subset subset) visited-hash) t)
           ;(format t "Visiting subset: ~A~%" subset) ; Debug
           ;; Explore each symbol in the alphabet
           (dolist (input-symbol alphabet)
             (setf edges (visit-symbol edges subset input-symbol)))
           edges))
      
      ;; Start the construction algorithm from the NFA's initial state
      (let* ((q0 (finite-automaton-start nfa))
             (initial-subset (e-closure nfa (list q0) nil))
             (sorted-initial-subset (sort-subset initial-subset))
             (edges (visit-subset nil sorted-initial-subset))
             (dfa-states (remove nil (hash-table-keys visited-hash)))
             (dfa-accept (remove-if-not (lambda (subset)
                                          (intersection subset (finite-automaton-accept nfa) :test #'equal))
                                        dfa-states)))
        

        
        ;; Create and return the DFA
        (make-finite-automaton
         :states dfa-states
         :alphabet alphabet
         :edges edges
         :delta (lambda (state-0 input-symbol)
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
  (labels ((h (regex)
              (cond
               ;; Case 1: we see :. 
               ((eq regex :.)
                 (assert alphabet nil "Alphabet required for :. operator")
                 `(:union ,@alphabet))
               ;; Case 2: we have atom
               ((atom regex)
                 regex)
               ;; Case 3: Handle compound expressions
               (t (destructuring-bind (operator &rest args) regex
                    (case operator
                      ;; Convert (:? X) to (:union X :epsilon)
                      (:?
                       (assert (= (length args) 1) nil
                           ":? operator requires one argument")
                       `(:union ,(h (first args)) :epsilon))
                      ;; Convert (:+ X) to (:concatenation X (:kleene-closure X))
                      (:+
                       (assert (= (length args) 1) nil
                           ":+ operator requires one argument")
                       `(:concatenation ,(h (first args))
                                        (:kleene-closure ,(h (first args)))))
                      ;; For other operators (:union, :concatenation, :kleene-closure),
                      (:union
                       (cons operator (mapcar #'h args)))
                      (:concatenation
                       (cons operator (mapcar #'h args)))
                      (:kleene-closure
                       (assert (= (length args) 1) nil
                           ":kleene-closure operator requires one argument")
                       `(,operator ,(h (first args))))
                      ;; Invalid operator
                      (otherwise
                       (error "Invalid operator: ~A" operator))))))))
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
  ;; Idea: add new start state and a new final accept state. 
  ;; Have new start state have eps transition into old start state and to new accept state.
  ;; Have all old accept states have eps transition to old start state and to new accept state.
  "Find the repetition / Kleene-closure of NFA."

  (let* 
    (
      (start (newstate))
      (accept (newstate))
    )
    
    ;; Make an fa by inputting the edge states, the start state, and the accept state
    (make-fa 
     
      ;; Edge states
      (append 

      	;; Output of the code directly below is something like
        ;; ((new-start-state :epsilon nfa-start-state) (new-start-state :epsilon new-accept-state))
        ;; Giving 'start' an 'epsilon' transition to the start state of 'nfa-1'       
        (list 
          (list start :epsilon (finite-automaton-start nfa))
        
          (list start :epsilon accept)
        )
       
        ;; Output of code directly below is something like
        ;; ((nfa-accept-state1 :epsilon accept) (nfa-accept-state2 :epsilon accept) ...)
        ;; Give every accept state in 'nfa1' an 'epsilon' transition to the start state of 'nfa2'
        (map 
          'list 
          (lambda 
            (x)
            (list x :epsilon accept)
          )
          
          (finite-automaton-accept nfa)
        )

        ;; Output of code directly below is something like
        ;; ((nfa-accept-state1 :epsilon nfa-start-state) (nfa-accept-state2 :epsilon nfa-start-state) ...)
        ;; Give every accept state in 'nfa1' an 'epsilon' transition to the start state of 'nfa2'
        (map 
          'list 
          (lambda 
            (x)
            (list x :epsilon (finite-automaton-start nfa))
          )
          
          (finite-automaton-accept nfa)
        )

        (finite-automaton-edges nfa)

      )

      ;; Start state    
      start
          
      ;; Accept state
      (list accept)
    )
  )
)


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
(defun regex->nfa (inp-regex)
  "Convert a regular expression to an NFA."
  (cond 
    ((null inp-regex) ; Base case for empty set
      (let* 
        (
          (start (newstate))
          (accept (newstate))
        )

        (make-fa nil start (list accept))
      )
    )

    ((listp (car inp-regex)) ; the element is a list, which may happen if their is nested ':' regex operations
      (regex->nfa (car inp-regex))
    )

    ((equal (car inp-regex) :kleene-closure)
      (let* 
        (
          (out-regex (cdr inp-regex))
        )

        (fa-repeat (regex->nfa out-regex))
      )
    )

    ((equal (car inp-regex) :union)
      (let* 
        (
          (num-args-after (length (cdr inp-regex)))
        )

        (cond 
          ((equal 0 num-args-after)
            (regex->nfa '())
          )

          ((equal 1 num-args-after)
            (let* 
              (
                (out-regex (list (car (cdr inp-regex))))
              )
              
              (regex->nfa out-regex)
            )
          )

          ((equal 2 num-args-after)
            (let* 
              (
                (regex-1 (list (car (cdr inp-regex))))
                (regex-2 (list (car (cdr (cdr inp-regex)))))
              )
              
              (fa-union (regex->nfa regex-1) (regex->nfa regex-2))
            )
          )

          (t ; Fall through case, apply concatenation to the first symbolic expression and to the result of the concatenation of the rest of the regex
            (let* 
              (
                (regex-1 (list (car (cdr inp-regex))))
                (regex-rest (cdr (cdr inp-regex)))
                (union-rest (append '(:union) regex-rest))
              )
              
              (fa-union (regex->nfa regex-1) (regex->nfa union-rest))
            )
          )
        )
      )
    )

    ((equal (car inp-regex) :concatenation)
      (let* 
        (
          (num-args-after (length (cdr inp-regex)))
        )

        (cond 
          ((equal 0 num-args-after)
            (regex->nfa '(:epsilon))
          )

          ((equal 1 num-args-after)
            (let* 
              (
                (out-regex (list (car (cdr inp-regex))))
              )
              
              (regex->nfa out-regex)
            )
          )

          ((equal 2 num-args-after)
            (let* 
              (
                (regex-1 (list (car (cdr inp-regex))))
                (regex-2 (list (car (cdr (cdr inp-regex)))))
              )
              
              (fa-concatenate (regex->nfa regex-1) (regex->nfa regex-2))
            )
          )

          (t ; Fall through case, apply concatenation to the first symbolic expression and to the result of the concatenation of the rest of the regex
            (let* 
              (
                (regex-1 (list (car (cdr inp-regex))))
                (regex-rest (cdr (cdr inp-regex)))
                (concat-rest (append '(:concatenation) regex-rest))
              )
              
              (fa-concatenate (regex->nfa regex-1) (regex->nfa concat-rest))
            )
          )
        )
      )
    )

    ;; If nothing else, what was encountered is assumed symbol to recognize
    (t
      (let* 
        (
          (start (newstate))
          (accept (newstate))
          (sym (car inp-regex))
        )

        (make-fa (list (list start sym accept)) start (list accept))
      )
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 3: Regular Decision and Closure Properties ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lecture: Decision Properties of Regular Languages, Emptiness
(defun fa-empty (fa)
  (let ((visited (make-symbol-hash-table)))
    (labels ((visit (current-state)
                    ;; not visited yet
                    (unless (gethash current-state visited)
                      ;; Mark current state as visited
                      (setf (gethash current-state visited) t)
                      (dolist (edge (finite-automaton-edges fa))
                        (destructuring-bind (state-0 input-symbol state-1) edge
                         (declare (ignore input-symbol))
                          ;; If edge starts from our current state
                          (when (equal state-0 current-state)
                                ;; Recursively visit the destination state
                                (visit state-1)))))))
      ;; Start DFS
      (visit (finite-automaton-start fa))
      (dolist (accept-state (finite-automaton-accept fa))
        ;; If any reachable accept state
        (when (gethash accept-state visited)
              ;; FA is not empty - return nil (false)
              (return-from fa-empty nil))))
    ;; FA is empty - return t (true)
    t))



(defun fa-reverse (fa)
  "Return the reversal of FA, creating a new start state with epsilon transitions only if there is more than one accept state."
  (let* ((accept-states (finite-automaton-accept fa))
         ;; If there is only one accept state, make it the new start state.
         (new-start (if (= (length accept-states) 1)
                        (first accept-states)
                        (newstate)))
         ;; The new accept states are always set to the original start state
         (new-accept (list (finite-automaton-start fa)))
         ;; Add epsilon transitions only if a new start state is created and we have multiple accept states
         (new-epsilon-edges (if (> (length accept-states) 1)
                                (mapcar (lambda (state) (list new-start :epsilon state))
                                        accept-states)
                                '()))
         ;; Reverse the edges
         (new-edges
          (append
           new-epsilon-edges
           ;; Reverse all edges
           (mapcar (lambda (edge)
                     (destructuring-bind (state-0 input-symbol state-1) edge
                       (list state-1 input-symbol state-0)))
                   (finite-automaton-edges fa)))))

    ;; Print out the states and edges for debugging purposes
    ;(format t "Original DFA Start State: ~A~%" (finite-automaton-start fa))
    ;(format t "Original DFA Accept States: ~A~%" accept-states)
    ;(format t "New Start State (for reversed NFA): ~A~%" new-start)
    ;(format t "New Accept States (for reversed NFA): ~A~%" new-accept)
    ;(format t "Reversed NFA Edges (after deduplication):~%")
    ;(dolist (edge new-edges)
      ;(format t "  ~A --'~A'--> ~A~%" (first edge) (second edge) (third edge)))

    ;; Build the reversed FA
    (make-fa new-edges new-start new-accept)))


(defun filter-nil-states-and-transitions (dfa)
  "Remove any NIL states and transitions from the DFA."
  (let* ((filtered-states (remove nil (finite-automaton-states dfa)))
         (filtered-edges (remove-if (lambda (edge)
                                      (or (equal (first edge) nil)
                                          (equal (third edge) nil)))
                                    (finite-automaton-edges dfa)))
         (filtered-accept (remove nil (finite-automaton-accept dfa))))
    ;; Create a new DFA without NIL states or transitions
    (make-fa filtered-edges
             (finite-automaton-start dfa)
             filtered-accept)))

(defun dfa-minimize (dfa)
  "Minimize a DFA using Brzozowski's Algorithm (double-reversal method)."
  (let* ((reversed-fa (fa-reverse dfa))
         (dfa-from-reversed (nfa->dfa reversed-fa))
         ;; Filter out NIL states and transitions from dfa-from-reversed
         (filtered-dfa-from-reversed (filter-nil-states-and-transitions dfa-from-reversed))
         (second-reversed-fa (fa-reverse filtered-dfa-from-reversed))
         (minimized-dfa (nfa->dfa second-reversed-fa))
         ;; Filter out NIL states and transitions from minimized DFA
         (final-minimized-dfa (filter-nil-states-and-transitions minimized-dfa))
         )
    
    ;; Display the minimized DFA for verification
    ;(format t "Minimized DFA States: ~A~%" (finite-automaton-states final-minimized-dfa))
    ;(format t "Minimized DFA Start State: ~A~%" (finite-automaton-start final-minimized-dfa))
    ;(format t "Minimized DFA Accept States: ~A~%" (finite-automaton-accept final-minimized-dfa))
    ;(format t "Minimized DFA Transitions:~%")
    ;(map-fa-edges
     ;(lambda (state-0 input-symbol state-1)
      ; (format t "  ~A --'~A'--> ~A~%" state-0 input-symbol state-1))
     ;final-minimized-dfa)
    final-minimized-dfa))

;; Lecture: Closure Properties of Regular Languages, Intersection

(defun create-product-dfa-states (dfa-a-states dfa-b-states)
  (labels
      ;; Local functions
    ((accumulate-state-cartesian-products (op-state old-states old-acc)
                                          (if old-states
                                              (let*
                                                  ((new-states (cdr old-states))

                                                   (new-acc
                                                    (append old-acc (list op-state (car old-states)))))

                                                ;(format t "~A~%" (list op-state (car old-states)))

                                                (accumulate-state-cartesian-products op-state new-states new-acc))

                                              old-acc))

     (accumulate-dfa-states (dfa-0-states dfa-1-states dfa-p-states)
                            ;; If dfa-0-states is not empty
                            (if dfa-0-states

                                ;; Then recursively apply function to dfa-0-states minus the first state 
                                (let*
                                    (;; Reduced dfa states by 1
                                     (dfa-r-states
                                      (cdr dfa-0-states))

                                     ;; Multiplied DFA states
                                     ;; Add to existing product DFA states product DFA states from Cartesian producting the first state in DFA 0 with DFA 1 states
                                     (dfa-m-states
                                      (append
                                        dfa-p-states
                                        (accumulate-state-cartesian-products (car dfa-0-states) dfa-1-states '()))))

                                  (accumulate-dfa-states dfa-r-states dfa-1-states dfa-m-states))

                                ;; Else return product dfa states
                                dfa-p-states)))

    ;; Body
    (accumulate-dfa-states dfa-a-states dfa-b-states '())))

(defun accumulate-product-op-edge-var-edges (op-edge old-edges acc-edges)
  (if old-edges
      (let*
          (;; The new set of variable edges is the old set minus the top of the stack
           (new-edges (cdr old-edges))

           ;; Give names to the edge being operated on
           (var-edge (car old-edges))

           ;; Get the prior states, symbols, and post states the edges encompass for each edge
           (prior-state-0 (first op-edge))
           (prior-state-1 (first var-edge))

           (symbol-0 (second op-edge))
           (symbol-1 (second var-edge))

           (post-state-0 (third op-edge))
           (post-state-1 (third var-edge))

           ;; Combine the individual edge prior states, symbols, and post states together to make a product prior state, symbol, and post state
           (prior-state-p (list prior-state-0 prior-state-1))
           (symbol-p (list symbol-0 symbol-1))
           (post-state-p (list post-state-0 post-state-1))

           ;; Combine the product edge prior states, symbols, and post states together to make a product edge
           (product-edge (list (list prior-state-p symbol-p post-state-p)))

           ;; Add the product-edge to the acc-edges
           (sum-edges (append acc-edges product-edge)))

        ;; Recursively apply function until there is no further variable edges
        ; (format t "~A~%" product-edge)

        (accumulate-product-op-edge-var-edges op-edge new-edges sum-edges))

      ;; Return the accumulated product edges when no further variable edges to operate on
      acc-edges))

(defun accumulate-product-edges (edge-set-op edge-set-var edge-set-p)
  ;; If there is edges left in the operating set
  (if edge-set-op
      (let*
          ((edge-set-r (cdr edge-set-op))

           (op-edge-products (accumulate-product-op-edge-var-edges (car edge-set-op) edge-set-var '()))
           (edge-set-m (append edge-set-p op-edge-products)))

        ;; Recursively apply the function until no further elements in the operating edge set
        (accumulate-product-edges edge-set-r edge-set-var edge-set-m))

      ;; Return the product edge set constructed if no further elements in the operating edge set
      edge-set-p))

(defun create-product-fa-transitions (fa-0 fa-1)
  (let*
      (;; Get all edges in both fas
       (edge-set-0 (finite-automaton-edges fa-0))
       (edge-set-1 (finite-automaton-edges fa-1)))

    ; (format t "~%")	
    ; (format t "EDGE-SET-0: ~A~%" edge-set-0)
    ; (format t "~%")
    ; (format t "EDGE-SET-1: ~A~%" edge-set-1)
    ; (format t "~%")

    ;; Accumulate the product edges between each edge set
    (accumulate-product-edges edge-set-0 edge-set-1 '())))

;; Lecture: Closure Properties of Regular Languages, Intersection
(defun dfa-intersection (dfa-0 dfa-1)
  "Return the intersection FA."

  ;; Create product DFA with the accept states as the Cartesian product of all accept states in 0 with all accept states in 1
  (let*
      (;; Create the product DFA accept states, should return a list of states (even if no or only one state inside the list)
       (dfa-p-accept (create-product-dfa-states (finite-automaton-accept dfa-0) (finite-automaton-accept dfa-1)))

       ;; Create new product DFA start state q0p = (q0a, q0b)
       (dfa-p-start (list (finite-automaton-start dfa-0) (finite-automaton-start dfa-1)))

       ;;
       (dfa-p-edges (create-product-fa-transitions dfa-0 dfa-1)))

    ; (format t "~%")
    ; (format t "DFA-P-ACCEPT: ~A~%" dfa-p-accept)
    ; (format t "~%")
    ; (format t "DFA-P-START: ~A~%" dfa-p-start)
    ; (format t "~%")
    ; (format t "DFA-P-EDGES: ~A~%" dfa-p-edges)
    ; (format t "~%")

    ;; Make an fa by inputting the edge states, the start state, and the accept state
    (make-fa

     ;; Edge states
     dfa-p-edges

     ;; Start state    
     dfa-p-start

     ;; Accept state
     dfa-p-accept)))

;; Lecture: Decision Properties of Regular Languages, Equivalence
(defun dfa-complement (dfa)
  ;; Check if input is a DFA
  (assert (dfa-p dfa) () "Input must be a DFA")

  ;; adding reject state
  (let* ((complete-dfa (dfa-add-reject dfa))
         ;; Get all states and accept states
         (all-states (finite-automaton-states complete-dfa))
         (accept-states (finite-automaton-accept complete-dfa))
         ;; New accept state
         (new-accept-states (set-difference all-states accept-states)))

    ;; Create new DFA 
    (make-fa (finite-automaton-edges complete-dfa)
             (finite-automaton-start complete-dfa)
             new-accept-states)))


(defun dfa-equivalent (dfa-0 dfa-1)
  "Check if two DFAs accept the same language in a simple way"
  ;; check valid DFAs
  (assert (dfa-p dfa-0) () "First argument is not a DFA")
  (assert (dfa-p dfa-1) () "Second argument is not a DFA")

  ;; both are empty DFAs
  (if (and (fa-empty dfa-0) (fa-empty dfa-1))
      t
      ;; one is empty and other isn't
      (if (or (and (fa-empty dfa-0) (not (fa-empty dfa-1)))
              (and (not (fa-empty dfa-0)) (fa-empty dfa-1)))
          nil
          ;; check if each DFA accepts anything the other doesn't
          (let ((dfa0-accepts-but-dfa1-doesnt nil)
                (dfa1-accepts-but-dfa0-doesnt nil))
            ;; DFA-0 accepts but DFA-1 doesn't
            (setf dfa0-accepts-but-dfa1-doesnt
              (not (fa-empty (dfa-intersection
                              dfa-0
                              (dfa-complement dfa-1)))))

            ;; DFA-1 accepts but DFA-0 doesn't
            (setf dfa1-accepts-but-dfa0-doesnt
              (not (fa-empty (dfa-intersection
                              dfa-1
                              (dfa-complement dfa-0)))))

            ;; DFA accepts anything the other doesn't
            (not (or dfa0-accepts-but-dfa1-doesnt
                     dfa1-accepts-but-dfa0-doesnt))))))



;;;;;;;;;       TEST                 TEST                              TEST
;;;;;;;;;;;;;;;;;;;;
;;; FA structure ;;;
;;;;;;;;;;;;;;;;;;;;

;; Make an DFA
(defparameter *dfa-0*
  (make-fa '((q0 0 q0)
             (q0 1 q1)
             (q1 1 q1)
             (q1 0 q0))
           'q0
           '(q1)))

(defparameter *dfa-1* 
  (make-fa '((q0 0 q1)
             (q1 1 q0))
           'q0
           '(q1)))

(defparameter *dfa-2* ; accepts nothing DFA
  (make-fa '()
           'q0
           '(q0)))

(defparameter *dfa-3* ; one-way door with accept after door
  (make-fa '((q0 0 q1)
             (q0 1 q1))
           'q0
           '(q1)))

(defparameter *dfa-4* ; one-way door with no accept after door
  (make-fa '((q0 0 q1)
             (q0 1 q1))
           'q0
           '(q0)))

;; Make an NFA
(defparameter *nfa-0* ; 
  (make-fa '((q0 0 q0)
             (q0 :epsilon q1)
             (q1 1 q1)
             (q1 :epsilon q2)
             (q2 2 q2))
           'q0
           '(q2)))

(defparameter *nfa-1*
  (make-fa '((q0 :epsilon q1)
             (q1 1 q1)
             (q1 0 q1)
             (q1 :epsilon q0))
           'q0
           '(q1)))

(defparameter *nfa-2* ; mirror of DFA 2 but an NFA. Also with a nasty self epsilon edge that may break stuff
  (make-fa '((q0 :epsilon q0))
           'q0
           '(q0)))

(defparameter *nfa-3* ; kind of like DFA 3 but an NFA with epsilon transition to door.
  (make-fa '((q0 0 q1)
             (q0 1 q1)
             (q0 :epsilon q1))
           'q0
           '(q1)))

(defparameter *nfa-4* ; kind of like DFA 4 but an NFA with epsilon transition to door.
  (make-fa '((q0 0 q1)
             (q0 1 q1)
             (q0 :epsilon q1))
           'q0
           '(q0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 0: DFA Simulation ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dfa-simulate *dfa-0* '(0 1))
;; => t

(dfa-simulate *dfa-1* '(0 1))
;; => nil

(dfa-simulate *dfa-1* '(0))
;; => t

(dfa-simulate *dfa-1* '(0 1 0))
;; => t

(dfa-simulate *dfa-1* '(1))
;; => nil


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 1: NFA Subset Construction ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Test code for nfa->dfa function #####################################################################################
(defun test-nfa->dfa ()
  (format t "Testing nfa->dfa function...~%")
  
  ;; Define the NFA
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

    ;; Display the DFA structure for inspection
    (format t "Final DFA States: ~A~%" (finite-automaton-states dfa))
    (format t "Final DFA Start State: ~A~%" (finite-automaton-start dfa))
    (format t "Final DFA Accept States: ~A~%" (finite-automaton-accept dfa))
    (format t "Final DFA Transitions:~%")
    
    ;; Print all possible DFA transitions
    (mapcar (lambda (state)
              (dolist (input-symbol (finite-automaton-alphabet dfa))
                (let ((target-state (fa-transition dfa state input-symbol)))
                  (when target-state
                    (format t "  ~A --'~A'--> ~A~%" state input-symbol target-state)))))
            (finite-automaton-states dfa))
  ))

; run test
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



                 (let* ((nfa (make-fa
               '(
                 ;; NFA transitions
                 (q0 0 q0)
                 (q0 1 q0)
                 (q0 1 q1)
                 )
               'q0   ; Start state
               '(q1)))  ; Accept state
         ;; Convert the NFA to a DFA
         (dfa (nfa->dfa nfa)))

|#


; NFA-SIMULATE ###################################################################################################
(nfa-simulate *nfa-0* '(0 1))
;; => t

(nfa-simulate *nfa-0* '())
;; => t

(nfa-simulate *nfa-0* '(0))
;; => t

(nfa-simulate *nfa-1* '())
;; => t

(nfa-simulate *nfa-1* '(0))
;; => t

(nfa-simulate *nfa-1* '(0 1 1 0 1 0 1))
;; => t

(nfa-simulate *nfa-2* '())
;; => t

(nfa-simulate *nfa-2* '(1))
;; => nil

(nfa-simulate *nfa-2* '(0))
;; => nil

(nfa-simulate *nfa-3* '())
;; => t

(nfa-simulate *nfa-3* '(0))
;; => t

(nfa-simulate *nfa-3* '(1))
;; => t

(nfa-simulate *nfa-3* '(0 1))
;; => nil

(nfa-simulate *nfa-3* '(1 0))
;; => nil

(nfa-simulate *nfa-4* '())
;; => t

(nfa-simulate *nfa-4* '(1))
;; => nil

(nfa-simulate *nfa-4* '(0))
;; => nil


; SIMPLIFY-REGEX ###################################################################################################
(defun test-simplify-regex-edge-cases ()
  (format t "~%=== Testing Edge Cases and Error Handling ===~%")

  ;; Helper function to safely test cases that might error
  (defun safe-test (name test-form)
    (format t "~%Testing ~A:~%" name)
    (handler-case
        (format t "Result: ~A" (eval test-form))
      (error (c)
        (format t "Caught error: ~A" c))))

  ;; 1. Basic Valid Cases (for comparison)
  (safe-test "Simple valid expression1"
             '(simplify-regex 'a))

  ;; 2. Dot Operator Edge Cases
  (safe-test "Dot without alphabet2"
             '(simplify-regex :.))

  (safe-test "Dot with empty alphabet3"
             '(simplify-regex :. '()))

  (safe-test "Dot with valid alphabet4"
             '(simplify-regex :. '(a b c)))

  ;; 3. Invalid Atomic Expressions
  (safe-test "Invalid atomic (number)5"
             '(simplify-regex 42))

  (safe-test "Invalid atomic (string)6"
             '(simplify-regex "a"))

  ;; 4. Optional Operator Edge Cases
  (safe-test "Optional with no args7"
             '(simplify-regex '(:?)))

  (safe-test "Optional with too many args8"
             '(simplify-regex '(:? a b)))

  ;; 5. One-or-more Operator Edge Cases
  (safe-test "One-or-more with no args9"
             '(simplify-regex '(:+)))

  (safe-test "One-or-more with too many args10"
             '(simplify-regex '(:+ a b)))

  ;; 6. Kleene-closure Edge Cases
  (safe-test "Kleene-closure with no args11"
             '(simplify-regex '(:kleene-closure)))

  (safe-test "Kleene-closure with too many args12"
             '(simplify-regex '(:kleene-closure a b)))

  ;; 7. Invalid Operators
  (safe-test "Unknown operator13"
             '(simplify-regex '(:unknown a)))

  ;; 8. Nested Edge Cases
  (safe-test "Deeply nested valid expression14"
             '(simplify-regex '(:concatenation
                                (:? (:union a (:+ b)))
                                (:kleene-closure (:? c)))))

  (safe-test "Nested with invalid operator15"
             '(simplify-regex '(:concatenation
                                (:? a)
                                (:invalid b))))

  ;; 9. Empty and Nil Cases
  (safe-test "Empty list16"
             '(simplify-regex '()))

  (safe-test "Nil expression17"
             '(simplify-regex nil))

  ;; 10. Complex Mixed Cases
  (safe-test "Mixed valid operators18"
             '(simplify-regex '(:concatenation
                                (:union (:? a) (:+ b))
                                (:kleene-closure c))))

  (safe-test "Mixed with dot operator19"
             '(simplify-regex '(:concatenation :. (:? a))
                              '(x y)))

  (safe-test "example from git20"
             '(simplify-regex '(:? (:union 0 1)) '(0 1)))
;; => (:union :epsilon (:union 0 1))

  (safe-test "example from git-2-21"
           '(simplify-regex '(:+ :.) '(0 1))))


; FA-UNION ###################################################################################################
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



; FA-REPEAT ###################################################################################################  -> no need 


; REGEX-NFA ###################################################################################################   -> no need 

; FA-EMPTY ###################################################################################################  
(defun test-fa-empty ()
  "Test function for fa-empty with various cases"
  (format t "~%=== Testing fa-empty ===~%")

  ;; Helper to run and display test
  (defun run-test (name fa expected)
    (format t "~%Test: ~A~%" name)
    (let ((result (fa-empty fa)))
      (format t "Result: ~A~%" result)
      (format t "Expected: ~A~%" expected)
      (format t "Test ~A~%"
        (if (eq result expected) "PASSED ✓" "FAILED ✗"))))

  ;; Test 1: Basic non-empty FA
  (run-test "Basic non-empty FA"
            (make-fa '((q0 0 q0)
                       (q0 1 q1)
                       (q1 1 q1))
                     'q0
                     '(q1))
            nil)

  ;; Test 2: Empty FA
  (run-test "Empty FA"
            (make-fa '((q0 0 q0))
                     'q0
                     '(q1))
            t)

  ;; Test 3: Start state is accept
  (run-test "Start state is accept"
            (make-fa '((q0 0 q0))
                     'q0
                     '(q0))
            nil)

  ;; Test 4: Cyclic path to accept
  (run-test "Cyclic path to accept"
            (make-fa '((q0 0 q1)
                       (q1 0 q2)
                       (q2 0 q0))
                     'q0
                     '(q2))
            nil)
  ;; Test 5: Unreachable accept state
  (run-test "Unreachable accept state"
            (make-fa '((q0 0 q1)
                       (q1 1 q2))
                     'q0
                     '(q3))
            t)

  ;; Test 6: Accept state not connected
  (run-test "Accept state not connected"
            (make-fa '((q0 0 q1)
                       (q1 1 q0))
                     'q0
                     '(q2))
            t)

  ;; Test 7: Multiple accept states, one reachable
  (run-test "Multiple accept states, one reachable"
            (make-fa '((q0 0 q1)
                       (q1 1 q0)
                       (q0 1 q2))
                     'q0
                     '(q2 q3))
            nil)

  ;; Test 8: Self-loop accept state
  (run-test "Self-loop accept state"
            (make-fa '((q0 0 q0))
                     'q0
                     '(q0))
            nil)

  ;; Test 9: FA with no transitions but start state is accept
  (run-test "No transitions, start is accept"
            (make-fa '()
                     'q0
                     '(q0))
            nil)

  ;; Test 10: FA with no accept states
  (run-test "No accept states"
            (make-fa '((q0 0 q1)
                       (q1 1 q0))
                     'q0
                     '())
            t)

  ;; Test 11: Epsilon transitions leading to accept state
  (run-test "Epsilon transitions to accept"
            (make-fa '((q0 :epsilon q1)
                       (q1 0 q1))
                     'q0
                     '(q1))
            nil)

  ;; Test 12: Unreachable start state
  (run-test "Unreachable start state"
            (make-fa '((q1 0 q2)
                       (q2 1 q3))
                     'q0
                     '(q3))
            t)

  ;; Test 13: All states are accept states
  (run-test "All states are accept states"
            (make-fa '((q0 0 q1)
                       (q1 1 q0))
                     'q0
                     '(q0 q1))
            nil)

  ;; Test 14: Cycles not leading to accept state
  (run-test "Cycles not leading to accept"
            (make-fa '((q0 0 q1)
                       (q1 0 q2)
                       (q2 0 q0))
                     'q0
                     '(q3))
            t)

  ;; Test 15: Dead-end states
  (run-test "Dead-end states"
            (make-fa '((q0 0 q1)
                       (q1 0 q2)
                       (q2 0 q3))
                     'q0
                     '(q4))
            t)

  ;; Test 16: Multiple paths, accept reachable
  (run-test "Multiple paths, accept reachable"
            (make-fa '((q0 0 q1)
                       (q0 1 q2)
                       (q1 0 q3)
                       (q2 1 q4)
                       (q3 0 q5)
                       (q5 0 q6)
                       (q6 0 q7)
                       (q7 0 q4)) ; Path leading to accept state q4
                     'q0
                     '(q4))
            nil)

  ;; Test 17: Only epsilon transitions
  (run-test "Only epsilon transitions"
            (make-fa '((q0 :epsilon q1)
                       (q1 :epsilon q2)
                       (q2 :epsilon q3))
                     'q0
                     '(q3))
            nil)

  ;; Test 18: Simulated multiple start states
  (run-test "Simulated multiple start states"
            (make-fa '((q_start :epsilon q0)
                       (q_start :epsilon q3)
                       (q0 0 q1)
                       (q1 1 q2)
                       (q3 0 q4)
                       (q4 1 q5))
                     'q_start
                     '(q2 q5))
            nil)

  ;; Test 19: FA with no states
  (run-test "FA with no states"
            (make-fa '()
                     nil
                     '())
            t)

  ;; Test 20: Start state, no transitions, no accept states
  (run-test "Start state, no transitions, no accept states"
            (make-fa '()
                     'q0
                     '())
            t))



; DFA-MINIMIZE ###################################################################################################
;; Test code for DFA-minimize


(defun test-dfa-minimize ()
  (format t "Testing dfa-minimize function...~%")

  ;; Define a sample DFA with redundant states
(let* ((dfa (make-fa
               '(
                 ;; DFA that accepts strings ending in "ab"
                 (a 0 b)
                 (a 1 c)
                 (b 0 a)
                 (b 1 d)
                 (c 0 e)
                 (c 1 f)
                 (d 0 e)
                 (d 1 f)
                 (e 0 e)
                 (e 1 f)
                 (f 0 f)
                 (f 1 f)
                 )
               'a
               '(c d e)))
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

;; Test dfs-minimize
(test-dfa-minimize)


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



; wikipedia example

(let* ((dfa (make-fa
               '(
                 ;; DFA that accepts strings ending in "ab"
                 (a 0 b)
                 (a 1 c)
                 (b 0 a)
                 (b 1 d)
                 (c 0 e)
                 (c 1 f)
                 (d 0 e)
                 (d 1 f)
                 (e 0 e)
                 (e 1 f)
                 (f 0 f)
                 (f 1 f)
                 )
               'a
               '(c d e)))
         ;; Minimize the DFA
         (minimized-dfa (dfa-minimize dfa)))



|#
; DFA-INTERSECTION ###################################################################################################


; DFA-EQUIVALENT ###################################################################################################