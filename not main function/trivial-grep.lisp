(defparameter *grep-alphabet*
  (labels ((h (list code end-code)
             (let ((list (cons (code-char code) list)))
               (if (eql code end-code)
                   list
                   (h list (1+ code) end-code )))))
    (h (h (h (list  #\Space #\Newline #\Tab
                    #\! #\@ #\# #\$ #\% #\^ #\& #\* #\( #\)
                    #\[ #\] #\{ #\}
                    #\| #\\
                    #\" #\' #\` #\~ #\: #\;
                    #\_ #\- #\+ #\/ #\=
                    #\< #\> #\, #\. #\?)
             (char-code #\a) (char-code #\z))
          (char-code #\A) (char-code #\Z))
       (char-code #\0) (char-code #\9)))
  "The alphabet, i.e., list of characters, to use for grepping.")

(defun grep-ensure-nfa (pattern)
  "Ensure that PATTERN is an NFA."
  (if (finite-automaton-p pattern)
      pattern
      (let ((pattern `(:concatenation (:kleene-closure :.)
                                      ,pattern
                                      (:kleene-closure :.))))
        (regex->nfa (simplify-regex pattern *grep-alphabet*)))))

(defun grep-string (pattern string)
  "Test if PATTERN matches STRING."
  (if (nfa-simulate (grep-ensure-nfa pattern) string)
      string
      nil))

(defun grep-stream (pattern stream)
  "Print lines in STREAM that match PATTERN."
  (let ((pattern (grep-ensure-nfa pattern))
        (line (read-line stream nil nil)))
    (when line
      (when (grep-string pattern line)
        (print line))
      (grep-stream pattern stream))))

(defun grep-file (pattern pathname)
  "Print lines in PATHNAME that match PATTERN."
  (with-open-file (stream pathname :direction :input)
    (grep-stream pattern stream)))
