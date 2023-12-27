(load "gpp_lexer.lisp")


(defvar *tokens* nil) ;; tokens read from the line

;; now according to the tokens we will check the syntax of the line
;; we will handle each grammar rule by checking the tokens one by one
;; if the syntax is correct then we will print the result

;; EXIT CASE: OP_OP KW_EXIT OP_CP

;; EXPRESSIONS
;; OP_OP OP_PLUS $EXP $EXP OP_CP
;; OP_OP OP_MINUS $EXP $EXP OP_CP
;; OP_OP OP_MULT $EXP $EXP OP_CP
;; OP_OP OP_DIV $EXP $EXP OP_CP

;; FUNCTION EXPRESSIONS
;; OP_OP IDENTIFIER $EXP OP_CP
;; OP_OP IDENTIFIER $EXP $EXP OP_CP
;; OP_OP IDENTIFIER $EXP $EXP $EXP OP_CP

;; IDENTIFIER
;; VALUEF


(defun add_valuef (valuef1 valuef2)
    ;; valuef1 is like this: "2b1"
    ;; valuef2 is like this: "4b1"
    ;; before b is numerator and after b is denominator
    ;; we will add these two valuefs

    ;; split the numerator and denominator
    (setq numerator1 (parse-integer (subseq valuef1 0 (position #\b valuef1))))
    (setq denominator1 (parse-integer (subseq valuef1 (+ 1 (position #\b valuef1)))))
    (setq numerator2 (parse-integer (subseq valuef2 0 (position #\b valuef2))))
    (setq denominator2 (parse-integer (subseq valuef2 (+ 1 (position #\b valuef2)))))
    ;; find the lcm of the denominators
    (setq lcm (lcm denominator1 denominator2))
    ;; multiply the numerators with lcm/denominator
    (setq numerator1 (* numerator1 (/ lcm denominator1)))
    (setq numerator2 (* numerator2 (/ lcm denominator2)))
    ;; add the numerators
    (setq numerator (+ numerator1 numerator2))
    
    ;; find the gcd of the numerator and denominator
    (setq gcd (gcd numerator lcm))
    ;; divide the numerator and denominator with gcd
    (setq numerator (/ numerator gcd))
    (setq lcm (/ lcm gcd))
    ;; combine numerator and denominator b at the middle.
    (let
        (
            (result nil)
        )
        (setq result (concatenate 'string (write-to-string numerator) "b" (write-to-string lcm)))
        (return-from add_valuef result)
    )
)

(defun subtract_valuef (valuef1 valuef2)
    ;; valuef1 is like this: "2b1"
    ;; valuef2 is like this: "4b1"
    ;; before b is numerator and after b is denominator
    ;; we will subtract these two valuefs

    ;; split the numerator and denominator
    (setq numerator1 (parse-integer (subseq valuef1 0 (position #\b valuef1))))
    (setq denominator1 (parse-integer (subseq valuef1 (+ 1 (position #\b valuef1)))))
    (setq numerator2 (parse-integer (subseq valuef2 0 (position #\b valuef2))))
    (setq denominator2 (parse-integer (subseq valuef2 (+ 1 (position #\b valuef2)))))

    ;; find the lcm of the denominators
    (setq lcm (lcm denominator1 denominator2))
    ;; multiply the numerators with lcm/denominator
    (setq numerator1 (* numerator1 (/ lcm denominator1)))
    (setq numerator2 (* numerator2 (/ lcm denominator2)))
    ;; subtract the numerators
    (setq numerator (- numerator1 numerator2))

    ;; find the gcd of the numerator and denominator
    (setq gcd (gcd numerator lcm))
    ;; divide the numerator and denominator with gcd
    (setq numerator (/ numerator gcd))
    (setq lcm (/ lcm gcd))
    ;; combine numerator and denominator b at the middle.
    (let
        (
            (result nil)
        )
        (setq result (concatenate 'string (write-to-string numerator) "b" (write-to-string lcm)))
        (return-from subtract_valuef result)
    )
)

(defun multiply_valuef (valuef1 valuef2)
    ;; valuef1 is like this: "2b1"
    ;; valuef2 is like this: "4b1"
    ;; before b is numerator and after b is denominator
    ;; we will multiply these two valuefs

    ;; split the numerator and denominator
    (setq numerator1 (parse-integer (subseq valuef1 0 (position #\b valuef1))))
    (setq denominator1 (parse-integer (subseq valuef1 (+ 1 (position #\b valuef1)))))
    (setq numerator2 (parse-integer (subseq valuef2 0 (position #\b valuef2))))
    (setq denominator2 (parse-integer (subseq valuef2 (+ 1 (position #\b valuef2)))))

    ;; multiply the numerators and denominators
    (setq numerator (* numerator1 numerator2))
    (setq denominator (* denominator1 denominator2))

    ;; find the gcd of the numerator and denominator
    (setq gcd (gcd numerator denominator))
    ;; divide the numerator and denominator with gcd
    (setq numerator (/ numerator gcd))
    (setq denominator (/ denominator gcd))
    ;; combine numerator and denominator b at the middle.
    (let
        (
            (result nil)
        )
        (setq result (concatenate 'string (write-to-string numerator) "b" (write-to-string denominator)))
        (return-from multiply_valuef result)
    )
)

(defun divide_valuef (valuef1 valuef2)
    ;; valuef1 is like this: "2b1"
    ;; valuef2 is like this: "4b1"
    ;; before b is numerator and after b is denominator
    ;; we will divide these two valuefs

    ;; split the numerator and denominator
    (setq numerator1 (parse-integer (subseq valuef1 0 (position #\b valuef1))))
    (setq denominator1 (parse-integer (subseq valuef1 (+ 1 (position #\b valuef1)))))
    (setq numerator2 (parse-integer (subseq valuef2 0 (position #\b valuef2))))
    (setq denominator2 (parse-integer (subseq valuef2 (+ 1 (position #\b valuef2)))))

    ;; multiply the numerators and denominators
    (setq numerator (* numerator1 denominator2))
    (setq denominator (* denominator1 numerator2))

    ;; find the gcd of the numerator and denominator
    (setq gcd (gcd numerator denominator))
    ;; divide the numerator and denominator with gcd
    (setq numerator (/ numerator gcd))
    (setq denominator (/ denominator gcd))
    ;; combine numerator and denominator b at the middle.
    (let
        (
            (result nil)
        )
        (setq result (concatenate 'string (write-to-string numerator) "b" (write-to-string denominator)))
        (return-from divide_valuef result)
    )
)

;; (defun parser (*tokens*)
;;     (cond
;;         (
;;             ;; if the length of the *tokens_as_symbols* is 3
;;             (equal (length *tokens_as_symbols*) 3)
;;             ;; if the first element is OP_OP and the last element is OP_CP
;;                 (when (and (string= (nth 0 *tokens_as_symbols*) "OP_OP")
;;                     (string= (nth 2 *tokens_as_symbols*) "OP_CP"))

;;                     ;; if the second element is KW_EXIT
;;                     (when (string= (nth 1 *tokens_as_symbols*) "KW_EXIT")
;;                         ;; exit from the program
;;                         (return-from parser "exit")
;;                     )
;;                 )
;;         )
;;         ;; EXPRESSIONS
;;         (
;;             (equal (length *tokens_as_symbols*) 1)
;;             (when (string= (nth 0 *tokens_as_symbols*) "VALUEF")
;;                 (format t "~d~%" (nth 0 *tokens*))
;;             )
;;             (when (string= (nth 0 *tokens_as_symbols*) "IDENTIFIER")
;;                 (format t "~d~%" (nth 0 *tokens*))
;;             )
;;         )
;;         (
;;             ;; if the length of the *tokens_as_symbols* is 5
;;             (equal (length *tokens_as_symbols*) 5)
;;             ;; if the first element is "OP_OP" and the last element is "OP_CP" and third and fourth element is "VALUEF"
;;             (when (and (string= (nth 0 *tokens_as_symbols*) "OP_OP")
;;                 (string= (nth 4 *tokens_as_symbols*) "OP_CP")
;;                 (string= (nth 2 *tokens_as_symbols*) "VALUEF")
;;                 (string= (nth 3 *tokens_as_symbols*) "VALUEF"))
;;                 ;; if the second element is "OP_PLUS"
;;                 (when (string= (nth 1 *tokens_as_symbols*) "OP_PLUS")
;;                     ;; print the result
;;                     (add_valuef (nth 2 *tokens*) (nth 3 *tokens*))
;;                 )
;;                 ;; if the second element is "OP_MINUS"
;;                 (when (string= (nth 1 *tokens_as_symbols*) "OP_MINUS")
;;                     ;; print the result
;;                     (subtract_valuef (nth 2 *tokens*) (nth 3 *tokens*))
;;                 )
;;                 ;; if the second element is "OP_MULT"
;;                 (when (string= (nth 1 *tokens_as_symbols*) "OP_MULT")
;;                     ;; print the result
;;                     (multiply_valuef (nth 2 *tokens*) (nth 3 *tokens*))
;;                 )
;;                 ;; if the second element is "OP_DIV"
;;                 (when (string= (nth 1 *tokens_as_symbols*) "OP_DIV")
;;                     ;; print the result
;;                     (divide_valuef (nth 2 *tokens*) (nth 3 *tokens*))
;;                 )
;;             )
;;         )
;;         (
;;             t
;;             (format t "Syntax error.~%")
;;             (return-from parser "syntax error")
;;         )
;;     )

;; )

(defvar *lookahead* nil)

(defvar result_index 2)

(defun getNextToken ()
    "Returns the next token from the *tokens* list."
    (pop *tokens_as_symbols*)
)

(defun match (expectedToken)
    "Checks whether the expectedToken is equal to the *lookahead* token or not."
    (when (string= *lookahead* expectedToken)
        (setq *lookahead* (getNextToken))
        (return-from match t)
    )
    (return-from match nil)
)

(setq result nil)
(defun EXPR ()
    "Returns a the value of the expression depending on whether the tokens are syntatically correct or not."
    (cond 
        ((string= *lookahead* "OP_OP")
            (match "OP_OP")
            (cond
                ((string= *lookahead* "OP_PLUS")
                    (match "OP_PLUS")
                    (let
                        (
                            (expr1_val nil)
                            (expr2_val nil)
                        )
                        (setq expr1_val (EXPR))
                        (incf result_index)
                        (setq expr2_val (EXPR))
                        (setq result (add_valuef expr1_val expr2_val))
                    )
                )
                (
                    (string= *lookahead* "OP_MINUS")
                        (match "OP_MINUS")
                        (let
                            (
                                (expr1_val nil)
                                (expr2_val nil)
                            )
                            (setq expr1_val (EXPR))
                            (incf result_index)
                            (setq expr2_val (EXPR))
                            (setq result (subtract_valuef expr1_val expr2_val))
                        )
                )
                (
                    (string= *lookahead* "OP_MULT")
                        (match "OP_MULT")
                        (let
                            (
                                (expr1_val nil)
                                (expr2_val nil)
                            )
                            (setq expr1_val (EXPR))
                            (incf result_index)
                            (setq expr2_val (EXPR))
                            (setq result (multiply_valuef expr1_val expr2_val))
                        )
                )
                (
                    (string= *lookahead* "OP_DIV")
                        (match "OP_DIV")
                        (let
                            (
                                (expr1_val nil)
                                (expr2_val nil)
                            )
                            (setq expr1_val (EXPR))
                            (incf result_index)
                            (setq expr2_val (EXPR))
                            (setq result (divide_valuef expr1_val expr2_val))
                        )
                )
                (
                    t
                    (return-from EXPR nil)
                )
            )
            (match "OP_CP")
            (return-from EXPR result)
        )
        (
            (string= *lookahead* "VALUEF")
                (match "VALUEF")
                ;; value of the valuef
                (return-from EXPR (nth result_index *tokens*))
        )
        (
            (string= *lookahead* "IDENTIFIER")
                (match "IDENTIFIER")
                (return-from EXPR t)
        )        
        (
            t
            (return-from EXPR nil)
        )
    )

)



(defun gppinterpreter (&optional file)
    (format t "Type exit to quit.~%Enter the string to be parsed:~%")
    (if file
        ;; if there is a file name given as an argument then read from the file
        (with-open-file (stream file)
            (loop for line = (read-line stream nil)
                while line do
                (setq *tokens* (tokenize_input line))
                (loop for token in *tokens* do
                    (setf result (dfa token))
                    (if (string= result "END_OF_LINE")
                        (return)
                    )
                )
            )
        )
        ;; if there is no file
        (loop
            (format t "> ")
            (let
                ( ;; local variable part
                (input (read-line))
                )
                (when (string= input "exit")
                    (return)
                )
                (setf *tokens_as_symbols* nil)
                (setf result nil)
                (setf result_index 2)
                (setq *tokens* (tokenize_input input))
                (loop for token in *tokens* do
                    (setf result (dfa token))
                    (if (string= result "END_OF_LINE")
                        (return)
                    )
                )
                ;; reverse the *tokens_as_symbols* list
                (nreverse *tokens_as_symbols*)
                ;; (format t "~d~%" *tokens_as_symbols*)
                ;; (setf parser_result (parser *tokens*))
                ;; (when (string= parser_result "exit")
                ;;     (return)
                ;; )
                (setq *lookahead* (getNextToken))
                (setq EXPR_result (EXPR))
                (if (equal EXPR_result nil)
                    (format t "Syntax error.~%")
                    (format t "~d~%" EXPR_result)
                )
            )
        )
  )
)

(gppinterpreter)
