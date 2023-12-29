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

(defclass defined_function ()
    (
        (name :accessor name :initarg :name)
        ;; (parameters :accessor parameters :initarg :parameters)
        ;; (body :accessor body :initarg :body)
    )

)
;; create a list of functions
(defvar *defined_function* nil)


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

(defvar *lookahead* nil)

(defvar result_index 2)


(setq tokens_copy nil) ;; this is for getting the values of the tokens like 2b1, 4b1 etc.
;; we pop it as we pop *tokens_as_symbols* list so that we can get the values of the correct tokens

(defun getNextToken ()
    "Returns the next token from the *tokens* list."
    (pop tokens_copy)
    (pop *tokens_as_symbols*)
)

(defun match (expectedToken)
    "Checks whether the expectedToken is equal to the *lookahead* token or not."
    ;; (when (string= *lookahead* expectedToken)
    ;;     (setq *lookahead* (getNextToken))
    ;;     (return-from match t)
    ;; )
    ;; (return-from match nil)
    (let
        (
            (result nil)
        )
        (when (string= *lookahead* expectedToken)
            (setq result (nth 0 tokens_copy))
            (setq *lookahead* (getNextToken))
            (return-from match result)
        )
    )
)

(defun isExpressionStart ()
    "Checks whether the *lookahead* token is an expression start or not."
    (cond
        (
            (string= *lookahead* "OP_OP")
                (match "OP_OP")
                (cond
                    (
                        (string= *lookahead* "OP_PLUS")
                            (match "OP_PLUS")

                            ;; fresh start for the next expression
                            (setq *tokens_as_symbols* (copy-list copy_tokens_as_symbols))
                            (setq tokens_copy (copy-list *tokens*))
                            (setq *lookahead* (getNextToken))
                            (setq tokens_copy (copy-list *tokens*)) ;; copying again because it should follow 1 index behind
                            (return-from isExpressionStart t)
                    )
                    (
                        (string= *lookahead* "OP_MINUS")
                            (match "OP_MINUS")

                            ;; fresh start for the next expression
                            (setq *tokens_as_symbols* (copy-list copy_tokens_as_symbols))
                            (setq tokens_copy (copy-list *tokens*))
                            (setq *lookahead* (getNextToken))
                            (setq tokens_copy (copy-list *tokens*)) ;; copying again because it should follow 1 index behind

                            (return-from isExpressionStart t)
                    )
                    (
                        (string= *lookahead* "OP_MULT")
                            (match "OP_MULT")

                            ;; fresh start for the next expression
                            (setq *tokens_as_symbols* (copy-list copy_tokens_as_symbols))
                            (setq tokens_copy (copy-list *tokens*))
                            (setq *lookahead* (getNextToken))
                            (setq tokens_copy (copy-list *tokens*)) ;; copying again because it should follow 1 index behind
                            

                            (return-from isExpressionStart t)
                    )
                    (
                        (string= *lookahead* "OP_DIV")
                            (match "OP_DIV")

                            ;; fresh start for the next expression
                            (setq *tokens_as_symbols* (copy-list copy_tokens_as_symbols))
                            (setq tokens_copy (copy-list *tokens*))
                            (setq *lookahead* (getNextToken))
                            (setq tokens_copy (copy-list *tokens*)) ;; copying again because it should follow 1 index behind


                            (return-from isExpressionStart t)
                    )
                    (
                        (string= *lookahead* "VALUEF")
                            (match "VALUEF")

                            ;; fresh start for the next expression
                            (setq *tokens_as_symbols* (copy-list copy_tokens_as_symbols))
                            (setq tokens_copy (copy-list *tokens*))
                            (setq *lookahead* (getNextToken))
                            (setq tokens_copy (copy-list *tokens*)) ;; copying again because it should follow 1 index behind

                            (return-from isExpressionStart t)
                    )
                    (
                        (string= *lookahead* "IDENTIFIER")
                            (match "IDENTIFIER")

                            ;; fresh start for the next expression
                            (setq *tokens_as_symbols* (copy-list copy_tokens_as_symbols))
                            (setq tokens_copy (copy-list *tokens*))
                            (setq *lookahead* (getNextToken))
                            (setq tokens_copy (copy-list *tokens*)) ;; copying again because it should follow 1 index behind


                            (return-from isExpressionStart t)
                    )
                    (
                        t

                        (setq *tokens_as_symbols* (copy-list copy_tokens_as_symbols))
                        (setq tokens_copy (copy-list *tokens*))
                        (setq *lookahead* (getNextToken))
                        (setq tokens_copy (copy-list *tokens*)) ;; copying again because it should follow 1 index behind
                        
                        (return-from isExpressionStart nil)
                    )
                )
                
            
        )
        (
            (string= *lookahead* "VALUEF")
                (match "VALUEF")

                ;; fresh start for the next expression
                (setq *tokens_as_symbols* (copy-list copy_tokens_as_symbols))
                (setq tokens_copy (copy-list *tokens*))
                (setq *lookahead* (getNextToken))
                (setq tokens_copy (copy-list *tokens*)) ;; copying again because it should follow 1 index behind


                (return-from isExpressionStart t)
        )
        (
            (string= *lookahead* "IDENTIFIER")
                (match "IDENTIFIER")

                ;; fresh start for the next expression
                (setq *tokens_as_symbols* (copy-list copy_tokens_as_symbols))
                (setq tokens_copy (copy-list *tokens*))
                (setq *lookahead* (getNextToken))
                (setq tokens_copy (copy-list *tokens*)) ;; copying again because it should follow 1 index behind


                (return-from isExpressionStart t)
        )
        (
            t
            
            (setq *tokens_as_symbols* (copy-list copy_tokens_as_symbols))
            (setq tokens_copy (copy-list *tokens*))
            (setq *lookahead* (getNextToken))
            (setq tokens_copy (copy-list *tokens*)) ;; copying again because it should follow 1 index behind

            (return-from isExpressionStart nil)
        )
    )
)

(defun isFunctionStart ()
    "Checks whether the *lookahead* token is a function start or not."
    (cond
        (
            (string= *lookahead* "OP_OP")
                (match "OP_OP")
                (cond
                    (
                        (string= *lookahead* "IDENTIFIER")
                            (match "IDENTIFIER")

                            ;; fresh start for the next expression
                            (setq *tokens_as_symbols* (copy-list copy_tokens_as_symbols))
                            (setq tokens_copy (copy-list *tokens*))
                            (setq *lookahead* (getNextToken))
                            (setq tokens_copy (copy-list *tokens*)) ;; copying again because it should follow 1 index behind

                            (return-from isFunctionStart t)
                    )
                    (
                        t
            
                        (setq *tokens_as_symbols* (copy-list copy_tokens_as_symbols))
                        (setq tokens_copy (copy-list *tokens*))
                        (setq *lookahead* (getNextToken))
                        (setq tokens_copy (copy-list *tokens*)) ;; copying again because it should follow 1 index behind

                        (return-from isFunctionStart nil)
                    )
                )
        )
        (
            t

            (setq *tokens_as_symbols* (copy-list copy_tokens_as_symbols))
            (setq tokens_copy (copy-list *tokens*))
            (setq *lookahead* (getNextToken))
            (setq tokens_copy (copy-list *tokens*)) ;; copying again because it should follow 1 index behind

            (return-from isFunctionStart nil)
        )
    )
)

(defun START ()
    (cond
        (
            (isExpressionStart)
            (return-from START (EXPR))
        )
        (
            (isFunctionStart)
            (return-from START (FUNCTION))
        )
        (
            (string= *lookahead* "OP_OP")
            (match "OP_OP")
            (if (string= *lookahead* "KW_EXIT")
                (progn
                    (match "KW_EXIT")
                    (if (string= *lookahead* "OP_CP")
                        (progn
                            (match "OP_CP")
                            (return-from START "exit")
                        )
                    )
                )
                (return-from START nil) ;; if there is no KW_EXIT then it is a syntax error
            )
        )
        ;; if there is no OP_OP then it is a syntax error
        (
            t
            (return-from START nil)
        )
    )
    
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
                        (setq expr2_val (EXPR))
                        
                        ;; if expr1_val or expr2_val are not strings then it is a syntax error
                        (if (and (stringp expr1_val) (stringp expr2_val))
                            (setq result (add_valuef expr1_val expr2_val))
                            (return-from EXPR nil)
                        )
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
                            (setq expr2_val (EXPR))

                            ;; if expr1_val or expr2_val are not strings then it is a syntax error
                            (if (and (stringp expr1_val) (stringp expr2_val))
                                (setq result (subtract_valuef expr1_val expr2_val))
                                (return-from EXPR nil)
                            )
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
                            (setq expr2_val (EXPR))
                            
                            ;; if expr1_val or expr2_val are not strings then it is a syntax error
                            (if (and (stringp expr1_val) (stringp expr2_val))
                                (setq result (multiply_valuef expr1_val expr2_val))
                                (return-from EXPR nil)
                            )
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
                            (setq expr2_val (EXPR))
                            ;; if expr1_val or expr2_val are not strings then it is a syntax error
                            (if (and (stringp expr1_val) (stringp expr2_val))
                                (setq result (divide_valuef expr1_val expr2_val))
                                (return-from EXPR nil)
                            )
                        )
                )
                (
                    t
                    (return-from EXPR nil)
                )
            )
            ;; if there is no OP_CP then it is a syntax error
            (if (string= *lookahead* "OP_CP")
                (match "OP_CP")
                (return-from EXPR nil)
            )
            (return-from EXPR result)
        )
        (
            (string= *lookahead* "VALUEF")
                (match "VALUEF")
                ;; value of the valuef
                ;; (return-from EXPR (nth result_index *tokens*))
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
                (setq *tokens* (tokenize_input input))
                (loop for token in *tokens* do
                    (setf result (dfa token))
                    (if (string= result "END_OF_LINE")
                        (return)
                    )
                )
                ;; reverse the *tokens_as_symbols* list
                (nreverse *tokens_as_symbols*)

                ;; copy the *tokens_as_symbols* list to *copy_tokens_as_symbols*
                (setq copy_tokens_as_symbols (copy-list *tokens_as_symbols*))

                ;; copy the *tokens* list to tokens_copy
                (setq tokens_copy (copy-list *tokens*))
                (setq *lookahead* (getNextToken))
                (setq tokens_copy (copy-list *tokens*)) ;; copying again because it should follow 1 index behind
                (setq START_result (START))

                ;; if else if else statements.
                (cond
                    ( ;; if the result is "exit" then exit the program
                        (string= START_result "exit")
                            (return)
                    )
                    ( ;; else if the result is nil then there is a syntax error
                        (string= START_result nil)
                            (format t "Syntax error.~%")
                    )
                    ( ;; else print the result
                        t
                            (format t "~a~%" START_result)
                    )
                )
            )
        )
  )
)

(gppinterpreter)
