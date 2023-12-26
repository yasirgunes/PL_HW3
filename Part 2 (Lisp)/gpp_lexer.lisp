(setq KEYWORDS (list "and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "def" "for" "if" "exit" "load" "display" "true" "false"))

(setq OPERATORS (list "+" "-" "/" "*" "(" ")" ","))


(defun remove_whitespace (token)
    (let* ((token-length (length token))
        (new-token "")
        (i 0))
        (loop for i from 0 below token-length
            for char = (char token i)
            do
            (when (not (char= char #\Space))
                (setf new-token (concatenate 'string new-token (string char)))
            )
        )
        new-token
    )
)

(defun tokenize_input (input)
    (let* ((token-list '())
        (current-token "")
        (operators "+-*/(),")
        (input-length (length input)))


        (loop for i from 0 below input-length
            for char = (char input i)
            for next-char = (if (< (1+ i) input-length)
                                (char input (1+ i))
                                #\Space)
            do
            (cond
                ((or (alphanumericp char) (digit-char-p char))
                    (setf current-token (concatenate 'string current-token (string char))
                    )
                )

                ((find char operators)

                    (when (not (string= current-token ""))
                        (setf current-token (remove_whitespace  current-token)
                        )
                        (push current-token token-list)
                        (setf current-token "")
                    )
                    (push (string char) token-list)
                )

                ((and (char= char #\Space) (not (char= next-char #\Space))
                  )
                    (when (not (string= current-token ""))
                        (setf current-token (remove_whitespace  current-token)
                        )
                        (when (or (not (string= current-token "")))
                            (push current-token token-list)
                        )
                        (setf current-token "")
                    )
                )

                ((and (char= char #\Semicolon) (char= next-char #\Semicolon)
                 )

                    (when (not (string= current-token ""))
                        (setf current-token (remove_whitespace  current-token)
                        )
                        (push current-token token-list)
                        (setf current-token "")
                    )
                    (push ";;" token-list)
                    (incf i)
                ) ; Skip the second semicolon

                (t
                (setf current-token (concatenate 'string current-token (string char)))
                )
            )
        )

        (setf current-token (remove_whitespace  current-token))

        (when (or (not (string= current-token "")))
            (push current-token token-list)
        )
        (nreverse token-list)
    )
)

(defun is_op (token)
  (if (member (char token 0) OPERATORS :test 'string=)
    t
    nil
  )
)

;; (gppinterpreter)

(defun is_keyword (token)
  ;; check if the token is in the KEYWORDS list.
  (if (member token KEYWORDS :test 'string=)
    t
    nil
  )
)

(defun is_operator (token)
  ;; check if the token is in the OPERATORS list.
  (if (member token OPERATORS :test 'string=)
    t
    nil
  )
)

(defun is_comment (token)
    ;; if there is ";;" inside the token, then it is a comment.
    (if (search ";;" token)
        t
        nil
    )
)

(defvar *state* "START")

(defun dfa (token)
    (setq token_index 0)
    (loop

        (when (string= *state* "START")

            (cond

                (
                    (is_keyword token)
                    (setf *state* "KEYWORD_FOUND")
                )

                (
                    (is_operator token)
                    (setf *state* "OPERATOR_FOUND")
                )

                (
                    (is_comment token)
                    (setf *state* "END_OF_LINE")
                )

                (
                    ;; if token_index == 0 and token[token_index] is a letter
                    (and (= token_index 0) (alpha-char-p (char token token_index)))
                    (setf *state* "IDENTIFIER_EVALUATE")
                    (incf token_index)
                )

                (
                    ;; if token_index == 0 and token[token_index] is a digit
                    (and (= token_index 0) (digit-char-p (char token token_index)))
                    (setf *state* "LITERAL_EVALUATE")
                    (incf token_index)
                )

                (
                    t
                    (setf *state* "INVALID_TOKEN")
                )
            )
        )

        ;; evaluate part
        
        ;; identifier evaluate
        (when (string= *state* "IDENTIFIER_EVALUATE")
            ;; if the char of token[token_index] is a letter or a digit keep evaluating
            ;; if the token_index >= token length then it means identifier is found
            ;; if the char of token[token_index] is not a letter or a digit then it is an invalid token

            (cond
                (
                    (>= token_index (length token))
                    (setf *state* "IDENTIFIER_FOUND")
                )

                (
                    (or (alpha-char-p (char token token_index)) (digit-char-p (char token token_index)))
                    (incf token_index)
                )

                (
                    t
                    (setf *state* "INVALID_TOKEN")
                )
            )
            
            
        )

        ;; literal evaluate
        (when (string= *state* "LITERAL_EVALUATE")
            ;; it should evaulate till it finds a 'b'
            ;; if the token_index >= token length then it means literal is not found because there is no b there. it is an invalid token
            ;; if the char of token[token_index] is not a digit then it is an invalid token

            (cond
                (
                    (>= token_index (length token))
                    (setf *state* "INVALID_TOKEN")
                )

                (
                    (digit-char-p (char token token_index))
                    (incf token_index)
                )

                (
                    (char= (char token token_index) #\b)
                    (incf token_index)
                    (setf *state* "LITERAL_B_EVALUATE")
                )

                (
                    t
                    (setf *state* "INVALID_TOKEN")
                )
                )
        )
            
        ;; literal b evaluate
        (when (string= *state* "LITERAL_B_EVALUATE")
            ;; it should evaluate till the token_index >= token length. if that condition is true then it means literal is found
            ;; the char of token[token_index] should be a digit. if it is not then it is an invalid token

            (cond
                (
                    (>= token_index (length token))
                    (setf *state* "LITERAL_FOUND")
                )

                (
                    (digit-char-p (char token token_index))
                    (incf token_index)
                )

                (
                    (t)
                    (setf *state* "INVALID_TOKEN")
                )
            )
        )





        ;; keyword found
        (when (string= *state* "KEYWORD_FOUND")
            (if (string= token "and")
                (format t "KW_AND~%")
            )
            (if (string= token "or")
                (format t "KW_OR~%")
            )
            (if (string= token "not")
                (format t "KW_NOT~%")
            )
            (if (string= token "equal")
                (format t "KW_EQUAL~%")
            )
            (if (string= token "less")
                (format t "KW_LESS~%")
            )
            (if (string= token "nil")
                (format t "KW_NIL~%")
            )
            (if (string= token "list")
                (format t "KW_LIST~%")
            )
            (if (string= token "append")
                (format t "KW_APPEND~%")
            )
            (if (string= token "concat")
                (format t "KW_CONCAT~%")
            )
            (if (string= token "set")
                (format t "KW_SET~%")
            )
            (if (string= token "def")
                (format t "KW_DEF~%")
            )
            (if (string= token "for")
                (format t "KW_FOR~%")
            )
            (if (string= token "if")
                (format t "KW_IF~%")
            )
            (if (string= token "exit")
                (format t "KW_EXIT~%")
            )
            (if (string= token "load")
                (format t "KW_LOAD~%")
            )
            (if (string= token "display")
                (format t "KW_DISPLAY~%")
            )
            (if (string= token "true")
                (format t "KW_TRUE~%")
            )
            (if (string= token "false")
                (format t "KW_FALSE~%")
            )
            (setf *state* "START")
            (return)
        )

        ;; operator found
        (when (string= *state* "OPERATOR_FOUND")
            (if (string= token "+")
                (format t "OP_PLUS~%")
            )
            (if (string= token "-")
                (format t "OP_MINUS~%")
            )
            (if (string= token "*")
                (format t "OP_MULT~%")
            )
            (if (string= token "/")
                (format t "OP_DIV~%")
            )
            (if (string= token "(")
                (format t "OP_OP~%")
            )
            (if (string= token ")")
                (format t "OP_CP~%")
            )
            (if (string= token ",")
                (format t "OP_COMMA~%")
            )
            (setf *state* "START")
            (return)
        )

        ;; identifier found
        (when (string= *state* "IDENTIFIER_FOUND")
            (format t "IDENTIFIER~%")
            (setf *state* "START")
            (return)
        )

        ;; literal found
        (when (string= *state* "LITERAL_FOUND")
            (format t "VALUEF~%")
            (setf *state* "START")
            (return)
        )

        ;; invalid token
        (when (string= *state* "INVALID_TOKEN")
            (format t "SYNTAX_ERROR ~d cannot be tokenized~%" token)
            (setf *state* "START")
            (return)
        )

        ;; end of line
        (when (string= *state* "END_OF_LINE")
            (format t "COMMENT~%")
            (setf *state* "START")
            (return-from dfa "END_OF_LINE")
        )

    )
)



(format t "Type exit to quit.~%Enter the string to be parsed:~%")

(defun gppinterpreter (&optional file)
    (if file
        ;; if there is a file name given as an argument then read from the file
        (with-open-file (stream file)
            (loop for line = (read-line stream nil)
                while line do
                (setq tokens (tokenize_input line))
                (loop for token in tokens do
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
                (setq tokens (tokenize_input input))
                (loop for token in tokens do
                    (setf result (dfa token))
                    (if (string= result "END_OF_LINE")
                        (return)
                    )
                )
            )
        )
  )
)


(gppinterpreter)


