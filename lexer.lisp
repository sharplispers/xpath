(in-package :xpath)

(defstruct (source (:constructor
                    make-source (text &optional (pos 0))))
  text pos)

(defstruct (token (:constructor
                   make-token (name pos value)))
  name pos value)

(defun skip-whitespace (source)
  (multiple-value-bind (start end)
      (cl-ppcre:scan (rx "\\s+")
                     (source-text source)
                     :start (source-pos source))
    (if (null start)
        (source-pos source)
        (setf (source-pos source) end))))

(defmacro deflexer (name &body rules) nil)

#|
3.7 Lexical Structure

When tokenizing, the longest possible token is always returned.

For readability, whitespace may be used in expressions even though not explicitly allowed by the grammar: ExprWhitespace may be freely added within patterns before or after any ExprToken.

The following special tokenization rules must be applied in the order specified to disambiguate the ExprToken grammar:

    *

      If there is a preceding token and the preceding token is not one of @, ::, (, [, , or an Operator, then a * must be recognized as a MultiplyOperator and an NCName must be recognized as an OperatorName.
    *

      If the character following an NCName (possibly after intervening ExprWhitespace) is (, then the token must be recognized as a NodeType or a FunctionName.
    *

      If the two characters following an NCName (possibly after intervening ExprWhitespace) are ::, then the token must be recognized as an AxisName.
    *

      Otherwise, the token must not be recognized as a MultiplyOperator, an OperatorName, a NodeType, a FunctionName, or an AxisName.

Expression Lexical Structure
[28]    ExprToken          ::=          '(' | ')' | '[' | ']' | '.' | '..' | '@' | ',' | '::'   
                        | NameTest      
                        | NodeType      
                        | Operator      
                        | FunctionName  
                        | AxisName      
                        | Literal       
                        | Number        
                        | VariableReference     
[29]    Literal    ::=          '"' [^"]* '"'   
                        | "'" [^']* "'" 
[30]    Number     ::=          Digits ('.' Digits?)?   
                        | '.' Digits    
[31]    Digits     ::=          [0-9]+  
[32]    Operator           ::=          OperatorName    
                        | MultiplyOperator      
                        | '/' | '//' | '|' | '+' | '-' | '=' | '!=' | '<' | '<=' | '>' | '>='   
[33]    OperatorName       ::=          'and' | 'or' | 'mod' | 'div'    
[34]    MultiplyOperator           ::=          '*'     
[35]    FunctionName       ::=          QName - NodeType        
[36]    VariableReference          ::=          '$' QName       
[37]    NameTest           ::=          '*'     
                        | NCName ':' '*'        
                        | QName 
[38]    NodeType           ::=          'comment'       
                        | 'text'        
                        | 'processing-instruction'      
                        | 'node'        
[39]    ExprWhitespace     ::=          S
|#

(deflexer xpath-lexer
  ("\\(" () :lparen)
  ("\\)" () :rparen)
  ("\\[" () :lbracket)
  ("\\]" () :rbracket)
  ("\\.\\." () :two-dots)
  ("\\." () :dot)
  ("\\@" () :at)
  ("\\," () :comma)
  ("([\\w-]+)\\s*::" (axis-name) (:axis-name axis-name) :two-colons) ;; FIXME: NCName
  ("(comment|text|processing-instruction|node)\\s*\\("
   (node-type) (:node-type (intern (string-upcase node-type) :keyword)))
  ("([\\w-])\\s*\\(" (name) (:function-name name)) ;; FIXME: NCName
  ("(\"[^\"]*\")" (value) (:string value))
  ("'[^']*'" (value) (:string value))
  ("\\d+(?:\\.\\d*)?|\\.\\d+" (:number (parse-number:parse-number value)))
  ("//" () ://)
  ("/" () :/)
  ("\\|" () :pipe)
  ("\\+" () :+)
  ("-" () :-)
  ("=" () :=)
  ("!=" () :!=)
  ("<" () :<)
  ("<=" () :<=)
  (">" () :>)
  (">=" () :>=)
  ("\\*" () (case (token-name prev-token)
              ((:at :two-colons :lparen :lbracket :comma
                    :// :/ :pipe :+ :- := :!= :< :<= :> :>=) (token :star))
              (t (token :multiply))))
  ) ;; FIXME