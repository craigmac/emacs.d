;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

;; each example is doubles: top line is what I wrote and below line
;; is what is printed to the buffer when you hit C-j (eval-print-last-sexp),
;;
;; keys here in emacs-lisp-mode:
;; C-x C-e (eval-last-expr) if after the sexp you want to eval, and result is
;; put to the minibuffer
;; C-M-x (eval-defun) eval top-level form containing current point, good when
;; inside a defun and don't want to move point to end to do C-x C-e

()
nil

;; empty list and nil are false, everything else is truthy
'()
nil

;; nil is an atom
nil
nil

;; numbers are atoms
1
1

1.0
1.0


3.234
3.234

-1.2
-1.2

0.45
0.45

;; strings are atoms, eval to themselves
"string stuff ehre"
"string stuff ehre"

"1"
"1"

"nil"
"nil"

;; return t is arg is nil, otherwise return nil
(null nil)
t

(null "not nil")
nil

;; '() is equal to nil
(null '())
t


;; car is first element, note list is quoted, otherwise interpreter
;; would try to do (1 2 3) aka call func 1 with args 2 and 3, which would fail
(car '(1 2 3))
1


;; cdr is rest of list
(cdr '(1 2 3))
(2 3)

;; builtin in (nth N LIST) is easier if you known N position, 0 indexed
(nth 1 '(1 2 3))
2


(nth 0 '("shell" "stuff" 0 2 3 4))
"shell"

;; return nil on index overflow
(nth 5 '())
nil



;; building lists
;; note quoted (2 3) otherwise tries to eval call fn 2 with arg 3
(cons 1 '(2 3))
(1 2 3)

;; innermost has to start with a blank list '() to append onto,
;; good thing about emtpy list is it's useful recursion base case as well
(cons 1 (cons 2 (cons 3 '())))
(1 2 3)


;; cons et al are non-destructive
;; concat all args and make result a list
(append '(1 2) '(3 4))
(1 2 3 4)

;; variables
(set (quote some-list) '(1 2 3))
(1 2 3)

some-list
(1 2 3)

;; easier quoting with ' which just does (quote arg) replacement
(set 'some-list '(1 2 3))
(1 2 3)

some-list
(1 2 3)

;; setq is even easier and just does (set 'foo)
(setq some-list '(1 2 3))
(1 2 3)


;; let is to do local scope variablels
;; first arg is a list of lists, which is where you define vars and their
;; initial values, and the second arg is a code to execute within the context
;; of those vars just bound
(let (
      (a "local")
      (b " defined variables!"))
  (message (concat a b)))
"local defined variables!"


(let ((a 1) (b 5))
  (format "a is %d and b is %d" a b))
"a is 1 and b is 5"


;; functions
(defun say-hello ()
  (message "hello!"))

(say-hello)				; prints to minibuffer

(defun square (x)
  (* x x))

(square 2)				; prints 4 (#o4, #x4, ?\C-d)
;; which means octal, hex, and character? 4

(defun distance (x1 y1 x2 y2)
  (sqrt (+ (square (- x2 x1))
	   (square (- y2 y1)))))

(distance 3 0 0 4)
;; 5.0

;; conditionals
;; when takes a predicate, a fn that return t or false, nil
(when (= (+ 2 2) 4)
  (message "passed sanity check!"))
;; prints the minibuffer

(defun evens-or-odds (n)
  (if (= 0 (% n 2))
      "even!"
    "odd!"))

(evens-or-odds 2)
;; "even!"

(evens-or-odds 3)
;; "odd!"

(defun pick-a-word (n)
  (cond
   ((= n 1) "baba")
   ((= n 2) "bouffet")
   ((= n 3) "cabada")
   ("otherwise case/default case")))

(pick-a-word 4)
;; "otherwise case/default case"

(pick-a-word 1)
;; "baba"

(= 0 t)					; wrong-type-argument error

;; recursion example, elisp does not have tail-call optimization
(defun factorial (n)
  (if (< n 1)				; base case
      1					; return 1 here on base case
    ;; else case, call again 
    (* n (factorial (- n 1)))))

(factorial 6)				; 720

;; anonymous functions
(lambda (x) (* x x x))			; => (lambda (x) (* x x x))

;; lambda itself is executable, here passing an arg of 5 to the first
;; fn which is a lambda
((lambda (x ) (* x x x)) 5)		; 125

;; bind lambda to symbol 'cube' using fset
;; reason not set is because symbol can point to function space or variable space
;; so this is setting the value of symbol cube function
(fset 'cube (lambda (x) (* x x x)))	; => (lambda (x) (* x x x))

(cube 4)				; 64, evaled as func
cube					; symbol value as variable is void, but it could have one!

(setq cube "symbol's value!")

cube					; "symbol's value!" now
(cube 4)				; 64, called as a function so eval looks up symbol's value as function, and evals that


;; higher order fn, apply mapcar first arg to all in second arg, list filter in other
;; languages
(mapcar 'upcase '("foo" "bar" "baz"))	; ("FOO" "BAR" "BAZ")

(mapc 'upcase '())			; nil

;; global keybinding
;; (global-set-key (kbd "M-#") 'sort-lines)

major-mode				; => emacs-lisp-mode

;; set keybinding in certain mode
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (local-set-key (kbd "<f5>") 'recompile))) ; returns last sexp, the whole lambda

;; shortcut to returning strings that set-key types can work with
(kbd "C-h k")				;"^Hk"



















