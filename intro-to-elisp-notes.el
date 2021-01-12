;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

;; 1 - Summary

;; lisp is made up expressions:
;; they are: lists, or single atoms

;; atoms are:
;; multi-character symbols, like 'foo'; single-char symbols like '+',
;; strings between double quotes like "foobar stuff", or numbers like
;; 12

;; strings, numbers evaluate to themselves and return the same.

;; single quote (') means what follows is taken literally, don't interpret
;; it, it is an alias for (quote take-this-literally)

;; Evaluating a symbol by itself, e.g.,
1
;; its value is returned, rather than calling it as a function like
;; (global-auto-revert-mode)
global-auto-revert-mode
;; => t

;; parentheses tell the lisp interpreter to treat e.g., 'buffer-name'
;; as functions, without parens the interpreter would attempt to
;; evaluate the symbols as variables.

;; To write the evaluation to the buffer use C-u C-x C-e instead
;; of C-x C-e, which will echo the return value to the mini-buffer.

; Basics
"Strings evalute to themselves, they are atoms."
"Strings evalute to themselves, they are atoms."

1 ; numbers eval to themselves
1

() ; empty parens eval to special 'nil'
nil

(equal nil ()) ; nil and () is 'true'
t

'(none of this will be eval'd because it is quoted)
(none of this will be eval 'd because it is quoted)

(quote some-function)
some-function

;; 1.8 Arguments
(message "This message in echo area")
"This message in echo area"

(message "This message in %s" (buffer-name))
"This message in *scratch*"

(message "There are %d %s in the office!"
	 (- fill-column 14) "pink elephants")
"There are 56 pink elephants in the office!"

(message "He saw %d %s"
	 (- fill-column 32)
	 (concat "red "
		 (substring
		  "The quick brown foxes jumped." 16 21)
		 " leaping."))

;; You can also use %s to print a number, it's non-specific

;; 1.9 Setting the Value of a Variable

;; In the jargon, we call it 'binding' a variable to a value.

;; what is returned is the list quoted below, but the side-effect
;; is binding the symbol "flowers" is bound to the list. What
;; we want most of the time is the side-effects.
(set 'flowers '(rose violet daisy buttercup))
;; (rose violet daisy buttercup)

;; Every lisp function must return a value if it does not get an error!

;; setq is alias for (set (quote carnivores) '(lion tiger leopard))
(setq carnivores '(lion tiger leopard))

;; setq can assign different values to different variables in one go:
(setq trees '(pine fir oak maple)
      herbivores '(gazelle antelope zebra))
;; the last sexp will return and the side-effect is it binds
;; the symbols trees and herbivores to the lists given

;; another way to say it is that setq and set makes the symbol point to
;; the list.

;; in lisp a symbol can point to both a function definition or a
;; value, under the same symbol! What is evaluates to depends on how
;; we evaluate the symbol! A symbol can only ever point to one
;; function definition, but which one it is can be changed.

(setq counter 0)
(setq counter (+ counter 1))

;; 2 - Practicing Evaluation

;; Even typing plain text involves evaluating a function, in this
;; case 'self-insert-command'

(buffer-name)
;; "intro-to-elisp-notes.el"

(buffer-file-name)
;; "c:/Users/cmaceachern/AppData/Roaming/.emacs.d/intro-to-elisp-notes.el"

;; a file and buffer are two different entities. A file is written on
;; disk, a buffer does not have to be, it exists in emacs memory.
;; when loading a file it is said we are 'visiting' that file, changes
;; to the buffer (copy of the file) are not changing the file itself
;; until we write back to disk. In that case the (buffer-file-name)
;; evaluation will return nil.

;; A name and the object or entity to which the name (symbol) refers
;; are different from each other. My name is "Craig" but those letters
;; are not ME!
(current-buffer)
;; #<buffer intro-to-elisp-notes.el>

;; Above is what emacs returns as a textual-representation of the
;; buffer entity. A printed representation of the name of the buffer
;; without the contents of the buffer. This special format indicates
;; the buffer itself is being returned rather than just its name.

(other-buffer)
;; #<buffer init.el>

(switch-to-buffer (other-buffer))
;; will switch to the buffer returned by other-buffer

;; 'set-buffer' function only does one thing: switch the cpu attention
;; to a different buffer. switch-to-buffer does that but also loads
;; that buffer in current window, which is probably what user wants.

(buffer-size)
;; 4592. Tells you the size of current buffer.

(point)
;; 4610. The current position of the cursor is called "point" in emacs.

(point-min)
;; 1. Always 1 unless narrowing in effect.

(point-max)
;; 4790. Max permissible value of point in the current buffer.

;; 3 - How to Write Function Definitions

;; all functions are defined in terms of other functions in lisp except for a
;; small few special primitive ones that are written in C (for elisp anyway).

;; in elisp we do not distinguish b/t C/Lisp functions, the difference is
;; irrelevent to the end writer of elisp.

;; 'defun' is a Macro (in elisp terms). The create a piece of code that
;; is run when we evaluate a symbol that points to it (the function
;; definition) we write a lisp expression that starts with the
;; the symbol 'defun'.

;; A function def has up to 5 parts following 'defun' symbol:
;; 1. Name of the symbol to which the following func definition will bind.
;; 2. A list of args to pass the function. If no args this is '()', empty list.
;; 3. Documentation string (technically option but don't skip this!)
;; 4. '(interactive)' and expression that allows us to do M-x our-new-function
;; so we/users can call this function interactively. This is like a flag.
;; 5. The body of the function definition, which returns the value of the last
;; evaluated expression from the body when it finishes.

;; the symbol 'number' will be bound to the argument given to this
;; function for the duration of the lifetime of this function.
;; the 'let' expression is a way to create these local-bindings as well,
;; and lispers use it often (let ... (... .. )) to create lexical bindings
;; , meaning symbol bound to a particular value for that scope/context.
(defun multiply-by-seven (number)
  "Multiplies NUMBER by 7 and returns."
  (* 7 number))
;; evaluating the above in the jargon is called 'installing' a function.

(multiply-by-seven 8)
;; 56

;; interactive-version, requires, e.g., C-u [0-9] M-x multiply-by-seven
(defun multiply-by-seven (number)
  "Multiplies NUMBER by 7 and returns."
  (interactive "p")
  (message "The result is %d" (* 7 number)))
;; now we can do e.g., C-u 5 M-x multiply-by-seven<RET>
;; => The result is 35

;; most function in elisp are interactive:
;; C-u 10 M-x previous-line<RET>
;; moves 10 lines up using M-x, or just do:
;; C-u 10 C-p
;; because C-p is bound to that function
;; or we can use alternative method with Meta/Alt key and do:
;; M-10 C-p
;; to really shorten it. Lots of people rebind M-[0-9] though so beware!

;; NOTE: for whatever reason, bare C-u without a number defaults to passing
;; 4 as the arg., i.e., as if you had pressed C-u 4 M-x ...

;; Options for (interactive ...)

;; (interactive "p\ncZap to char: ")
;; 'p' part is familiar from above, says interpret a prefix (C-u ...)
;; '\n' is needed to separate each interactive argument, here 'p' and 'c'
;; 'c' tells the function the name of character to which to delete up to.
;; This 'p\nc' construct allows the function to accept both a prefix
;; argument, e.g., C-u 2 M-xZap to char<RET> and then accept another
;; argument (the 'c') that you enter when prompted. This function will
;; delete up to character 'c', 'p' many times, i.e., delete up to
;; nth occurence of character 'c' passed in.

;; If a function does not require args, then 'interactive' does not
;; require any, simply put (interactive).

;; 'load' function causes emacs to evaluate and thereby install each of
;; functions found in the argument give it (the filename).

;; let expression

;; (let VARLIST BODY...)
;; var list might look like this:
;; (thread (needles 3))
;; this will by default assign 'nil' to symbol 'thread' because
;; we did not give it one, and bind value 3 to symbold 'needles'

;; the VARLIST is a list (varlist in the jargon), where each element
;; of is either a symbol by itself (will be bound to 'nil') or a
;; 2 element list, the symbol and the value to bind it to (for the
;; duration of the 'let' expression body.
(let (first-var-no-value-defined-so-nil
      (second-var-with-value 3)
      (third-var-with-value "goofy"))
  (message "Inside the body of the let! I have access to: %d"
	   second-var-with-value))

;;second-var-with-value
;; will throw error when eval'd because it was defined within a let,
;; so not know outside of that context.

(let ((zebra "stripes")
      (tiger "fierce"))
  (message "One kind of animal has %s and another is %s"
	   zebra tiger))
;; "One kind of animal has stripes and another is fierce"

(let ((birch 3)
      pine
      fir
      (oak 'some))
  (message
   "Here are %d vars with %s, %s, and %s values."
   birch pine fir oak))
;; "Here are 3 vars with nil, nil, and some values."

(if (> 5 4)
    (message "5 is greater than 4 obviously!"))
;; "5 is greater than 4 obviously!"

(defun type-of-animal (characteristic)
  "Print message in echo area depending on CHARACTERISTIC. If the
CHARACTERISTIC is string \"fierce\", then warn of a tiger."
  (if (equal characteristic "fierce")
      (message "It is a tiger!")))
(type-of-animal "fierce")
;; "It is a tiger!"
(type-of-animal "striped")
;; nil

;; if-then-else: second expression is the 'else' one
(if (> 4 5)
    (message "4 falsely greater than 5!")
  (message "4 is not greater than 5!"))
;; "4 is not greater than 5!"

;; The result of a test is considered true ('t' symbol) if value
;; returned is anything other than 'nil' or '()' (empty list which
;; is alias/same as 'nil' to the interpreter).

(if 4
    'true
  'false)
;; true

(if nil
    'true
  'false)
;; false

;; 'save-excursion' saves origin point location before doing its
;; body and then puts the point back. Used all the time in conjunction
;; with let in functions. Without it the point can jump all over the
;; place as side-effect, which is not the user experience we want.

;; e.g.,
;; (let (foo
;;       (bar 3))
;;     (save-excursion
;;        BODY...))

;; 3 Exercises


;; Write a non-interactive function that doubles the value of its
;; argument, a number.  Make that function interactive.
(defun doubler (n)
  "Doubles the value of N."
  (+ n n))
(doubler 3)
;; 6
(doubler "foo")
;; number-or-maker error, as expected with '+' symbol function defition

;; Write a function that tests whether the current value of
;; ‘fill-column’ is greater than the argument passed to the function,
;; if so, prints an appropriate message.
(defun fill-column-exercise (column)
  "Test whether current value of fill-column is greater than COLUMN."
  (interactive "p")
  (> fill-column column))
(fill-column-exercise 100)
;; nil
(fill-column-exercise 50)
;; t

;; 4 - A Few Buffer-Related Functions

;; simplified goto beginning of buffer definition
(defun simplified-beginning-of-buffer ()
  "Move point to start of buffer; leave mark at previous position."
  (interactive)
  (push-mark)				; set mark at current point
  (goto-char (point-min)))		; goto first possible point location

;; mark-whole-buffer function definition
;; in emacs 22 it looked like this:
(defun mark-whole-buffer-22 ()
  "Docs here."
  (interactive)
  (push-mark (point))
  (push-mark (point-max) nil t)
  (goto-char (point-min)))

;; append-to-buffer definition
(defun append-to-buffer (buffer start end)
  "Append to specified buffer the text of the region.
It is inserted into that buffer before its point.

When calling from a program, give 3 args:
BUFFER (or buffer name), START, and END.
START and END specify the portion of the current buffer to be copied."
  (interactive
   (list (read-buffer "Append to buffer: " (other-buffer
					    (current-buffer) t))
	 (region-beginning) (region-end))) ; interactive expr end
  ;; defun BODY
  (let ((oldbuf (current-buffer)))
    ;; let BODY
    (save-excursion
      (let* ((append-to (get-buffer-create buffer))
	     (windows (get-buffer-window-list append-to t t))
	     point)
	;; let* BODY
	(set-buffer append-to)
	(setq point (point))
	(barf-if-buffer-read-only)
	(insert-buffer-substring oldbuf start end)
	(dolist (window windows)
	  (when (= (window-point window) point)
	    (set-window-point window (point))))))))

;; 'interactive' portion:
;; it starts with a list with these parts:
;; First part of the list is an expression to read the name of a buffer
;; and return it as a string (read-buffer expr). It requires a prompt,
;; the "Append to buffer: " we pass it, and the second arg tells
;; read-buffer what value to provide if user does not specify anything--
;; 'other-buffer' and 'current-buffer' here.

;; (other-buffer (current-buffer) t) explained:
;;
;; Here the 'other-buffer' first arg is the exception, we give it
;; the function 'current-buffer'. The second arg is 't' symbol for
;; 'true' value; this is a flag that tells 'other-buffer' that it can
;; show visible buffers except in thist case it will not show current buffer
;; which makes sense.

;; (list (..) (2nd) (3rd)) explained: 2nd and 3rd arguments to 'list':
;; 2nd arg: 'region-beginning' expression
;; 3rd arg: 'region-end' expression
;; They specify start and end of text to be appended, as numbers.

;; First (let ..) explanation: BODY part of the 'defun' expression:
;; the varlist of the let is:
;; (oldbuf (current-buffer) which binds oldbuf symbol to the value
;; returned by evaluating current-buffer function definition.

;; The varlist of a let are surrounded by () to distinguish to interpreter
;; where varlist ends and BODY begins. So it really looks like this:
;; (let ((oldbuf (current-buffer))) BODY...)

;; next is the save-excursion BODY of the let, which saves and restores
;; the previous point and buffer after performs ITS own BODY.

;; (save-excursion
;;   FIRST-EXPR
;;   SECOND-EXPR
;;   ...
;;   LAST-EXPR)

;; 'let*' symbol => like 'let' but enables emacs to set each variable
;; in its varlist in sequence, one after another. Using let* you can
;; then use the value of an early set variable from the varlist while
;; still IN THE VARLIST!

;; in our case we bind the symbol 'append-to' in the first let* varlist
;; expression, and then in the second varlist expression we use the
;; value of 'append-to' that was just bound!

;; SUMMARY of append-to-buffer:
;; ‘append-to-buffer’ works as follows: it saves the value
;; of the current buffer in the variable called ‘oldbuf’.  It gets the new
;; buffer (creating one if need be) and switches Emacs’s attention to it.
;; Using the value of ‘oldbuf’, it inserts the region of text from the old
;; buffer into the new buffer; and then using ‘save-excursion’, it brings
;; you back to your original buffer.


;; 4 - Exercises

;; 1.Write your own ‘simplified-end-of-buffer’ function definition; then
;;   test it to see whether it works.


(defun simplified-end-of-buffer ()
  "Go to further possible location for point in current buffer."
  (interactive)
  (goto-char (point-max)))

;; 2.Use ‘if’ and ‘get-buffer’ to write a function that prints a message
;;   telling you whether a buffer exists.
(defun does-buffer-exist? (buf)
  "Print message telling you whether BUFFER exists."
  (interactive "B")			; use B to allow non-existant bufs
  (if (get-buffer buf)
      (message "Buffer exists.")
    (message "Buffer does not exist.")))

;; 3.Using ‘xref-find-definitions’, find the source for the
;;   ‘copy-to-buffer’ function.

;; Answer: just press M-. on top of the 'copy-to-buffer' text above.
;; M-, to come back.


;; 5 - A Few More Complex Functions

;; body of 'copy-to-buffer' looks like this:

;; ...
;; (interactive "BCopy to buffer: \nr")	; B to allow non-existant bufs
;; (let ((oldbuf (current-buffer)))
;;   (with-current-buffer (get-buffer-create buffer)
;;     (barf-if-buffer-read-only)
;;     (erase-buffer)
;;     (save-excursion
;;       (insert-buffer-substring oldbuf start end))))

;; (with-current-buffer (get-buffer-create buffer) ... explained:
;; Use the buffer with the name specifed as the one to which you are copying
;; or if it doesn't exist, create it. Then, with-current-buffer evals
;; its BODY with that buffer temporarily current (context).

;; with-current-buffer is a newer and easier mechanism that doing the
;; save-excursion/set-buffer combo.

;; (barf-if-buffer-read-only) and
;; (erase-buffer) explained:
;; error out right here if buffer is set to read only, and if not then
;; continue to erase-buffer which does just that.

;; outline of 'copy-to-buffer' function definition in pseudo-code:
;;
;; (let (BIND-oldbuf-TO-VALUE-OF-current-buffer)
;;     (WITH-THE-BUFFER-YOU-ARE-COPYING-TO
;;       (BUT-DO-NOT-ERASE-OR-COPY-TO-A-READ-ONLY-BUFFER)
;;       (erase-buffer)
;;       (save-excursion
;;         INSERT-SUBSTRING-FROM-oldbuf-INTO-BUFFER)))
