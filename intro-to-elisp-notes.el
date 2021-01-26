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

;; Case study of 'insert-buffer' function definition:
;; it copies another buffer into current buffer. Reverse of append|copy-to-buf

;; Original code (simplified later in 2003, but harder to understand):

;; IMPORTANT: shows 'interactive' with a buffer that might be read only!
;; and the distinction between the NAME of an object and the ACTUAL object
;; NAME refers to.

(defun tutorial-insert-buffer (buf)
  "Insert after point the contents of BUF.
Puts mark after the inserted text. BUF can be buffer or buffer name."
  ;; "*b" for situation when current buffer is a read-only buffer.
  ;; if old-insert-buffer called when current buffer RO, error msg
  ;; printed and beep/blink may happen (if not turned off).
  ;; "B" allows non-existing buffers but we need BUF to be existing
  ;; and not read-only
  (interactive "*bInsert buffer: ")
  ;; 'or' purpose: ensure BUF not bound to a buffer AND not just
  ;; the NAME of a buffer. Could be done with 'if' as well like:
  ;; (if (not (real-buff? buf) get-real-buffer buf)) pseudo-code
  ;; or:
  ;; (if (not (bufferp buf))
  ;;     (setq buf (get-buffer buf)))
  ;; The job is make sure BUF is a buffer object not just a buffer
  ;; name. If it is just a name of a buffer, we grab the actual
  ;; buffer object.
  (or (bufferp buf)
      (setq buf (get-buffer buf)))
  ;; code that copies the other buffer (BUF) into current buffer
  (let (start end newmark)		; set these to nil
    (save-excursion
      (save-excursion
	(set-buffer buf)			  ; switch target to BUF
	;; bind our let variables start and end to full buffer range
	(setq start (point-min) end (point-max))) ; inner save-ex. done
      (insert-buffer-substring buf start end)
      (setq newmark (point)))		; outer save-excursion done
    ;; sets a mark to this location: end of inserted text
    (push-mark newmark))) 		; let and defun done

;; 'bufferp' returns true if arg is a buffer object, false is just
;; the name of a buffer. 'p' is added to many function names to show
;; they are 'predicates' (test whether argument/s are true or false).

;; newer Body for insert-buffer
;; (push-mark
;;  (save-excursion
;;    (insert-buffer-substring (get-buffer buf))
;;    (point)))
;;  nil

;; buffer object returned by 'get-buffer' is passed to 'insert-buffer-sub..'
;; which inserts the whole of the buffer since we did not specify anything
;; else (no start or end ranges).

;; location into which buffer was inserted is recorded by push-mark, then
;; we return nil, the value of the last command because we are doing this
;; for side-effect only, not to return any value.

;; 5.3 Complete Definition of 'beginning-of-buffer'

;; 'beginning-of-buffer' function args: M-< without prefix calls it as is,
;; to move to beginning of the accessible portion of the current buffer,
;; or you can use e.g., C-u 7 M-< to move to the point 70% of the way
;; through the buffer (alt. M-7 M-<).

;; keyword '&optional arg' allows arg to be optional argument in elisp.

;; pseudo-code of full definition

;; (defun tutorial-beginning-of-buffer (&optional arg)
;;   "Documentation string..."
;;   (interactive "P")
;;   (or (is-the-argument-a-cons-cell arg)
;;       (and are-both-transient-mark-mode-and-mark-active-true)
;;       (push-mark))
;;   (let (determine-size-and-set-it)
;;     (goto-char
;;      (if-there-is-an-argument
;;       figure-out-where-to-go
;;       else-go-to
;;       (point-min))))
;;   do-nicety)

;; "P" in interactive means pass raw prefix arg if there is one to
;; the function.

;; goto-char uses if-then-else expression to figure out where to put
;; the cursor if there is an arg that is not a cons cell. cons explained
;; later chapters. It checks through if- expression here whether
;; ARG has a non-nil value and whether it is a cons cell (consp).

;; If ARG is not nil and is not a cons cell: true when 'beginning-of-buffer'
;; is called with a numeric ARG via e.g., M-7 or C-u 7, then this test will
;; return true and the 'then' part of the 'if' expression is eval'd.
;; Else part is eval'd is the function was called without an argument, e.g.,
;; M-< directly with no prefix argument (e.g., M-5 or C-u 5). Else part
;; simply goes to 'point-min' value, start of accessible buffer.

;; full definition:
(defun tutorial-beginning-of-buffer (&optional arg)
  "Move point to the beginning of the buffer; leave mark at previous position.
With \\[universal-argument] prefix, do not set mark at previous position.
With numering arg N, e.g., M-7 or C-u 7 prefix, put point N/10 of the way
from the beginning.

If the buffer is narrowed, this command uses the beginning and size
of the accessible part of the buffer. Don't use this command in
Lips programs! \(goto-char (point-min)) is faster and avoids clobbering mark."
  (interactive "P")
  (or (consp arg)
      (and transient-mark-mode mark-active)
      (push-mark))
  (let ((size (- (point-max) (point-min))))
    (goto-char (if (and arg (not (consp arg)))
		   (+ (point-min)
		      (if (> size 10000) ; avoid overflow for large buffers
			  (* (prefix-numeric-value arg)
			     (/ size 10))
			;; else
			(/ (+ 10 (* size (prefix-numeric-value arg)))
			   10)))
		 (point-min))))		; let finish
  (if (and arg (not (consp arg))) (forward-line 1)))

;; using \\[...] in the doc string tells lisp intepreter to sub whatever
;; key is current bound to the symbol in [...] and show that instead, e.g.,
;; "With C-u prefix, ..."


;; 5.4 Review

;; 'or': evaluate each arg given and return first argument that is non-nil,
;; aka true, if none return 'nil'.

;; 'and': eval each arg given and if no args eval to nil, return last argument
;; given to 'and' (truthy return). Returns 'nil' upon first arg eval'd to 'nil'

;; '&optional': keyword used before function arg name to denote optional
;; argument to user.

;; 'prefix-numeric-value': convert raw prefix arg produced by
;; '(interactive "P")' to a numeric value.

;; 'forward-line': move point to beginning of next line, or if arg is > 1,
;; that many lines forward of point. If it can't go any further it returns
;; the number of additional lines it was supposed to move but could not.

;; 'erase-buffer': delete entire contents of current buffer.

;; 'bufferp': return 't' if arg is a buffer object (NOT just a name of a
;; buffer); otherwise, returns 'nil'.

;; 5.5 Exercise

;; Write an interactive function with an optional argument that tests
;; whether its argument, a number, is greater than or equal to, or else,
;; less than the value of ‘fill-column’, and tells you which, in a message.
;; However, if you do not pass an argument to the function, use 56 as a
;; default value.

(defun tutorial-greater-than-fill-column-p (&optional arg)
  (interactive "P")
  ;; value of arg seems to be 1 if we don't pass arg, i.e., we
  ;; just run it directly with M-x tutorial-greater-than-fill-column-p<RET>
  ;; so let's use that to default to 56 when arg is 1.
  (let ((n (prefix-numeric-value arg)))
    (if (= n 1) (setq n 56))
    ;; let body
    (if (or
	 (> n fill-column)
	 (= n fill-column))
	(message "Arg value is greater or equal to fill-column")
      (message "Arg value is less than fill-column"))))


fill-column
;; 70

(setq arg 1)
;; 1

(equal arg 1)
;; t

(= arg 1)
;; t

;; 6 - Narrowing and Widening

;; narrowing means to focus on a part of buffer without affecting other
;; parts of buffer. Like opening a window with just the selected text in it.
;; widening is the opposite. Putting the narrowed buffer region back into its
;; context (region) of the whole buffer.

;; narrow-to-region, i.e., C-x n n
;; widen-to-region, i.e., C-x n w

;; often lisp functions need the whole buffer so they remove
;; any narrowing near the beginning of the function definition
;; and restoring it after.

;; done with (save-restriction BODY... ) special form. If used with
;; save-excursion which is done often, save-excursion should come first,
;; aka be the outer expression

;; 6.2 - 'what-line' command

(defun tutorial-what-line ()
  "Print current line num in the buffer of point."
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (message "Line %d"
	       (1+ (count-lines 1 (point)))))))

;; 6.3 Exercise

;; Write a function that will display the first 60 characters of the
;; current buffer, even if you have narrowed the buffer to its latter half
;; so that the first line is inaccessible.  Restore point, mark, and
;; narrowing.  For this exercise, you need to use a whole potpourri of
;; functions, including ‘save-restriction’, ‘widen’, ‘goto-char’,
;; ‘point-min’, ‘message’, and ‘buffer-substring’.

(defun first-60-chars-of-buffer ()
  "Display first 60 characters of the current buffer, widened."
  (save-restriction
    (widen)
    (message (buffer-substring point-min 60))))


;; 7 'car','cdr','cons': Fundamental Functions

;; cons is short for 'construct'.
;; 'car' for 'Contents of Address Register' aka first location
;; in a list.

;; 'cdr' is contents of Decrement Register aka whole list minus
;; first element in the list

;; car and cdr are non-destructive (don't alter original list)
(car '(rose violet daisy))
;;rose

(cdr '(rose violet daisy buttercup))
;; (violet daisy buttercup)

(cdr '((lion tiger cheetah)
       (gazelle antelope zebra)
       (whale dolphin seal)))
;; ((gazelle antelope zebra) (whale dolphin seal))

(cons 'pine '(fir oak maple))
;;(pine fir oak maple)

(cons 'buttercup ())			; first needs empty list!
;; (buttercup)

(cons 'daisy '(buttercup))		; has to cons onto a list!
;; (daisy buttercup)

(cons 'violet '(daisy buttercup))
;; (violet daisy buttercup)

(length '(buttercup))
;; 1

(length (cons 'violet '(daisy buttercup)))
;; 3

(length ())
;; 0

;; 7.3 'nthcdr'

(cdr (cdr '(pine fir oak maple)))
;; (oak maple)

;; easier with nthcdr. non-destructive.
(nthcdr 2 '(pine fir oak maple))
;; (oak maple)

;; nil when element index does not exist
(nthcdr 5 '(pine fir))
;; nil

;; 7.4 'nth'

;; get value for n position (0-indexed) in a list
(nth 0 '(one two three))
;; one

;; return nil if out of bounds index passed
(nth 5 '(five six))
;; nil

;; 7.5 'setcar' - destructive (changes original list)

;; way it works is that it can't have a quoted list, so
;; we need to use the 'list' function to create one, manual says
;; a quoted form like '(a b c d e) would yield a list that
;; is part of the program and bad things could happen if we tried
;; to change part of the program while running it. He says generally
;; program's components should be constant while program is running.
;; QUESTION: how does '(...) versus (list '... '...) differ?
(setq animal (list 'a 'b 'c 'd 'e))

animal
;; (a b c d e)

(setcar animal 'hippo)
;; hippo
;; returns this value but we don't need it

animal
;; (hippo b c d e)
;; removed 'a' and put in 'hippo' instead

;; 7.6 'setcdr'

;; like 'setcar' but for cdr: replaces the second and subsequent
;; elements of a list but keeps the first element. Think of it like
;; a push operation in other languages.

(setq more-animals (list 'horse 'cow 'sheep 'goat))
;; (horse cow sheep goat)

(setcdr more-animals '(cat dog))
;; (cat dog)

more-animals
;; (horse cat dog)

;; 7.7 Exercise

;; Construct a list of four birds by evaluating several expressions with
;; ‘cons’.  Find out what happens when you ‘cons’ a list onto itself.
;; Replace the first element of the list of four birds with a fish.
;; Replace the rest of that list with a list of other fish.


;; 8 - Cutting and Storing Text

;; 8.1 'zap-to-char' function definition
(defun tutorial-zap-to-char (arg char)
  "Kill up to and including ARG'th occurrence of CHAR.
     Case is ignored if `case-fold-search' is non-nil in the current buffer.
     Goes backward if ARG is negative; error if CHAR not found."
  (interactive "p\ncZap to char: ")
  (if (char-table-p translation-table-for-input)
      (setq char (or (aref translation-table-for-input char) char)))
  (kill-region (point) (progn
                         (search-forward (char-to-string char)
                                         nil nil arg)
                         (point))))
;; 'progn' is a special form that causes each of its arg to evaluated
;; in sequence and then returns the value of the last one. The args in
;; between are evaluated only for the side-effects, like switching buffer
;; or moving point. The values produced by them are discarded inside of
;; of progn, except for the last argument.

;; 8.2 'kill-region'

;;  kill-region in essence calls condition-case which takes 3 args:
;; in this function the 1st arg does nothing; the 2nd arg contains
;; the code that does the work when all goes well; the 3rd arg contains
;; the code called in case of an error.

(defun tutorial-kill-region (beg end)
  "Kill (\"cut\") text between point and mark.
     This deletes the text from the buffer and saves it in the kill ring.
     The command \\[yank] can retrieve it from there. ... "

  ;; Since order matters, pass point first.
  (interactive (list (point) (mark)))
  ;; And tell us if we cannot cut the text.
  ;; 'unless' is an 'if' without a then-part.
  (unless (and beg end)
    (error "The mark is not set now, so there is no region"))

  ;;  'condition-case' takes three arguments.
  ;;    If the first argument is nil, as it is here,
  ;;    information about the error signal is not
  ;;    stored for use by another function.
  (condition-case nil

      ;;  The second argument to 'condition-case' tells the
      ;;    Lisp interpreter what to do when all goes well.

      ;;    It starts with a 'let' function that extracts the string
      ;;    and tests whether it exists.  If so (that is what the
      ;;    'when' checks), it calls an 'if' function that determines
      ;;    whether the previous command was another call to
      ;;    'kill-region'; if it was, then the new text is appended to
      ;;    the previous text; if not, then a different function,
      ;;    'kill-new', is called.

      ;;    The 'kill-append' function concatenates the new string and
      ;;    the old.  The 'kill-new' function inserts text into a new
      ;;    item in the kill ring.

      ;;    'when' is an 'if' without an else-part.  The second 'when'
      ;;    again checks whether the current string exists; in
      ;;    addition, it checks whether the previous command was
      ;;    another call to 'kill-region'.  If one or the other
      ;;    condition is true, then it sets the current command to
      ;;    be 'kill-region'.
      (let ((string (filter-buffer-substring beg end t)))
        (when string                    ;STRING is nil if BEG = END
          ;; Add that string to the kill ring, one way or another.
          (if (eq last-command 'kill-region)
              ;;    − 'yank-handler' is an optional argument to
              ;;    'kill-region' that tells the 'kill-append' and
              ;;    'kill-new' functions how deal with properties
              ;;    added to the text, such as 'bold' or 'italics'.
              (kill-append string (< end beg) yank-handler)
            (kill-new string nil yank-handler)))
        (when (or string (eq last-command 'kill-region))
          (setq this-command 'kill-region))
        nil)

    ;;   The third argument to 'condition-case' tells the interpreter
    ;;    what to do with an error.
    ;;    The third argument has a conditions part and a body part.
    ;;    If the conditions are met (in this case,
    ;;             if text or buffer are read-only)
    ;;    then the body is executed.
    ;;    The first part of the third argument is the following:
    ((buffer-read-only text-read-only) ;; the if-part
     ;; ...  the then-part
     (copy-region-as-kill beg end)
     ;;    Next, also as part of the then-part, set this-command, so
     ;;    it will be set in an error
     (setq this-command 'kill-region)
     ;;    Finally, in the then-part, send a message if you may copy
     ;;    the text to the kill ring without signaling an error, but
     ;;    don't if you may not.
     (if kill-read-only-ok
         (progn (message "Read only text copied to kill ring") nil)
       (barf-if-buffer-read-only)
       ;; If the buffer isn't read-only, the text is.
       (signal 'text-read-only (list (current-buffer)))))))

;; 8.3 'copy-region-as kill'

;; copy region of text from a buffer and saves it in kill-ring

;; if copy-region-as-kill is called right after kill-region, we append
;; the newly copied text to the previously copied text (from the kill-region)
;; , this way when you yank back you get the whole thing. If some
;; other command precedes 'copy-region-as-kill' we just create a separate
;; entry in the kill ring instead of added it to the last element in the
;; kill ring

(defun tutorial-copy-region-as-kill (beg end)
  "Save the region as if killed, but don't kill it.
     Details of some side-effects:
     In Transient Mark mode, deactivate the mark.
     If `interprogram-cut-function' is non-nil, also save the text for a window
     system cut and paste."
  (interactive "r")			; r for region (start end numbers)
  ;; explained in text above why, append-case test
  (if (eq last-command 'kill-region)
      (kill-append (filter-buffer-substring beg end) (< end beg))
    ;; else
    (kill-new (filter-buffer-substring beg end)))
  (if transient-mark-mode		; per doc string explanation
      ;; stops the region from highlighting
      (setq deactivate-mark t))
  nil)					; return nil

;; this-command variable: normally when function executing emacs sets it
;; to the value of function being executed, and the 'last-command' value
;; is set to previous 'this-command' value.

;; kill-append function defintion and explanation:
(defun tutorial-kill-append (string before-p &optional yank-handler)
  "Append STRING to end of latest kill in kill ring. If BEFORE-P is
non-nill, prepend instead."
  (let* ((cur (car kill-ring)))
    ;; use BEFORE-P arg to determine whether to prepend STRING to cur,
    ;; or else append STRING to cur.
    (kill-new (if before-p
		  (concat string cur)
		(concat cur string))
	      ;; REPLACE argument to 'kill-new', return first non-nil
	      ;; expression as the REPLACE argument
	      (or (= (length cur) 0)	; nothing in kill-ring
		  (equal yank-handler
			 (get-text-property 0 'yank-handler cur)))
	      ;; &optional yank-handler argument, return it
	      yank-handler)))

;; concat examples
(concat "abc" "def")
;; "abcdef"

(concat "new "
	(car '("first element" "this will be gone")))
;; "new first element"

(concat (car
	 '("first element" "foobar")) " modified")
;; "first element modified"

;; 'kill-new' function and explanation
(defun tutorial-kill-new (string &optional replace yank-handler)
  "Make STRING the latest kill in the kill ring.
     Set `kill-ring-yank-pointer' to point to it.

     If `interprogram-cut-function' is non-nil, apply it to STRING.
     Optional second argument REPLACE non-nil means that STRING will replace
     the front of the kill ring, rather than being added to the list.
     ..."
  (if (> (length string) 0)
      (if yank-handler
          (put-text-property 0 (length string)
                             'yank-handler yank-handler string))
    (if yank-handler
        (signal 'args-out-of-range
                (list string "yank-handler specified for empty string"))))
  ;; fboundp tests whether given symbol is bound to function, which it will
  ;; be if menu bar is activated. When it is
  (if (fboundp 'menu-bar-update-yank-menu)
      (menu-bar-update-yank-menu string (and replace (car kill-ring))))
  ;; critical lines
  (if (and replace kill-ring)		; kills exists AND replace arg given
      ;; then
      (setcar kill-ring string)		; destructive. changes first element
    ;; else
    (add-to-list kill-ring string)	; self-explanatory, used to be 'push'
    ;; 60 kills by default. If larger set last element to 'nil' by
    ;; using 'nthcdr' and 'setcdr' by grabbing last element and then
    ;; using setcdr to set it to 'nil'
    (if (> (length kill-ring) kill-ring-max)
	;; avoid overly long kill ring size
        (setcdr (nthcdr (1- kill-ring-max) kill-ring) nil)))
  (setq kill-ring-yank-pointer kill-ring)
  ;;  add copied string to whatever os level copy paste function might
  ;; exist, e.g., on x11 'x-select-text' function does this.
  (if interprogram-cut-function
      (funcall interprogram-cut-function string (not replace))))

;; 'push' function explanation: (formerly in above fn where add-to-list is)

(setq example-list '("Here is a clause" "another clause"))
;; ("Here is a clause" "another clause")

example-list
;; ("Here is a clause" "another clause")

(push "a third clause" example-list)
;; ("a third clause" "Here is a clause" "another clause")

;; how setcdr and nthcdr work together:

(setq trees (list 'maple 'oak 'pine 'birch))
;; (maple oak pine birch)

(nthcdr 1 trees)
;; (oak pine birch)

(nthcdr 2 trees)
;; (pine birch)

;; setcdr sets the cdr of a list, so it needs a list and a value
;; to set that element to so we pass it the 2 cdr, which is
;; (pine birch) and the setcdr sets birch to nil, effectively
;; removing it from the list.
(setcdr (nthcdr 2 trees) nil)
;; nil

trees
;; (maple oak pine)
;; no more birch!

;; 8.4 Digression into C

;; 'copy-region-as-kill' uses -> 'filter-buffer-substring' (elisp), which
;; uses 'delete-and-extract-region' function written in C, not elisp.
;; It is written as an instance of a C macro (template for code).

;; 8.5 Initializing a Variable with 'defvar'

;; defvar is a special form (name comes from 'define variable')

;; similar to 'setq' but only sets value of the variable if the var
;; does not already have a value. defvar also has a doc string.

;; kill-ring is defined using it, and sets default list to nil if
;; nothing is in it.

(defvar tutorial-kill-ring nil
  "List of killed text sequences...")

tutorial-kill-ring
;; nil

;; defvar with '*' explanation:

;; use 'defcustom' now instead, because it a macro that hooks into the
;; 'customize' help system and has much better integration especially
;; if other users going to use this variable.

;; In the past, elisp writers would add asterisk like:
;; "*Buffer name for ..." in documentation string of defvar to show
;; user they might want to change it. Simply a convention. Use
;; defcustom instead!

;; 'set-variable' command will change value of a symbol will emacs
;; is running but it does not make it permanent. That's what init.el/.emacs
;; file is for.

;; Use M-x set-variable<RET> to see what variables you can set in your
;; .emacs file!

;; 9 - How Lists are Implemented

;; the atom 'rose' is recorded as four contiguous letters: 'r', 'o',
;; 's', 'e' (like in C strings).

;; A list is kept differently: it is kept using a series of 'pairs of pointers'

;; In the series, the first pointer of a pair point to: atom or another list.
;; the second pointer in the pair points to: next pair of pointers, or symbol
;; 'nil' (marks end of list)

;; pointers here having same meaning as in C, memory address of what is
;; pointed to.

;; linked lists, basically.

;; 'bouquet' symbol holds address of the first pair of boxes
(setq bouquet '(rose violet buttercup))
;; (rose violet buttercup)

;; can be drawn like this:
     ;; bouquet
     ;;  |
     ;;  |    --------------       ---------------       ----------------
     ;;  |   | car   | cdr  |     | car    | cdr  |     | car     | cdr  |
     ;;   -->| rose  |   o------->| violet |   o------->| butter- |  nil |
     ;;      |       |      |     |        |      |     | cup     |      |
     ;;       --------------       ---------------       ----------------

;; the structure of a symbol is made up of address: 'bouquet' consists
;; of a group of address-boxes (memory locations), 1 of which is the address
;; start for the printed word 'bouquet' and the 2nd of which is the
;; address of any function definition attached to that symbol, if any,
;; and the 3rd in this case is the address of the first pair of address
;; for the list '(rose violet buttercup).

;; use cdr to bind a symbol only sets the address to where the symbol
;; points to (further down the list, in bare cdr case, the memory
;; address of the second element of a list), but does not alter the
;; original list, we are reassign a new value to the symbol's pointer.

;; in lisp/elisp a pair of 'address-boxes' as we have been referring to them
;; are called a "cons cell" or a "dotted pair". 'cons' function adds
;; a new pair of address to the front a series of addresses.

(setq flowers (cdr bouquet))
;; (violet buttercup)

(setq bouquet (cons 'lily bouquet))
;; (lily rose violet buttercup)

;; can be diagrammed:

     ;; bouquet                       flowers
     ;;   |                             |
     ;;   |     ___ ___        ___ ___  |     ___ ___       ___ ___
     ;;    --> |   |   |      |   |   |  --> |   |   |     |   |   |
     ;;        |___|___|----> |___|___|----> |___|___|---->|___|___|--> nil
     ;;          |              |              |             |
     ;;          |              |              |             |
     ;;           --> lily      --> rose       --> violet    --> buttercup

;; however this does not change the value of symbol 'flowers':

flowers
;; (violet buttercup)

bouquet
;; (lily lily rose violet buttercup)

(eq (cdr (cdr bouquet)) flowers)
;; t

;; In Lisp, to get the cdr of list -> just get the address of next cons cell
;; in the series. For car of list -> get the address of the first element
;; of the list. Simple. To 'cons' (construct) a new element on a list,
;; we are just added a new 'cons cell' to the front of the list, e.g.,
;; (setq my-list (cons 'new-symbol my-list))

;; 9.1 - Symbols as a Chest of Drawers (metaphor)

;; a symbol being a chest with 'drawers'. what is put in each drawer is
;; the address of a value or function definition, if any. We can see
;; a symbol as a chest with 4 drawers:

;;'bouqet' symbol as a 'chest of drawers' metaphor:

             ;;     Chest of Drawers            Contents of Drawers

             ;;     __   o0O0o   __
             ;;   /                 \
             ;;  ---------------------
             ;; |    directions to    |            [map to]
             ;; |     symbol name     |             bouquet
             ;; |                     |
             ;; +---------------------+
             ;; |    directions to    |
             ;; |  symbol definition  |             [none]
             ;; |                     |
             ;; +---------------------+
             ;; |    directions to    |            [map to]
             ;; |    variable value   |             (rose violet buttercup)
             ;; |                     |
             ;; +---------------------+
             ;; |    directions to    |
             ;; |    property list    |             [not described here]
             ;; |                     |
             ;; +---------------------+
             ;; |/                   \|

;; we can see that a symbol contains 4 main 'drawers'.

;; 9.2 - Exercise

;; Set ‘flowers’ to ‘violet’ and ‘buttercup’.  Cons two more flowers on to
;; this list and set this new list to ‘more-flowers’.  Set the CAR of
;; ‘flowers’ to a fish.  What does the ‘more-flowers’ list now contain?

(setq flowers (list 'violet 'buttercup))
;; (violet buttercup)

(setq more-flowers (cons 'dandelion flowers))
;; (dandelion violet buttercup)

(setq more-flowers (cons 'rose flowers))
;; (rose violet buttercup)

more-flowers
;; (rose violet buttercup)

(setcar flowers 'tuna)
;; tuna

flowers
;; (tuna buttercup)

more-flowers
;; (rose tuna buttercup)
;; now second element (cdr more-flowers) has had its address updated
;; by the (setcar flowers 'tuna) call, which is destructive (changes it).
;; so now why we we evaluated more-flowers, the address lookup stored in
;; (cdr more-flowers), second element of this list, returns the newly
;; updated value 'tuna' instead of returning previous 'violet'.

;; 10 - Yanking Text Back

;; kill ring is simple a list of textual strings, i.e.:
;; ("foo" "bar" "more")

;; C-y brings back "foo" first, and M-y cycles to next element in the list,
;; so "bar", only if C-y was first pressed (last command was a yank). If
;; you press M-y without a preceeding yank command, M-y tells you yank
;; command didn't happen first, so it won't fetch next element.

;; insertion code part for 'yank' and 'yank-pop' functions is:
;; (insert (car kill-ring-yank-pointer))
;; 'kill-ring-yank-pointer' is what tracks current address to fetch
;; from the kill-ring.

;; 10.3 Exercises

;; Using ‘C-h v’ (‘describe-variable’), look at the value of your kill
;; ring.  Add several items to your kill ring; look at its value
;; again.  Using ‘M-y’ (‘yank-pop)’, move all the way around the kill
;; ring.  How many items were in your kill ring?  Find the value of
;; ‘kill-ring-max’.  Was your kill ring full, or could you have kept
;;  more blocks of text within it?

(length kill-ring)
;; 29

kill-ring-max
;; 60

;; Using ‘nthcdr’ and ‘car’, construct a series of expressions to
;; return the first, second, third, and fourth elements of a list.

(defalias '1st 'car)

(1st '("car" "train"))
;; "car"

(defun 2nd (l)
  "Return second element of list L."
  (interactive)
  (car (nthcdr 1 l)))

(2nd '("rose" "gold" "tree"))
;; "gold"

(defun 3rd (l)
  "Return third element of list L."
  (interactive)
  (car (nthcdr 2 l)))

(3rd '("one" "two" "three" "four"))
;; "three"

(defun 4th (l)
  "Return fourth element of list L."
  (interactive)
  (car (nthcdr 3 l)))

(4th '("one" "two" "three" "four"))
;; "four"

;; 11 - Loops and Recursion

;; 11.1 'while' function

;; special form; if first arg is false, interpreter skips rest of
;; expression body of while, if first arg true, evals body then
;; tests first arg again.

;; (while true-or-false-test
;;   BODY...)

;; the value returned by eval a while is value of the true of false test.
;; as side-effect, while loop will return 'nil' or false regardless
;; if it has looped 1 or 1000 times or none at all. A while expr
;; that evals successfully never returns a true value! And we dont' care,
;; because we are using while for it's side effect.

(setq empty-list ())
;; nil

empty-list
;; nil

(setq animals '(gazelle giraffe lion tiger))
;; (gazelle giraffe lion tiger)

;; (while animals
;;   ...)
;; would run while-body as long as there are items in the list. So, forever,
;; or as some people do, we have a form that cdr's the list down as it loops
;; each time, basically create a bounded for loop.

;; 11.1.2 'print-elements-of-list' function

;; illustrates the 'while' loop on a list:

(setq animals '(gazelle giraffe lion tiger))
;; (gazelle giraffe lion tiger)

(defun tut-print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (while list
    (print (car list))
    (setq list (cdr list))))

(tut-print-elements-of-list animals)
;; nil (output will print to echo area one on each line with 'nil' at end,
;; or if you do C-u C-x C-e, will show animals in echo area and print 'nil'
;; to buffer text. When while loop ends it always returns a nil because test
;; failed, which is fine and what we want to happen.

(defun tut-triangle (number-of-rows)
  "Showing incrementing loop details."
  (let ((total 0)
	(row-number 1))
    (while (<= row-number number-of-rows)
      (setq total (+ total row-number))
      (setq row-number (1+ row-number)))
    total))

(tut-triangle 4)
;; 10

;; 11.2 'dolist' and 'dotimes'

;; dolist works like a while loop that cdrs down the list, just
;; easier to remember and write. dolist and dotimes are macros.

;; dotimes macro similar to dolist, expecpt it loops a specific number of times

;; (let (value)
;;   (dotimes (number 3)
;;     (setq value (cons number value)))
;;   value)

;; 11.3 Recursion

;; (defun NAME-OF-RECURSIVE-FUNCTION (ARGUMENT-LIST)
;;   "DOCUMENTATION..."
;;   (if DO-AGAIN-TEST
;;     BODY...
;;     (NAME-OF-RECURSIVE-FUNCTION
;;          NEXT-STEP-EXPRESSION)))

(setq animals '(gazelle giraffe lion tiger))
;; (gazelle giraffe lion tiger)

(defun print-elements-recursively (list)
  "Print each element of LIST on a line of its own.
     Uses recursion."
  (when list                            ; do-again-test
    (print (car list))              ; body
    (print-elements-recursively     ; recursive call
     (cdr list))))                  ; next-step-expression

(print-elements-recursively animals)
;; nil (prints to echo area and returns 'nil' when 'when' fails

;; recursion instead of a counter version
(defun recurse-triangle (number)
  "Use recursion this time."
  (if (= number 1)			; base-recursion test
      1					; return 1
    (+ number
       (recurse-triangle
	(1- number)))))			; next-step expression

(recurse-triangle 7)
;; 28

;; recursion version using 'cond' (conditional, like switch statement
;; in C). Returns nil is none of its true or false test true.
;; (cond
;;  (test-one first-consequent)
;;  (test-two second-consequent)
;;  (..)
;;  (test-N N-consequent))

(defun tutorial-triangle-using-cond (number)
  (cond ((<= number 0) 0)
        ((= number 1) 1)
        ((> number 1)
         (+ number (triangle-using-cond (1- number))))))

;; 11.3.6 Recursive Patterns

;; common recursive patterns, each involving a list.

;; 'every' pattern of recursion: an action is performed on every
;; element of a list:
;; * If list empty return 'nil'
;; * Else, act on car of the list:
;;  * through recursive call by the function on the rest (cdr) of the list,
;;  * and optionally combine the acted-on element, using 'cons',
;;    with the results of acting on the rest.

;; example:
(defun square-each (numbers-list)
  "Doo doo"
  (if (not numbers-list)
      nil
    (cons
     (* (car numbers-list) (car numbers-list)) ; double the current num
     (square-each (cdr numbers-list)))))       ; next-step expression


(square-each '(1 2 3))
;; (1 4 9)

;; 'accumulate' pattern, is like 'every' pattern using 'cons' except that
;; 'cons' is not used, but some other combiner function.

;; example:
(defun add-elements (numbers-list)
  "Foo doo"
  (if (not numbers-list)
      0
    (+ (car numbers-list) (add-elements (cdr numbers-list)))))

(add-elements '(2 4 6 8))
;; 20

;; 'keep' pattern: each element is tested and the results are kept only
;; if the element meets a criterion.

(defun keep-three-letter-words (words)
  "Return a list of three letter words found in WORDS."
  (cond
   ((not words) nil)		; stop/base condition
   ;; condition 2, current symbol is length of 3
   ((eq 3 (length (symbol-name (car words))))
    (cons (car words) (keep-three-letter-words (cdr words))))
   ;; cond 3, decide when to skip element, recursively call with shorter list
   (t (keep-three-letter-words (cdr words)))))

(keep-three-letter-words '(one two three four five six))
;; (one two six)

;; 11.3.7 Recursion without Deferments

;; usually with recursions the intermediary calculations are deferred
;; until the recursion is done and then they are all done, the drawback of
;; this is that it requires more storage for cpu to be able to unwind
;; all the way back to first call of the recursive function.

;; 11.3.8 No Deferment Solution

;; Pattern is usually done this way: write two function defintions, an
;; initialization function and a helper function which does the work and
;; returns after each run. The phrase "tail recursive" is used to describe
;; such a process, one that uses constant space.

(defun triangle-initialization (number)
  "FOOBAR."
  (triangle-recursive-helper 0 0 number))

(defun triangle-recursive-helper (sum counter number)
  "Return SUM, using COUNTER, through NUMBER, inclusive. This is the helper
component of a two function duo that uses recursion."
  (if (> counter number)
      sum
    (triangle-recursive-helper (+ sum counter) ; sum
			       (1+ counter)    ; counter
			       number)))       ; number
(triangle-initialization 2)
;; 3

;; 12 - Regular Expression Searches

;; covering 'forward-sentence', 'forward-paragraph' and 're-search-forward'

;; 'sentence-end' is bound to a pattern that marks eof of a sentence.
;; by default it finds sentence end as two spaces after a period,
;; question mark or exclamtion mark. However in a file the two spaces
;; can be a tab or EOL as well so we need to include these 3 items as
;; alternates:

;; \\($\\| \\|  \\)
;;   EOL  Tab SPCs

;; 2 backslashes required before | and ( to denote special, the first
;; backslash quoting the second so than an actual backslash is read, e.g.,
;; \( is read.

;; 16 - Your '.emacs' File
