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
