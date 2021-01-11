;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

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

(set 'flowers '(rose violet daisy buttercup))
;; (rose violet daisy buttercup)
