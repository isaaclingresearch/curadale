(in-package :sexp)

(defun parse (s-expression-string)
  "read an s-expression from a string"
  (with-input-from-string (in s-expression-string)
    (read in)))

(defun stringify (sexp)
  "Convert a Lisp S-expression to its string representation."
  (cond
    ((null sexp) "nil")
    ((atom sexp)
     (format nil "~A" sexp))
    (t
     (format nil "(~{~A~^ ~})" (mapcar #'stringify sexp)))))

(defun ensure-plist (lst)
  "sometimes the llm will return odd elements, remove the odd one"
  (let ((l (length lst)))
    (if (evenp l)
	lst
	(subseq lst 0 (1- l)))))

(defun plistp (lst)
  "Check if LST is a valid property list."
  (and (listp lst)                    ; Ensure it's a list
       (evenp (length lst))           ; Ensure it has an even number of elements
       (loop for (key value) on lst by #'cddr
             always (symbolp key))))  ; Ensure every other element is a valid key
