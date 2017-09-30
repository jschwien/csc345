
;;Joseph Schwien
;;CSC 345
;;Professor Wyatt


(defun my-replace (e1 e2 L)
  "function will deep replace a member in the list that is eqqual to e1 and replace it with e2"
  (cond ((endp L) nil)
	  
	  ;;if first/current member of the list is equal to e1 replace it with e2 put it back with rest of list
	((equal (first L) e1)(cons e2 (my-replace e1 e2 (rest L))))
	  
	  ;; if a member of the list is a list it self we cons whats on the list with the current list
	((listp (first L)) (cons (my-replace e1 e2 (first L)) (my-replace e1 e2 (rest L))))
	  
	  ;;if the current value is not equal to e1 and not a list cons the rest of the list 
	(t(cons (first L) (my-replace e1 e2 (rest L))))))
		       


(defun fibonacci (n)
	"this function will use the text book use of double recursion to do fibonacci sequence"
	
	;;checks for base case if 1 return 0 or if 2 return 1
  (cond ((eql n 1)0)
	((eql n 2)1)

	  ;;if the function is not done evaluating it will recusivley call on itself until it reaches base case
	((+ (fibonacci(- n 1)) (fibonacci(- n 2))))))

(defun fibonacci-TR (n)
	
"this function will use tail recursion to evaluate the fibonacci sequence"
	
;;have auxilary function that will hold accumulator values and current value
  (labels((fibonacci-extra (oneacc twoacc current)

		;; checks for base case first, if current value is larger then 3 we ad accumulators 
	    (cond((eql current 1) 0)
		 ((eql current 2) 1)
		 ((eql current 3)(+ oneacc twoacc))

		   ;;Will recursively call itself until ir reaches its base case
		 ((fibonacci-extra (+ oneacc twoacc) oneacc (- current 1))))))
;;call au
(fibonacci-extra 1 0 n)))








;;; FOR USE IN HOMEWORK 1

;;;==================================================================================
;;; a macro to do FOR loops  ;; equivalent to Java for-loop control: for (int var=start, var<=stop; var+=update) body
(defmacro for ((var start stop update) &body body)
  (let ((gstop (gensym))                    ;; generate new symbols, GUARANTEED to be new; prevents capture
	(gupdate (gensym)))
    `(do ( (,gupdate ,update)               ;; needed so that the update expression is evaluated just once
	   (,var ,start (+ ,gupdate ,var))  ;; needed so that the stop expression is evaluated just once
	   (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))


;; EXAMPLE
;; CL-USER> (for (i 1 6 1) (print i))     ;; equivalent to Java for-loop control: (for int i=1, i<=6; i++)
;;
;; 1 
;; 2 
;; 3 
;; 4 
;; 5 
;; 6 
;; NIL

;;;==================================================================================
(defun comparefibonaccis()
  (for (i 10 35 5)                               ;; for (int i=10; i<=35, i+=5) ...
       (format t "TAIL REC FIBONACCI ~a~%" i)
       (time(fibonacci-TR i))
       (format t "FIBONACCI ~a~%" i)
       (time(fibonacci i))
       (format t "=======================================================~%")
       )
  )

;;;==================================================================================
;;; END 
