(defun my-replace (e1 e2 L)
  "function will deep replace a member in the list that is eqqual to e1 and replace it with e2"
  (cond ((endp L) nil)
	((equal (first L) e1)(cons e2 (my-replace e1 e2 (rest L))))
	((listp (first L)) (cons (my-replace e1 e2 (first L)) (my-replace e1 e2 (rest L))))
	(t(cons (first L) (my-replace e1 e2 (rest L))))))
		       


(defun fibonacci (n)
  (cond ((eql n 1)0)
	((eql n 2)1)

	((+ (fibonacci(- n 1)) (fibonacci(- n 2))))))

(defun fibonacci-TR (n)

  (labels((fibonacci-extra (oneacc twoacc current)


	    (cond((eql current 1) 0)
		 ((eql current 2) 1)
		 ((eql current 3)(+ oneacc twoacc))

		 ((fibonacci-extra (+ oneacc twoacc) oneacc (- current 1))))))

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
