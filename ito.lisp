;; ito oriented design
;(in-package :cl-user)
;
;(defpackage :ito
;  (:use :cl)
;  (:export
;     :defito 
;     :intend-skip 
;     :intend-t 
;     :intend-f 
;     :intend-eq 
;     :intend-neq 
;     :intend-equal 
;     :intend-notequal 
;     :intend-multiple-equal 
;     :intend-multiple-notequal 
;     :intend->
;     :intend->=
;     :intend-<
;     :intend-<=
;     :intend-=
;     :ito-set
;     :report-result
;    )
; )
;
;(in-package :ito)

;; Intent framework from test framework
;; based on the code of ch. 9 of Practical Commonlisp by Peter Seibel.
;;  modified it as a novice commonlisper

(defparameter *ito-name* nil)

(defmacro defito (name param desc &body body)
  `(defun ,name ,param
     (let ((*ito-name* (append *ito-name* (list ',name))))
       (format t "~%ito: ~a~%" ,desc)
       (force-output t)
       ,@body)))

(defmacro intend-skip (desc form);; not yet work
    `(report-result ,desc :skip nil ',form nil)
  )

(defmacro intend-t (desc form)
    `(let (val)
       (setf val ,form)  ;;; eval form just once
       (report-result ,desc val val ',form val)
     )
  )

(defmacro intend-f (desc form)
    `(let (val)
       (setf val ,form)  ;;; eval form just once
       (report-result ,desc (null val) NIL ',form val)
     )
  )

(defmacro intend-eq (desc expv form)
    `(let (val)
       (setf val ,form)  ;;; eval form just once
       (report-result ,desc (eq ,expv val) ,expv ',form val)
     )
  )

(defmacro intend-neq (desc expv form)
    `(let (val)
       (setf val ,form)  ;;; eval form just once
       (report-result ,desc (not (eq ,expv val)) ,expv ',form val)
     )
  )

(defmacro intend-equal (desc expv form)
    `(let (val)
       (setf val ,form)  ;;; eval form just once
       (report-result ,desc (equal ,expv val) ,expv ',form val)
     )
  )

(defmacro intend-notequal (desc expv form)
    `(let (val)
       (setf val ,form)  ;;; eval form just once
       (report-result ,desc (not (equal ,expv val)) ,expv ',form val)
     )
  )

(defmacro intend-multiple-equal (desc exps form)
  `(intend-equal ,desc ',exps (multiple-value-list ,form))
)

(defmacro intend-multiple-notequal (desc exps form)
  `(intend-notequal ,desc ',exps (multiple-value-list ,form))
)

(defmacro intend->(desc form1 form2)
    `(let (val1 val2)
       (setf val1 ,form1)  ;;; eval form just once
       (setf val2 ,form2)  ;;; eval form just once
       (report-result ,desc (> val1 val2) T '(> ,form1 ,form2) T)
     )
  )

(defmacro intend->=(desc form1 form2)
    `(let (val1 val2)
       (setf val1 ,form1)  ;;; eval form just once
       (setf val2 ,form2)  ;;; eval form just once
       (report-result ,desc (>= val1 val2) T '(>= ,form1 ,form2) T)
     )
  )

(defmacro intend-<(desc form1 form2)
    `(let (val1 val2)
       (setf val1 ,form1)  ;;; eval form just once
       (setf val2 ,form2)  ;;; eval form just once
       (report-result ,desc (< val1 val2) T '(> ,form1 ,form2) T)
     )
  )

(defmacro intend-<=(desc form1 form2)
    `(let (val1 val2)
       (setf val1 ,form1)  ;;; eval form just once
       (setf val2 ,form2)  ;;; eval form just once
       (report-result ,desc (<= val1 val2) T '(<= ,form1 ,form2) T)
     )
  )

(defmacro intend-=(desc form1 form2)
    `(let (val1 val2)
       (setf val1 ,form1)  ;;; eval form just once
       (setf val2 ,form2)  ;;; eval form just once
       (report-result ,desc (= val1 val2) T '(= ,form1 ,form2) T)
     )
  )

(defmacro ito-set (desc &body forms)
  (let  ((result (gensym)))
  	`(let ((,result t))
	   ,(format t "~%itocase: ~a ~%" desc)
           ,(force-output t)
       	   ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
	   ,result)
	)
  )

(defun report-result (desc result expv form value)
  (if  result 
    (progn (format t ".")(force-output t))
    (progn
      (format t "~% ~a: Ito Failed at ~a" desc *ito-name*)
      (format t "~%   Expression: ~a" form)
      (format t "~%    Intented ~a" expv)
      (format t "~%    Evaluated: ~a~%" value)
      (force-output t)
    )
  )
  result
)


