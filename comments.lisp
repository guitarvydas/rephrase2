(in-package :rephrase)

;; comments to end of line, begin with %

(defclass comments (rephrase)
  ((state :accessor state)))

(defmethod reset ((self comments))
  (setf (state self) :idle))

(defmethod filter ((self comments) token-list)
  (flet ((comment-start? (tok)
	   (and (eq :character (token-kind tok))
		(char= #\% (token-text tok))))
	 (eol? (tok)
	   (and (eq :character (token-kind tok))
		(char= #\Newline (token-text tok)))))
    (let ((output nil))
      (@:loop
	(@:exit-when (null token-list))
	(let ((tok (pop token-list)))
	  (ecase (state self)
	    (:idle
	     (cond ((eq :EOF (token-kind tok))
		    (push tok output)
		    (assert (null token-list)))
		   ((comment-start? tok)
		    (setf (state self) :ignoring))
		   (t (push tok output))))
	    (:ignoring
	     (cond ((eq :EOF (token-kind tok))
		    (push tok output)
		    (assert (null token-list)))
		   ((eol? tok)
		    (push tok output)
		    (setf (state self) :idle))
		   (t )))
	    )))
      output)))
