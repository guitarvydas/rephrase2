(in-package :rephrase)

(defmethod initially ((self parser) token-stream)
  (setf (token-stream self) token-stream)
  (reset self))

(defmethod reset ((self parser))
  (setf (next-token self) (pop (token-stream self))))

(defmethod read-next-token ((p parser) &optional (debug t))
  (unless (eq :eof (token-kind (accepted-token p)))
    (setf (next-token p) (pop (token-stream p)))
    (when debug
      (cond ((eq :eof (token-kind (accepted-token p)))
	     (format *standard-output* "		next-token: no more tokens~%"))
            (t 
	     (format *standard-output* "		next-token ~s ~s ~a ~a~%" 
		     (token-kind (next-token p)) (token-text (next-token p))
		     (token-line (next-token p)) (token-position (next-token p))))))))

(defmethod parser-err ((p parser) kind text)
  (let ((nt (next-token p)))
    (let ((error-message
	   (if kind
               (format nil "~&parser error in ~s - wanted ~s ~s, but got ~s ~s at line ~a position ~a~%" (current-rule p)
                       kind text (token-kind nt) (token-text nt) (token-line nt) (token-position nt))
	       (format nil "~&parser error in ~s - got ~s ~s at line ~a position ~a~%" (current-rule p)
		       (token-kind nt) (token-text nt) (token-line nt) (token-position nt)))))
    (error error-message)
    ;(read-next-token p)
    :fail)))

(defmethod accept ((p parser))
  (setf (accepted-token p) (next-token p))
  (format *standard-output* "~&~s" (token-text (accepted-token p)))
  (read-next-token p)
  :ok)

(defmethod input ((p parser) kind)
  (if (eq kind (token-kind (next-token p)))
      (accept p)
    (parser-err p kind "")))

(defmethod input-symbol ((p parser) text)
  (if (and (eq :symbol (token-kind (next-token p)))
           (string= text (token-text (next-token p))))
      (accept p)
    (parser-err p :symbol text)))

(defmethod input-upcase-symbol ((p parser))
  (if (and (eq :symbol (token-kind (next-token p)))
           (all-upcase-p (token-text (next-token p))))
        (accept p)
    (parser-err p 'upcase-symbol (token-text (next-token p)))))

(defmethod input-char ((p parser) char)
  (if (and (eq :character (token-kind (next-token p)))
           (char= (token-text (next-token p)) char))
      (accept p)
    (parser-err p :character char)))

(defmethod look-upcase-symbol? ((p parser))
  (if (and (eq :symbol (token-kind (next-token p)))
           (all-upcase-p (token-text (next-token p))))
      :ok
    :fail))

(defmethod look-char? ((p parser) c)
  (if (and (eq :character (token-kind (next-token p)))
           (char= (token-text (next-token p)) c))
      :ok
    :fail))

(defmethod look? ((p parser) kind)
  (if (eq kind (token-kind (next-token p)))
      :ok
    :fail))

(defmethod look-symbol? ((p parser) text)
  (if (and (eq :symbol (token-kind (next-token p)))
           (string= text (token-text (next-token p))))
      :ok
    :fail))

(defun all-upcase-p (s)
  (dotimes (i (length s))
    (unless (upper-case-p (char s i))
      (return-from all-upcase-p nil)))
  t)


;;  code emission mechanisms

(defmethod emit ((p parser) fmtstr &rest args)
  (let ((str (apply #'format nil fmtstr args)))
    (write-string str (output-stream p))))

