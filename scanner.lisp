(in-package :rephrase)

(defun scanner (filename)
  (let ((tokenizer (make-instance 'tokenizer))
	(comments  (make-instance 'comments))
	(spaces    (make-instance 'spaces))
	(strings   (make-instance 'strings))
	(symbols   (make-instance 'symbols))
	(integers  (make-instance 'integers)))
    #+nil(declare (ignore spaces strings symbols integers))
    (reset tokenizer)
    (reset comments)
    (reset spaces)
    (reset strings)
    (reset symbols)
    (reset integers)
    (let ((r (exec-with-filename tokenizer filename)))
      (let ((r (filter comments r)))
	(let ((r (filter strings r)))
	  (let ((r (filter spaces r)))
	    (let ((r (filter symbols r)))
	      (let ((r (filter integers r)))
		r))))))))

    
