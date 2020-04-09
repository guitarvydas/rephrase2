(in-package :rephrase)

(defstruct token
  kind
  text
  position
  line)

(defgeneric exec-with-filename (self filename))
(defgeneric filter (self token-list))

(defclass parser ()
  ((token-stream :accessor token-stream :initarg :token-stream :initform nil) ;; actually, just a list
   (output-stream :accessor output-stream :initarg :output-stream :initform (make-string-output-stream))
   (next-token :accessor next-token :initform nil)
   (accepted-token :accessor accepted-token :initform nil)
   (state :accessor state :initform :idle)
   (current-rule :accessor current-rule :initform nil)
   (depth :accessor depth :initform 0)
   ))

(defclass rephrase (parser) () )


;; a parser must support:

(defmethod input-char ((self parser) c)
  )

(defmethod input ((self parser) tok)
  )

