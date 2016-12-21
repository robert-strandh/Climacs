;;; -*- Mode: Lisp -*-

;;;  (c) copyright 2005 by
;;;           Brian Mastenbrook (brian@mastenbrook.net)
;;;           Christophe Rhodes (c.rhodes@gold.ac.uk)
;;;           Robert Strandh (strandh@labri.fr)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(defpackage :climacs-ttcn3-syntax
  (:use :clim-lisp :clim :clim-extensions :drei-buffer :drei-base 
	:drei-syntax :drei :drei-fundamental-syntax)
  (:export))
(in-package :climacs-ttcn3-syntax)

(defgeneric display-parse-tree (parse-symbol pane drei syntax))

(defclass ttcn3-parse-tree (drei-syntax:parse-tree) ())

(defclass ttcn3-entry (ttcn3-parse-tree)
  ((ink) (face)
  (state :initarg :state)))

(defclass ttcn3-nonterminal (ttcn3-entry) ())

(defclass ttcn3-terminal (ttcn3-entry)
  ((item :initarg :item)))

(defclass ttcn3-lexeme (ttcn3-entry) ())

(defgeneric lexeme-string (foo))

(defmethod lexeme-string ((thing ttcn3-entry))
  (coerce
   (drei-buffer:buffer-sequence (buffer thing)
                    (drei-syntax:start-offset thing)
                    (drei-syntax:end-offset thing))
   'string))

(defmethod print-object ((o ttcn3-lexeme) s)
  (print-unreadable-object (o s :type t)
    (format s "~S" (lexeme-string o))))

(defmacro define-lexemes (superclass &body lexemes)
  `(progn
     ,@(loop for lexeme in lexemes
	    collect `(defclass ,lexeme (,superclass) ()))))

(define-lexemes ttcn3-lexeme
  start-lexeme
  list-open list-close
  block-open block-close
  alternative-open alternative-close
  to-symbol
  line-comment block-comment
  line-or-statement-terminator-symbol
  plus minus divide concatenation
  not-equal equals greater-than less-than
  double-quote single-quote question star
  assignment communication identifier number-form
  dot comma
  other-entry)

(defclass ttcn3-lexer (drei-syntax:incremental-lexer) ())

(defun identifier-char-p (var &key start)
  (and (characterp var)
       (if start (alpha-char-p var) t)
       (or (alphanumericp var) (eql var #\_))))

(defmethod drei-syntax:next-lexeme ((lexer ttcn3-lexer) scan)
  (flet ((fo () (drei-buffer:forward-object scan)))
    (let ((object (drei-buffer:object-after scan)))
      (macrolet ((dispatch-object (&body cases)
		   `(case object
		      ,@(loop for case in cases
			     collect `(,(first case)
					(fo)
					,@(if (and (eql (length case) 2)
						  (symbolp (second case)))
					     `((make-instance ',(second case)))
					     (cdr case)))))))
	(dispatch-object
	 (#\( list-open)
	 (#\) list-close)
	 (#\{ block-open)
	 (#\} block-close)
	 (#\; line-or-statement-terminator-symbol)
	 (#\. dot)
	 (#\, comma)
	 (#\: (if (and (not (drei-buffer:end-of-buffer-p scan))
		       (eql (drei-buffer:object-after scan) #\=))
		  (progn (fo) (make-instance 'assignment))
		  (make-instance 'other-entry)))
	 (t
	  (cond
	    ((digit-char-p object)
	     (loop until (drei-buffer:end-of-buffer-p scan)
		while (digit-char-p (drei-buffer:object-after scan))
		do (fo))
	     (make-instance 'number-form))
	    ((identifier-char-p object :start t)
	     (loop until (drei-buffer:end-of-buffer-p scan)
		while (identifier-char-p (drei-buffer:object-after scan))
		do (fo))
	     (make-instance 'identifier))
	    (t (fo) (make-instance 'other-entry)))))))))

(drei-syntax:define-syntax ttcn3-syntax (fundamental-syntax)
  ((lexer :reader lexer)
   (valid-parse :initform 1)
   (parser))
  (:name "TTCN3")
  (:pathname-types "ttcn" "ttcn3"))

(defparameter *ttcn3-grammar* (drei-syntax:grammar))

(defmethod initialize-instance :after ((syntax ttcn3-syntax) &rest args)
  (declare (ignore args))
  (with-slots (parser lexer buffer) syntax
    (setf parser (make-instance 'drei-syntax:parser
				:grammar *ttcn3-grammar*
				:target 'ttcn3-terminals))
    (setf lexer (make-instance 'ttcn3-lexer :buffer (buffer syntax)))
    (let ((m (drei-buffer:clone-mark (low-mark buffer) :left))
	   (lexeme (make-instance 'start-lexeme :state (drei-syntax:initial-state parser))))
      (setf (drei-buffer:offset m) 0)
      (setf (drei-syntax:start-offset lexeme) m
	    (drei-syntax:end-offset lexeme) 0)
      (drei-syntax:insert-lexeme lexer 0 lexeme))))

(defmacro define-list (name empty-name nonempty-name item-name)
  `(progn
     (defclass ,name (ttcn3-entry) ())
     (defclass ,empty-name (,name) ())
     
     (defclass ,nonempty-name (,name)
       ((items :initarg :items)
	(item :initarg :item)))
     
     (drei-syntax:add-rule (drei-syntax:grammar-rule (,name -> () (make-instance ',empty-name))) *ttcn3-grammar*)
     
     (drei-syntax:add-rule (drei-syntax:grammar-rule
		(,name -> (,name ,item-name)
		       (make-instance ',nonempty-name
				      :items ,name :item ,item-name))) *ttcn3-grammar*)

     (defmethod display-parse-tree ((entity ,empty-name) (pane clim-stream-pane)
                                    (drei drei) (syntax ttcn3-syntax))
       (declare (ignore pane))
       nil)
     
     (defmethod display-parse-tree ((entity ,nonempty-name) (pane clim-stream-pane)
                                    (drei drei) (syntax ttcn3-syntax))
       (with-slots (items item) entity
	  (display-parse-tree items drei pane syntax)
	  (display-parse-tree item drei pane syntax)))))

(defmacro define-simple-list (name item-name)
  (let ((empty-name (gensym))
	(nonempty-name (gensym)))
    `(define-list ,name ,empty-name ,nonempty-name ,item-name)))

(defmacro define-simple-nonempty-list (nonempty-name item-name)
  (let ((empty-name (gensym))
	(name (gensym)))
    `(define-list ,name ,empty-name ,nonempty-name ,item-name)))

(defgeneric word-is (word string))

(defmethod word-is (word string)
  (string-equal (coerce (drei-buffer:buffer-sequence (buffer word) (drei-syntax:start-offset word) (drei-syntax:end-offset word)) 'string)
		string))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun sort-definitions (forms)
    (loop for form in forms
	  for name = (and (consp form) (car form))
	  if (eq name 'defclass)
	  collect form into defclasses
	  else if (eq name 'define-simple-list)
	  collect form into simple-lists
	  else if (eq name 'define-simple-nonempty-list)
	  collect form into nonempty-lists
	  else collect form into others
	  end
	  finally (return `(,@defclasses
			    ,@simple-lists
			    ,@nonempty-lists
			    ,@others)))))

(defmacro define-parsing-rules ((grammar entry terminal syntax) &body rules)
  (let (already-processed-rules)
    (flet
	((process-rule (name rule-body start-p)
	   (assert (not (member name already-processed-rules)))
	   (push name already-processed-rules)
	   (cond
	     ((and (eql (length rule-body) 1)
		   (typep (first rule-body) 'string))
	      `((defclass ,name (,entry) ((word :initarg :word)))
		(drei-syntax:add-rule (drei-syntax:grammar-rule (,name -> ((word identifier (word-is word ,(first rule-body)))) :word word))
			  ,grammar)
		,@(if start-p `((drei-syntax:add-rule (drei-syntax:grammar-rule (,terminal -> (,name) :item ,name)) ,grammar)))
		(defmethod display-parse-tree :around ((entity ,name) (pane clim-stream-pane)
                                                       (drei drei) (syntax ,syntax))
		  (with-drawing-options (pane :ink +blue-violet+)
		    (call-next-method)))))
	     ((and (eql (length rule-body) 1)
		   (typep (first rule-body) 'cons)
		   (eq (first (first rule-body)) 'or))
	      `((defclass ,name (,entry) ((item :initarg :item)))
		,@(loop for alt in (cdr (first rule-body))
		     collect `(drei-syntax:add-rule (drei-syntax:grammar-rule (,name -> ((item ,alt)) :item item)) ,grammar))
		,@(if start-p `((drei-syntax:add-rule (drei-syntax:grammar-rule (,terminal -> (,name) :item ,name)) ,grammar)))
		(defmethod display-parse-tree ((entity ,name) (pane clim-stream-pane)
                                               (drei drei) (syntax ,syntax))
		  (display-parse-tree (slot-value entity 'item) pane drei syntax))))
	     ((and (eql (length rule-body) 1)
		   (typep (first rule-body) 'cons)
		   (eq (first (first rule-body)) 'nonempty-list-of))
	      `((define-simple-nonempty-list ,name ,(second (first rule-body)))
		,@(if start-p `((drei-syntax:add-rule (drei-syntax:grammar-rule (,terminal -> (,name) :item ,name)) ,grammar)))))
	     ((and (eql (length rule-body) 1)
		   (typep (first rule-body) 'cons)
		   (eq (first (first rule-body)) 'list-of))
	      `((define-simple-list ,name ,(second (first rule-body)))
		,@(if start-p `((drei-syntax:add-rule (drei-syntax:grammar-rule (,terminal -> (,name) :item ,name)) ,grammar)))))
	     ((every #'symbolp rule-body)
	      `((defclass ,name (,entry)
		  (,@(loop for component in rule-body
			collect `(,component :initarg ,(intern (symbol-name component) :keyword)))))
		(drei-syntax:add-rule
		 (drei-syntax:grammar-rule (,name ->
				      (,@(loop for component in rule-body
					    collect `(,component ,component)))
				      ,@(loop for component in rule-body
					   appending `(,(intern (symbol-name component) :keyword)
							,component)))) ,grammar)
		,@(if start-p `((drei-syntax:add-rule (drei-syntax:grammar-rule (,terminal -> (,name) :item ,name)) ,grammar)))
		(defmethod display-parse-tree ((entity ,name) (pane clim-stream-pane)
                                               (drei drei) (syntax ,syntax))
		  (with-slots ,rule-body
		      entity
		    ,@(loop for component in rule-body collect
			   `(display-parse-tree ,component pane drei syntax))))))
	     (t (error "Unrecognized rule body ~S for rule ~S~%" rule-body
		       name)))))
      `(progn
	 ,@(sort-definitions
	    (loop for rule in rules
	       appending (destructuring-bind (=-thingy rule-name &body rule-body)
			     rule
			   (assert (member =-thingy '(:= :==)))
			   (process-rule rule-name rule-body (eq =-thingy :==)))))))))

(define-list ttcn3-terminals empty-ttcn3-terminals
  nonempty-ttcn3-terminals ttcn3-terminal)

(define-parsing-rules (*ttcn3-grammar* ttcn3-entry ttcn3-terminal ttcn3-syntax)
  (:== ttcn3-module
       ttcn3-module-keyword ttcn3-module-id
       block-open opt-module-definitions-part
					; opt-module-control-part
       block-close			; opt-with-statement
       opt-semicolon)
  (:= opt-module-definitions-part
      (or module-definitions-part empty-ttcn3-terminals))
  (:= opt-semicolon
      (or line-or-statement-terminator-symbol empty-ttcn3-terminals))
  (:= ttcn3-module-keyword "module")
  (:= ttcn3-module-id module-identifier opt-definitive-identifier)
  (:= opt-definitive-identifier
      (or definitive-identifier empty-ttcn3-terminals))
  (:= module-identifier identifier)
  (:= object-identifier-keyword "objid")
  (:= definitive-obj-id-component-list
      (nonempty-list-of definitive-obj-id-component))
  (:= definitive-obj-id-component
      (or identifier definitive-number-form definitive-name-and-number-form))
  (:= definitive-identifier dot object-identifier-keyword block-open definitive-obj-id-component-list block-close)
  (:= definitive-number-form number-form)
  (:= definitive-name-and-number-form identifier list-open definitive-number-form list-close)
  (:= module-definitions-list (nonempty-list-of module-definition-and-optional-semicolon))
  (:= module-definitions-part module-definitions-list)
  (:= module-definition-and-optional-semicolon
      module-definition opt-semicolon)
  (:= module-definition
      (or ; type-def
					const-def
					; template-def
					; module-par-def
					; function-def
					; signature-def
					; testcase-def
					; altstep-def
					; import-def
					; group-def
					; ext-function-def
					; ext-const-def
       ))
  (:= const-def const-keyword ttcn3-type const-list)
  (:= const-list single-const-def comma-sep-single-const-defs)
  (:= comma-sep-single-const-defs (list-of comma-and-single-const-def))
  (:= comma-and-single-const-def comma single-const-def)
  (:= single-const-def const-identifier ; opt-array-def
      assignment constant-expression)
  (:= const-keyword "const")
  (:= const-identifier identifier)
  (:= ttcn3-type #+(or) (or predefined-type referenced-type)
      identifier)
  (:= constant-expression
      (or identifier number-form)))
      

(defmethod display-parse-tree ((entity ttcn3-terminal) (pane clim-stream-pane)
                               (drei drei) (syntax ttcn3-syntax))
  (with-slots (item) entity
      (display-parse-tree item pane drei syntax)))

(defmethod display-parse-tree ((entity ttcn3-entry) (pane clim-stream-pane)
                               (drei drei) (syntax ttcn3-syntax))
  (flet ((cache-test (t1 t2)
	   (and (eq t1 t2)
		(eq (slot-value t1 'ink)
		    (medium-ink (sheet-medium pane)))
		(eq (slot-value t1 'face)
		    (text-style-face (medium-text-style (sheet-medium pane)))))))
    (updating-output (pane :unique-id entity
			   :id-test #'eq
			   :cache-value entity
			   :cache-test #'cache-test)
		     (with-slots (ink face) entity
		       (setf ink (medium-ink (sheet-medium pane))
			     face (text-style-face (medium-text-style (sheet-medium pane))))
		       (present (coerce (drei-buffer:buffer-sequence (buffer syntax)
							 (drei-syntax:start-offset entity)
							 (drei-syntax:end-offset entity))
					'string)
				'string
				:stream pane)))))

(defgeneric display-parse-stack (symbol stack pane drei syntax))

(defmethod display-parse-stack (symbol stack (pane clim-stream-pane)
                               (drei drei) (syntax ttcn3-syntax))
  (let ((next (drei-syntax:parse-stack-next stack)))
    (unless (null next)
      (display-parse-stack (drei-syntax:parse-stack-symbol next) next pane drei syntax))
    (loop for parse-tree in (reverse (drei-syntax:parse-stack-parse-trees stack))
       do (display-parse-tree parse-tree pane drei syntax))))

(defun display-parse-state (state pane drei syntax)
  (let ((top (drei-syntax:parse-stack-top state)))
    (if (not (null top))
	(display-parse-stack (drei-syntax:parse-stack-symbol top) top pane drei syntax)
	(display-parse-tree (drei-syntax:target-parse-tree state) pane drei syntax))))

(defmethod update-syntax-for-display (buffer (syntax ttcn3-syntax) top bot)
  (with-slots (parser lexer valid-parse) syntax
    (loop until (= valid-parse (drei-syntax:nb-lexemes lexer))
       while (drei-buffer:mark<= (drei-syntax:end-offset (drei-syntax:lexeme lexer valid-parse)) bot)
       do (let ((current-token (drei-syntax:lexeme lexer (1- valid-parse)))
		(next-lexeme (drei-syntax:lexeme lexer valid-parse)))
	    (setf (slot-value next-lexeme 'state)
		  (drei-syntax:advance-parse parser (list next-lexeme) (slot-value current-token 'state))))
	 (incf valid-parse))))

(defmethod drei-syntax:inter-lexeme-object-p ((lexer ttcn3-lexer) object)
  (drei-syntax:whitespacep (drei-syntax:syntax (buffer lexer)) object))

(defmethod drei-syntax:update-syntax (buffer (syntax ttcn3-syntax))
  (with-slots (lexer valid-parse) syntax
    (let* ((low-mark (low-mark buffer))
	   (high-mark (high-mark buffer)))
       (when (drei-buffer:mark<= low-mark high-mark)
	 (let ((first-invalid-position (drei-syntax:delete-invalid-lexemes lexer low-mark high-mark)))
	   (setf valid-parse first-invalid-position)
	   (drei-syntax:update-lex lexer first-invalid-position high-mark))))))

(defvar *white-space-start* nil)

(defvar *cursor-positions* nil)
(defvar *current-line* 0)

(defun handle-whitespace (pane buffer start end)
  (let ((space-width (space-width pane))
        (tab-width (tab-width pane)))
    (with-sheet-medium (medium pane)
      (with-accessors ((cursor-positions cursor-positions)) (drei-syntax:syntax buffer)
        (loop while (< start end)
           do (case (drei-buffer:buffer-object buffer start)
                (#\Newline (record-line-vertical-offset pane (drei-syntax:syntax buffer) (incf *current-line*))
                           (terpri pane)
                           (stream-increment-cursor-position
                            pane (first (aref cursor-positions 0)) 0))
                ((#\Page #\Return #\Space) (stream-increment-cursor-position
                                            pane space-width 0))
                (#\Tab (let ((x (stream-cursor-position pane)))
                         (stream-increment-cursor-position
                          pane (- tab-width (mod x tab-width)) 0))))
           (incf start))))))

(defmethod display-parse-tree :before ((entity ttcn3-entry) (pane clim-stream-pane)
                               (drei drei) (syntax ttcn3-syntax))
  (handle-whitespace pane (buffer pane) *white-space-start* (drei-syntax:start-offset entity))
  (setf *white-space-start* (drei-syntax:end-offset entity)))

(defmethod display-parse-tree :around ((entity ttcn3-parse-tree) pane drei syntax)
  (with-slots (top bot) pane
    (when (and (drei-syntax:end-offset entity) (drei-buffer:mark> (drei-syntax:end-offset entity) top))
      (call-next-method))))

(defmethod display-drei-contents ((pane clim-stream-pane) (drei drei) (syntax ttcn3-syntax))
  (with-slots (top bot) pane
    (with-accessors ((cursor-positions cursor-positions)) syntax
      (setf cursor-positions (make-array (1+ (number-of-lines-in-region top bot))
                                         :initial-element nil)
            *current-line* 0
            (aref cursor-positions 0) (multiple-value-list
                                       (stream-cursor-position pane))))
    (with-slots (lexer) syntax
      (let ((average-token-size (max (float (/ (drei-buffer:size (buffer pane)) (drei-syntax:nb-lexemes lexer)))
				     1.0)))
	;; find the last token before bot
	(let ((end-token-index (max (floor (/ (drei-buffer:offset bot) average-token-size)) 1)))
	  ;; go back to a token before bot
	  (loop until (drei-buffer:mark<= (drei-syntax:end-offset (drei-syntax:lexeme lexer (1- end-token-index))) bot)
	     do (decf end-token-index))
	  ;; go forward to the last token before bot
	  (loop until (or (= end-token-index (drei-syntax:nb-lexemes lexer))
			  (drei-buffer:mark> (drei-syntax:start-offset (drei-syntax:lexeme lexer end-token-index)) bot))
	     do (incf end-token-index))
	  (let ((start-token-index end-token-index))
	    ;; go back to the first token after top, or until the previous token
	    ;; contains a valid parser state
	    (loop until (or (drei-buffer:mark<= (drei-syntax:end-offset (drei-syntax:lexeme lexer (1- start-token-index))) top)
			    (not (drei-syntax:parse-state-empty-p 
				  (slot-value (drei-syntax:lexeme lexer (1- start-token-index)) 'state))))
               do (decf start-token-index))
	    (let ((*white-space-start* (drei-buffer:offset top)))
	      ;; display the parse tree if any
	      (unless (drei-syntax:parse-state-empty-p (slot-value (drei-syntax:lexeme lexer (1- start-token-index)) 'state))
		(display-parse-state (slot-value (drei-syntax:lexeme lexer (1- start-token-index)) 'state)
                                     pane drei syntax))
	      ;; display the lexemes
	      (with-drawing-options (pane :ink (make-rgb-color 0.7 0.7 0.7))
		(loop while (< start-token-index end-token-index)
		   do (let ((token (drei-syntax:lexeme lexer start-token-index)))
			(display-parse-tree token pane drei syntax))
                   (incf start-token-index))))))))))
