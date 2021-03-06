;;; -*- Mode: Lisp; Package: CLIMACS-CORE -*-

;;;  (c) copyright 2004-2005 by
;;;           Robert Strandh (robert.strandh@gmail.com)
;;;  (c) copyright 2004-2005 by
;;;           Elliott Johnson (ejohnson@fasl.info)
;;;  (c) copyright 2005 by
;;;           Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;  (c) copyright 2005 by
;;;           Aleksandar Bakic (a_bakic@yahoo.com)
;;;  (c) copyright 2006 by
;;;           Troels Henriksen (athas@sigkill.dk)

(cl:in-package #:climacs-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Buffer handling

(define-presentation-method present ((object drei-view) (type view)
                                     stream (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (princ (esa-utils:subscripted-name object) stream))

(define-presentation-method accept ((type view) stream (view textual-view)
                                    &key (default nil defaultp)
                                    (default-type type))
  (multiple-value-bind (object success string)
      (complete-input stream
		      (lambda (so-far action)
			(complete-from-possibilities
			 so-far (views esa:*esa-instance*) '()
                         :action action
			 :name-key #'esa-utils:subscripted-name
			 :value-key #'identity))
		      :partial-completers '(#\Space)
		      :allow-any-input t)
    (cond ((and success (plusp (length string)))
           (if object
               (values object type)
               (values string 'string)))
	  ((and (zerop (length string)) defaultp)
           (values default default-type))
	  (t
           (values string 'string)))))

(defgeneric switch-to-view (drei view)
  (:documentation "High-level function for changing the view
displayed by a Drei instance."))

(defmethod switch-to-view (pane (name string))
  (let ((view (find name (views (pane-frame pane))
               :key #'esa-utils:subscripted-name :test #'string=)))
    (switch-to-view
     pane (or view (make-new-view-for-climacs
                    (pane-frame pane) 'textual-drei-syntax-view
                    :name name)))))

(defun switch-or-move-to-view (pane view)
  "Switch `pane' to show `view'. If `view' is already on display
in some other pane, switch that pane to be the active one."
  (handler-bind ((view-already-displayed
                  #'(lambda (c)
                      (declare (ignore c))
                      (invoke-restart 'switch-to-pane))))
    (switch-to-view pane view)))

(defun views-having-buffer (climacs buffer)
  "Return a list of the buffer-views of `climacs' showing
`buffer'."
  (loop for view in (views climacs)
     when (and (typep view 'drei-buffer-view)
               (eq (buffer view) buffer))
     collect view))

(defun buffer-of-view-needs-saving (view)
  "Return true if `view' is a `drei-buffer-view' and it needs to
be saved (that is, it is related to a file and it has changed
since it was last saved)."
  (and (typep view 'drei-buffer-view)
       (esa-buffer:filepath (buffer view))
       (esa-buffer:needs-saving (buffer view))))

(defun dummy-buffer ()
  "Create a dummy buffer object for use when killing views, to
prevent increasing memory usage."
  (make-instance 'drei-buffer))

(defgeneric kill-view (view)
  (:documentation "Remove `view' from the Climacs specified in
`*esa-instance*'. If `view' is currently displayed in a window,
it will be replaced by some other view."))

(defmethod kill-view ((view view))
  (with-accessors ((views views)) esa:*esa-instance*
    ;; It might be the case that this view is the only view remaining
    ;; of some particular buffer, in that case, the user might want to
    ;; save it.
    (when (and (buffer-of-view-needs-saving view)
               (= (length (views-having-buffer esa:*esa-instance* (buffer view)))
                  1)
               (handler-case (accept 'boolean :prompt "Save buffer first?")
                 (error () (progn (beep)
                                  (esa:display-message "Invalid answer")
                                  (return-from kill-view nil)))))
      (esa-io:save-buffer (buffer view)))
    (setf views (remove view views))
    ;; If we don't change the buffer of the view, a reference to the
    ;; view will be kept in the buffer, and the view will thus not be
    ;; garbage-collected. So create a circular reference structure
    ;; that can be garbage-collected instead.
    (when (buffer-view-p view)
      (setf (buffer view) (dummy-buffer)))
    (full-redisplay (esa:current-window))
    (current-view)))

(defmethod kill-view ((name string))
  (let ((view (find name (views *application-frame*)
                 :key #'esa-utils:subscripted-name :test #'string=)))
    (when view (kill-view view))))

(defmethod kill-view ((symbol null))
  (kill-view (current-view)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; File handling

(defun filepath-filename (pathname)
  (if (null (pathname-type pathname))
      (pathname-name pathname)
      (concatenate 'string (pathname-name pathname)
		   "." (pathname-type pathname))))

(defun syntax-class-name-for-filepath (filepath)
  (let ((syntax-description
         (find (or (pathname-type filepath)
                   (pathname-name filepath))
               drei-syntax::*syntaxes*
               :test (lambda (x y)
                       (member x y :test #'string-equal))
               :key #'drei-syntax::syntax-description-pathname-types)))
    (if syntax-description
        (drei-syntax::syntax-description-class-name
         syntax-description)
        drei-syntax:*default-syntax*)))

(defun evaluate-attributes (view options)
  "Evaluate the attributes `options' and modify `view' as
appropriate. `Options' should be an alist mapping option names to
their values."
  ;; First, check whether we need to change the syntax (via the SYNTAX
  ;; option). MODE is an alias for SYNTAX for compatibility with
  ;; Emacs. If there is more than one option with one of these names,
  ;; only the first will be acted upon.
  (let ((specified-syntax
         (drei-syntax:syntax-from-name
          (second (find-if #'(lambda (name)
                               (or (string-equal name "SYNTAX")
                                   (string-equal name "MODE")))
                           options
                           :key #'first)))))
    (when (and specified-syntax
               (not (eq (class-of (drei-syntax:syntax view))
                        specified-syntax)))
      (setf (drei-syntax:syntax view)
            (make-syntax-for-view view specified-syntax))))
  ;; Now we iterate through the options (discarding SYNTAX and MODE
  ;; options).
  (loop for (name value) in options
     unless (or (string-equal name "SYNTAX")
                (string-equal name "MODE"))
     do (drei-syntax:eval-option (drei-syntax:syntax view) name value)))

(defun split-attribute (string char)
  (let (pairs)
    (loop with start = 0
	  for ch across string
	  for i from 0
	  when (eql ch char)
	    do (push (string-trim '(#\Space #\Tab) (subseq string start i))
		     pairs)
	       (setf start (1+ i))
	  finally (unless (>= start i)
		    (push (string-trim '(#\Space #\Tab) (subseq string start))
			  pairs)))
    (nreverse pairs)))

(defun split-attribute-line (line)
  (when line
    (mapcar (lambda (pair) (split-attribute pair #\:))
	    (split-attribute line #\;))))

(defun find-attribute-line-position (buffer)
  (let ((scan (drei-buffer:make-buffer-mark buffer 0)))
    ;; skip the leading whitespace
    (loop until (drei-buffer:end-of-buffer-p scan)
       until (not (buffer-whitespacep (drei-buffer:object-after scan)))
       do (drei-buffer:forward-object scan))
    ;; stop looking if we're already 1,000 objects into the buffer
    (unless (> (drei-buffer:offset scan) 1000)
      (let ((start-found
	     (loop with newlines = 0
                when (drei-buffer:end-of-buffer-p scan)
                do (return nil)
                when (eql (drei-buffer:object-after scan) #\Newline)
                do (incf newlines)
                when (> newlines 1)
                do (return nil)
                until (looking-at scan "-*-")
                do (drei-buffer:forward-object scan)
                finally (return t))))
	(when start-found
          (let* ((end-scan (drei-buffer:clone-mark scan))
                 (end-found
                  (loop when (drei-buffer:end-of-buffer-p end-scan)
                     do (return nil)
                     when (eql (drei-buffer:object-after end-scan) #\Newline)
                     do (return nil)
                     do (drei-buffer:forward-object end-scan)
                     until (looking-at end-scan "-*-")
                     finally (return t))))
            (when end-found
              (values scan
                      (progn (drei-buffer:forward-object end-scan 3)
                             end-scan)))))))))

(defun get-attribute-line (buffer)
  (multiple-value-bind (start-mark end-mark)
      (find-attribute-line-position buffer)
   (when (and start-mark end-mark)
     (let ((line (drei-buffer:buffer-substring buffer
				   (drei-buffer:offset start-mark)
				   (drei-buffer:offset end-mark))))
       (when (>= (length line) 6)
	 (let ((end (search "-*-" line :from-end t :start2 3)))
	   (when end
	     (string-trim '(#\Space #\Tab) (subseq line 3 end)))))))))

(defun replace-attribute-line (view new-attribute-line)
  (let ((full-attribute-line (concatenate 'string
                                          "-*- "
                                          new-attribute-line
                                          "-*-")))
   (multiple-value-bind (start-mark end-mark)
       (find-attribute-line-position (buffer view))
     (cond ((not (null end-mark))
            ;; We have an existing attribute line.
            (drei-buffer:delete-region start-mark end-mark)
            (let ((new-line-start (drei-buffer:clone-mark start-mark :left)))
              (insert-sequence start-mark full-attribute-line)
              (drei-syntax:comment-region (drei-syntax:syntax view)
                              new-line-start
                              start-mark)))
           (t
            ;; Create a new attribute line at beginning of buffer.
            (let* ((mark1 (drei-buffer:make-buffer-mark (buffer view) 0 :left))
                   (mark2 (drei-buffer:clone-mark mark1 :right)))
              (insert-sequence mark2 full-attribute-line)
              (drei-buffer:insert-object mark2 #\Newline)
              (drei-syntax:comment-region (drei-syntax:syntax view)
                              mark1
                              mark2)))))))

(defun update-attribute-line (view)
  (replace-attribute-line
   view (drei-syntax:make-attribute-line (drei-syntax:syntax view))))

(defun evaluate-attribute-line (view)
  (evaluate-attributes
   view (split-attribute-line (get-attribute-line (buffer view)))))

;; Adapted from cl-fad/PCL
(defun directory-pathname-p (pathspec)
  "Returns NIL if PATHSPEC does not designate a directory."
  (let ((name (pathname-name pathspec))
	(type (pathname-type pathspec)))
    (and (or (null name) (eql name :unspecific))
	 (or (null type) (eql type :unspecific)))))

(defun findablep (pathname)
  "Return non-NIL if `pathname' can be opened by Climacs. That
  is, check whether the file exists and is not a directory."
  (and (probe-file pathname)
       (not (directory-pathname-p pathname))))

(defun find-view-with-pathname (pathname)
  "Return the (first) with associated with the file designated by
`pathname'. Returns NIL if no buffer can be found."
  (flet ((usable-pathname (pathname)
           (if (probe-file pathname)
               (truename pathname)
               pathname)))
    (find pathname (remove-if-not #'(lambda (view)
                                      (typep view 'drei-buffer-view))
                                  (views *application-frame*))
     :key #'(lambda (view) (esa-buffer:filepath (buffer view)))
     :test #'(lambda (fp1 fp2)
               (and fp1 fp2
                    (equal (usable-pathname fp1)
                           (usable-pathname fp2)))))))

(defun ensure-open-file (pathname)
  "Make sure a buffer opened on `pathname' exists, finding the
file if necessary."
  (when (and (findablep pathname)
             (not (find-buffer-with-pathname pathname)))
    (esa-io:find-file pathname)))

(defun find-file-impl (filepath &optional readonlyp)
  (cond ((null filepath)
	 (esa:display-message "No file name given.")
	 (beep))
	((directory-pathname-p filepath)
	 (esa:display-message "~A is a directory name." filepath)
	 (beep))
        (t
         (let ((existing-view (find-view-with-pathname filepath)))
           (if (and existing-view (if readonlyp (esa-buffer:read-only-p (buffer existing-view)) t))
               (switch-to-view (esa:current-window) existing-view)
               (let* ((newp (not (probe-file filepath)))
                      (buffer (if (and newp (not readonlyp))
                                  (esa-buffer:make-new-buffer)
                                  (with-open-file (stream filepath :direction :input)
                                    (make-buffer-from-stream stream))))
                      (view (make-new-view-for-climacs
                             esa:*esa-instance* 'textual-drei-syntax-view
                             :name (filepath-filename filepath)
                             :buffer buffer)))
                 (unless (buffer-pane-p (esa:current-window))
                   (other-window (or (find-if #'(lambda (window)
                                                  (typep window 'climacs-pane))
                                              (esa:windows esa:*esa-instance*))
                                     (split-window t))))
                 (setf (drei-buffer:offset (point buffer)) (drei-buffer:offset (point view))
                       (drei-syntax:syntax view) (make-syntax-for-view view (syntax-class-name-for-filepath filepath))
                       (esa-buffer:file-write-time buffer) (if newp (get-universal-time) (file-write-date filepath))
                       (esa-buffer:needs-saving buffer) nil
                       (esa-utils:name buffer) (filepath-filename filepath))
                 (setf (current-view (esa:current-window)) view)
                 (evaluate-attribute-line view)
                 (setf (esa-buffer:filepath buffer) (pathname filepath)
                       (esa-buffer:read-only-p buffer) readonlyp)
                 (drei-buffer:beginning-of-buffer (point view))
                 buffer))))))

(defun directory-of-buffer (buffer)
  "Extract the directory part of the filepath to the file in BUFFER.
If BUFFER does not have a filepath, the path to the user's home
directory will be returned."
  (make-pathname
   :directory
   (pathname-directory
    (or (esa-buffer:filepath buffer)
	(user-homedir-pathname)))))

(defun check-file-times (buffer filepath question answer)
  "Return NIL if filepath newer than buffer and user doesn't want
to overwrite."
  (let ((f-w-d (file-write-date filepath))
	(f-w-t (esa-buffer:file-write-time buffer)))
    (if (and f-w-d f-w-t (> f-w-d f-w-t))
	(if (accept 'boolean
		    :prompt (format nil "File has changed on disk. ~a anyway?"
				    question))
	    t
	    (progn (esa:display-message "~a not ~a" filepath answer)
		   nil))
	t)))
