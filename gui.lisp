;;; -*- Mode: Lisp; Package: CLIMACS-GUI -*-

;;;  (c) copyright 2004-2005 by
;;;           Robert Strandh (robert.strandh@gmail.com)
;;;  (c) copyright 2004-2005 by
;;;           Elliott Johnson (ejohnson@fasl.info)
;;;  (c) copyright 2005 by
;;;           Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;  (c) copyright 2005 by
;;;           Aleksandar Bakic (a_bakic@yahoo.com)
;;;  (c) copyright 2006-2008 by
;;;           Troels Henriksen (athas@sigkill.dk)

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

;;; GUI for the Climacs editor. 

(in-package #:climacs-gui)

(defvar *default-external-format* :utf-8
  "The encoding to use by default when reading and saving
files.")

(defvar *with-scrollbars* t
  "If T, classic look and feel. If NIL, stripped-down look (:")

(defvar *show-info-pane-mark-position* nil
  "If T, show the line number and column number in the info pane
  of all panes. If NIL, don't. This is off by default, as finding
  the line and column numbers is potentially expensive.")

(defvar *climacs-target-creator* nil
  "A function for creating targets for commands potentially
acting over multiple views.")

(defvar *climacs-text-style* (make-text-style :fix nil nil)
  "The default CLIM text style used in Climacs panes.")

(defclass climacs-buffer (drei-buffer)
  ((%external-format :initform *default-external-format*
                     :accessor external-format
                     :documentation "The external format that was
used when reading the source destination of the buffer
contents.")))

(defclass climacs-pane (drei-pane esa:esa-pane-mixin)
  ()
  (:metaclass modual-class)
  (:default-initargs
   :view (make-instance 'textual-drei-syntax-view
          :buffer (make-instance 'climacs-buffer))
   :display-time :command-loop
   :text-style *climacs-text-style*
   :width 900 :height 400))

(defmethod command-table ((pane climacs-pane))
  (command-table (pane-frame pane)))

(define-condition view-setting-error (error)
  ((%view :accessor view
          :initarg :view
          :initform (error "The view used in the error-causing operation must be supplied")
          :documentation "The view that is attempted set"))
  (:documentation "This error is signalled when something goes
wrong while setting the view of a Climacs pane."))

(define-condition unknown-view (view-setting-error)
  ()
  (:report (lambda (condition stream)
             (format
              stream "Attempting to set view of a window to view object ~A, which is not known to Climacs"
              (view condition))))
  (:documentation "This error is signalled whenever a window is
attempted to be set to a view that is not recognized by the
Climacs instance the window belongs to."))

(define-condition view-already-displayed (view-setting-error)
  ((%window :accessor window
            :initarg :window
            :initform (error "The window already displaying the view must be provided")
            :documentation "The window that already displays the view"))
  (:report (lambda (condition stream)
             (format
              stream "Attempting to set view of a window to view object ~A, which is already on display in another window"
              (view condition))))
  (:documentation "This error is signalled whenever a window is
attempted to be set to a view already on display in some other
window"))

(defmethod (setf view) :around ((view drei-view) (pane climacs-pane))
  (let ((window-displaying-view
         (find-if #'(lambda (other-pane)
                      (and (not (eq other-pane pane))
                           (typep other-pane 'climacs-pane)
                           (eq (view other-pane) view)))
                  (esa:windows (pane-frame pane))))
        (old-view-active (active (view pane))))
    (prog1
        (cond ((not (member view (views (pane-frame pane))))
               (restart-case (error 'unknown-view :view view)
                 (add-to-view-list ()
                  :report "Add the view object to Climacs"
                  (push view (views (pane-frame pane)))
                  (setf (view pane) view))))
              (window-displaying-view
               (restart-case
                   (error 'view-already-displayed :view view :window window-displaying-view)
                 (switch-to-pane ()
                  :report "Switch the active window to the one containing the view"
                  (other-window window-displaying-view)
                  view)
                 (remove-other-use ()
                  :report "Make the other window try to display some other view"
                  (setf (view window-displaying-view) (any-preferably-undisplayed-view))
                  (setf (view pane) view))
                 (remove-other-pane ()
                  :report "Remove the other window displaying the view"
                  (delete-window window-displaying-view)
                  (setf (view pane) view))
                 (clone-view ()
                  :report "Make a clone of the view and use that instead"
                  (setf (view pane) (clone-view-for-climacs
                                     (pane-frame window-displaying-view) view)))
                 (cancel ()
                  :report "Cancel the setting of the windows view and just return nil")))
              (t (call-next-method)))
      ;; Move view to the front of the view-list, doesn't carry
      ;; semantic significance, but makes view-switching more
      ;; convenient.
      (setf (views (pane-frame pane))
            (cons view (delete view (views (pane-frame pane)))))
      (when old-view-active
        (ensure-only-view-active (pane-frame pane) view)))))

(defmethod (setf view) :before ((view drei-view) (pane climacs-pane))
  (with-accessors ((views views)) (pane-frame pane)
    (full-redisplay pane)))

(defgeneric buffer-pane-p (pane)
  (:documentation "Returns T when a pane contains a buffer."))

(defmethod buffer-pane-p (pane)
  (declare (ignore pane))
  nil)

(defmethod buffer-pane-p ((pane climacs-pane))
  (typep (view pane) 'drei-buffer-view))

(defmethod in-focus-p ((pane climacs-pane))
  (eq pane (first (esa:windows *application-frame*))))

(defvar *info-bg-color* +gray85+)
(defvar *info-fg-color* +black+)
(defvar *mini-bg-color* +white+)
(defvar *mini-fg-color* +black+)

(defclass climacs-info-pane (esa:info-pane)
  ()
  (:default-initargs
      :height 20 :max-height 20 :min-height 20
      :display-function 'display-info
      :incremental-redisplay t
      :background *info-bg-color*
      :foreground *info-fg-color*
      :width 900))

(defclass climacs-minibuffer-pane (esa:minibuffer-pane)
  ()
  (:default-initargs
   :default-view +textual-view+
   :background *mini-bg-color*
   :foreground *mini-fg-color*
   :width 900))

;;; Basic command tables follow. The global command table,
;;; `global-climacs-table', inherits from these, so they should not
;;; contain any overly syntax-specific commands. The idea is that it
;;; should always be safe to invoke commands from these tables,
;;; without risking adding alien commands that require the current
;;; window to contain a specific type of view or syntax. In general,
;;; the Climacs frame has a special command table of type
;;; `climacs-command-table' (that's not its name) that selectively
;;; inherits from view-specific tables and the `global-climacs-table'
;;; based on the current window and view.

;;; Basic functionality
(make-command-table 'base-table :errorp nil)
;;; Buffers
(make-command-table 'buffer-table :errorp nil)
;;; Commands used for climacs development
(make-command-table 'development-table :errorp nil)
;;; Windows
(make-command-table 'window-table :errorp nil)

;;; customization of help.  FIXME: this might be better done by having
;;; the functions that the ESA commands call be customizeable generic
;;; functions; however, while they're not, scribbling over the ESA
;;; command tables is a bad thing.
(make-command-table 'climacs-help-table :inherit-from '(esa:help-table)
                    :errorp nil)

(make-command-table 'global-climacs-table
                    :errorp nil
                    :inherit-from '(base-table
                                    buffer-table
                                    window-table
                                    development-table
                                    climacs-help-table
                                    esa:global-esa-table
                                    esa-io:esa-io-table))

;; This command table is what assembles the various other command
;; tables for the commands actually accessible by the user.
(defclass climacs-command-table (standard-command-table)
  ())

(defmethod command-table-inherit-from ((table climacs-command-table))
  (append (view-command-tables (current-view))
          '(global-climacs-table)
          (when (use-editor-commands-p (current-view))
            '(editor-table))
          (call-next-method)))

;; This is the actual command table that will be used for Climacs.
(make-command-table 'climacs-global-table
 :inherit-from (list (make-instance 'climacs-command-table
                      :name 'climacs-dispatching-table))
 :menu `(("File" :menu esa-io:esa-io-menu-table)
         ("Macros" :menu esa:keyboard-macro-menu-table)
         ("Windows" :menu window-menu-table)
         ("Help" :menu esa:help-menu-table))
 :errorp nil)

(define-application-frame climacs (esa:esa-frame-mixin
				   standard-application-frame)
  ((%views :initform '() :accessor views)
   (%groups :initform (make-hash-table :test #'equal) :accessor groups)
   (%active-group :initform nil :accessor active-group)
   (%command-table :initform (find-command-table 'climacs-global-table)
                   :accessor esa:find-applicable-command-table
                   :accessor frame-command-table)
   (%output-stream :accessor output-stream
                   :initform nil
                   :initarg :output-stream))
  (:menu-bar climacs-global-table)
  (:panes
   (climacs-window
    (let* ((esa:*esa-instance* *application-frame*)
           (climacs-pane (make-pane 'climacs-pane :active t))
	   (info-pane (make-pane 'climacs-info-pane
                       :master-pane climacs-pane)))
      (unless (output-stream esa:*esa-instance*)
        (setf (output-stream esa:*esa-instance*)
              (make-typeout-stream *application-frame* "*standard-output*")))
      (setf (esa:windows *application-frame*) (list climacs-pane)
	    (views *application-frame*) (list (view climacs-pane)))
      (vertically ()
        (if *with-scrollbars*
            (scrolling ()
              climacs-pane)
            climacs-pane)
        info-pane)))
   (minibuffer (make-pane 'climacs-minibuffer-pane)))
  (:layouts
   (default
       (overlaying ()
         (vertically (:scroll-bars nil)
           climacs-window
           minibuffer))))
  (:top-level ((lambda (frame)
                 (with-frame-manager ((make-instance 'climacs-frame-manager))
                   (esa:esa-top-level frame :prompt "M-x "))))))

(esa:define-esa-top-level ((frame climacs) command-parser
			   command-unparser
			   partial-command-parser
			   prompt)
 :bindings ((*default-target-creator* *climacs-target-creator*)
            (esa:*previous-command* (esa:previous-command (drei-instance)))
            (*standard-output* (or (output-stream frame)
                                   *terminal-io*))))

(defmethod frame-standard-input ((frame climacs))
  (get-frame-pane frame 'minibuffer))

(defmethod esa:buffers ((climacs climacs))
  (remove-duplicates
   (mapcar #'esa-io:buffer (remove-if-not
			    #'(lambda (view)
				(typep view 'drei-buffer-view))
			    (views climacs)))))

(defmethod esa:esa-current-buffer ((application-frame climacs))
  (when (buffer-pane-p (esa:esa-current-window application-frame))
    (buffer (current-view (esa:esa-current-window application-frame)))))

(defmethod (setf esa:esa-current-buffer) ((new-buffer climacs-buffer)
					  (application-frame climacs))
  (setf (buffer (current-view (esa:esa-current-window application-frame)))
        new-buffer))

(defmethod drei-instance-of ((frame climacs))
  (esa:esa-current-window frame))

(defmethod (setf esa:windows) :after (new-val (climacs climacs))
  ;; Ensures that we don't end up with two views that both believe
  ;; they are active.
  (activate-window (esa:esa-current-window climacs)))

(defun current-window-p (window)
  "Return true if `window' is the current window of its Climacs
instance."
  (eq window (esa:esa-current-window (pane-frame window))))

(defun ensure-only-view-active (climacs &optional view)
  "Ensure that `view' is the only view of `climacs' that is
active. `View' may be NIL, in which case no view is set as
active."
  (dolist (other-view (views climacs))
    (unless (eq other-view view)
      (setf (active other-view) nil)))
  (unless (null view)
    (setf (active view) t)))

(defmethod (setf views) :around (new-value (frame climacs))
  ;; If any windows show a view that no longer exists in the
  ;; view-list, make them show something else. The view-list might be
  ;; destructively updated, so copy it for safekeeping. Also make sure
  ;; only one view thinks that it's active.
  (with-accessors ((views views)) frame
    (let* ((old-views (copy-list views))
           (removed-views (set-difference
                           old-views (call-next-method) :test #'eq)))
      (dolist (window (esa:windows frame))
        (when (and (typep window 'climacs-pane)
                   (member (view window) removed-views :test #'eq))
          (handler-case (setf (view window)
                              (any-preferably-undisplayed-view))
            (view-already-displayed ()
              (delete-window window)))))))
  (ensure-only-view-active
   frame (when (typep (esa:esa-current-window frame) 'climacs-pane)
           (view (esa:esa-current-window frame)))))

(defmethod (setf views) :after ((new-value null) (frame climacs))
  ;; You think you can remove all views? I laught at your silly
  ;; attempt!
  (setf (views frame) (list (make-new-view-for-climacs
                             frame 'textual-drei-syntax-view))))

(defmethod esa:command-for-unbound-gestures ((frame climacs) gestures)
  (esa:command-for-unbound-gestures (esa:esa-current-window frame) gestures))

(defun make-view-subscript-generator (climacs)
  #'(lambda (name)
      (1+ (reduce #'max (remove name (views climacs)
                         :test-not #'string= :key #'name)
           :initial-value 0
           :key #'subscript))))

(defun clone-view-for-climacs (climacs view &rest initargs)
  "Clone `view' and add it to `climacs's list of views."
  (let ((new-view (apply #'clone-view view
                   :subscript-generator (make-view-subscript-generator climacs)
                   :active nil initargs)))
    (setf (syntax new-view) (make-syntax-for-view new-view (class-of (syntax view))))
    (push new-view (views climacs))
    new-view))

(defun make-new-view-for-climacs (climacs view-class &rest initargs)
  "Instiantiate an object of type `view-class' and add it to
`climacs's list of views."
  (let ((new-view (apply #'make-instance view-class
                   :subscript-generator (make-view-subscript-generator climacs)
                   initargs)))
    (push new-view (views climacs))
    new-view))

(defun any-view ()
  "Return some view, any view."
  (first (views esa:*esa-instance*)))

(defun any-displayed-view ()
  "Return some view on display."
  (view (esa:esa-current-window *application-frame*)))

(defun view-on-display (climacs view)
  "Return true if `view' is on display in a window of `climacs',
false otherwise."
  (member view (remove-if-not #'(lambda (window)
                                  (typep window 'climacs-pane))
                              (esa:windows climacs))
   :key #'view))

(defun any-preferably-undisplayed-view ()
  "Return some view, any view, preferable one that is not
currently displayed in any window."
  (or (find-if-not #'(lambda (view)
                       (view-on-display esa:*esa-instance* view))
                   (views esa:*esa-instance*))
      (any-view)))

(defun any-undisplayed-view ()
  "Return some view, any view, as long as it is not currently
displayed in any window. If necessary, clone a view on display."
  (or (find-if-not #'(lambda (view)
                       (view-on-display esa:*esa-instance* view))
                   (views esa:*esa-instance*))
      (clone-view-for-climacs esa:*esa-instance* (any-view))))

(define-presentation-type read-only ())
(define-presentation-method highlight-presentation 
    ((type read-only) record stream state)
  nil)
(define-presentation-type modified ())
(define-presentation-method highlight-presentation 
    ((type modified) record stream state)
  nil)

(defgeneric display-view-info-to-info-pane (info-pane master-pane view)
  (:documentation "Display interesting information about
`view' (which is in `master-pane') to `info-pane'."))

(defgeneric display-view-status-to-info-pane (info-pane master-pane view)
  (:documentation "Display interesting information about the
status of `view' (which is in `master-pane') to `info-pane'. The
status should be things like whether it is modified, read-only,
etc."))

(defmethod display-view-info-to-info-pane ((info-pane climacs-info-pane)
                                           (master-pane climacs-pane)
                                           (view drei-syntax-view))
  (with-text-family (info-pane :sans-serif)
    (display-syntax-name (syntax view) info-pane :view view)))

(defmethod display-view-info-to-info-pane ((info-pane climacs-info-pane)
                                           (master-pane climacs-pane)
                                           (view textual-drei-syntax-view))
  (let ((point (point view))
        (bot (bot view))
        (top (top view))
        (size (size (esa-io:buffer view))))
    (format info-pane "  ~A  "
	    (cond ((and (mark= size bot)
			(mark= 0 top))
		   "")
		  ((mark= size bot)
		   "Bot")
		  ((mark= 0 top)
		   "Top")
		  (t (format nil "~a%"
			     (round (* 100 (/ (offset top)
					      size)))))))
    (when *show-info-pane-mark-position*
      (format info-pane "(~A,~A)     "
              (1+ (line-number point))
              (column-number point)))
    (princ #\( info-pane)
    (call-next-method)
    (format info-pane "~{~:[~*~; ~A~]~}" (list
                                          (overwrite-mode view)
                                          "Ovwrt"
                                          (auto-fill-mode view)
                                          "Fill"
                                          (isearch-mode master-pane)
                                          "Isearch"))
    (princ #\) info-pane)))

(defmethod display-view-info-to-info-pane ((info-pane climacs-info-pane)
                                           (master-pane climacs-pane)
                                           (view typeout-view)))

(defmethod display-view-status-to-info-pane ((info-pane climacs-info-pane)
                                             (master-pane climacs-pane)
                                             (view drei-syntax-view))
  (with-output-as-presentation (info-pane view 'read-only)
    (princ (cond
             ((read-only-p (esa-io:buffer view)) "%")
             ((needs-saving (esa-io:buffer view)) "*")
             (t "-"))
           info-pane))
  (with-output-as-presentation (info-pane view 'modified)
    (princ (cond
             ((needs-saving (esa-io:buffer view)) "*")
             ((read-only-p (esa-io:buffer view)) "%")
             (t "-"))
           info-pane))
  (princ "  " info-pane))

(defmethod display-view-status-to-info-pane ((info-pane climacs-info-pane)
                                             (master-pane climacs-pane)
                                             (view typeout-view)))

(defun display-info (frame pane)
  (let* ((master-pane (esa:master-pane pane))
	 (view (view master-pane)))
    (princ "   " pane)
    (display-view-status-to-info-pane pane master-pane view)
    (with-text-face (pane :bold)
      (with-output-as-presentation (pane view 'view)
        (format pane "~A" (subscripted-name view)))
      ;; FIXME: bare 25.
      (format pane "~V@T" (max (- 25 (length (subscripted-name view))) 1)))
    (display-view-info-to-info-pane pane master-pane view)
    (with-text-family (pane :sans-serif)
      (princ (if (esa:recordingp frame)
		 "Def"
		 "")
	     pane))))

(defmethod handle-drei-condition ((drei climacs-pane) condition)
  (call-next-method)
  (display-drei drei :redisplay-minibuffer t))

(defmethod execute-frame-command :around ((frame climacs) command)
  (if (eq frame esa:*esa-instance*)
      (handling-drei-conditions
        (with-undo ((esa:buffers frame))
          (call-next-method)))
      (call-next-method)))

(define-command (com-full-redisplay :name t :command-table base-table) ()
  "Redisplay the contents of the current window.
FIXME: does this really have that effect?"
  (full-redisplay (esa:current-window)))

(esa:set-key 'com-full-redisplay
	     'base-table
	     '((#\l :control)))

(defun activate-window (window)
  "Set `window' to be the active window for its Climacs
instance. `Window' must already be recognized by the Climacs
instance."
  ;; Ensure that only one pane can be active.
  (let ((climacs (pane-frame window)))
    (unless (current-window-p window)
      (when (typep (esa:esa-current-window climacs) 'climacs-pane)
        (setf (active (esa:esa-current-window climacs)) nil))
      (unless (member window (esa:windows climacs))
        (error "Cannot set unknown window to be active window"))
      (setf (esa:windows climacs)
            (cons window (remove window (esa:windows climacs)))))
    (ensure-only-view-active
     climacs (when (typep window 'climacs-pane)
               (view window)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Pane functions

(defun replace-constellation (constellation additional-constellation vertical-p)
  (let* ((parent (sheet-parent constellation))
	 (children (sheet-children parent))
	 (first (first children))
	 (second (second children))
	 (third (third children))
	 (first-split-p (= (length (sheet-children parent)) 2))
	 (parent-region (sheet-region parent))
	 (parent-height (rectangle-height parent-region))
	 (parent-width (rectangle-width parent-region))
	 (filler (when first-split-p (make-pane 'basic-pane))) ;Prevents resizing.
         (adjust #+mcclim (make-pane 'clim-extensions:box-adjuster-gadget)))
    (assert (member constellation children))
    
    (when first-split-p (setf (sheet-region filler) (sheet-region parent)) 
      (sheet-adopt-child parent filler))

    (sheet-disown-child parent constellation)

    (if vertical-p
	(resize-sheet constellation parent-width (/ parent-height 2))
	(resize-sheet constellation  (/ parent-width 2) parent-height))
    
    (let ((new (if vertical-p
		   (vertically ()
		     constellation adjust additional-constellation)
		   (horizontally ()
		     constellation adjust additional-constellation))))
      (sheet-adopt-child parent new)

      (when first-split-p (sheet-disown-child parent filler))
      (reorder-sheets parent 
		      (if (eq constellation first)
			  (if third
			      (list new second third)
			      (list new second))
			  (if third
			      (list first second new)
			      (list first new)))))))
(defun find-parent (sheet)
  (loop for parent = (sheet-parent sheet)
	  then (sheet-parent parent)
	until (typep parent 'vrack-pane)
	finally (return parent)))

(defun make-pane-constellation (&optional (with-scrollbars *with-scrollbars*))
  "make a vbox containing a scroller pane as its first child and an
info pane as its second child.  The scroller pane contains a viewport
which contains an extended pane.  Return the vbox and the extended pane
as two values.
If with-scrollbars nil, omit the scroller."
  (let* ((climacs-pane
	  (make-pane 'climacs-pane :name 'window))
	 (vbox
	  (vertically ()
	    (if with-scrollbars
		(scrolling ()
		  climacs-pane)
		climacs-pane)
	    (make-pane 'climacs-info-pane
                       :master-pane climacs-pane))))
    (values vbox climacs-pane)))

(defgeneric setup-split-pane (orig-pane new-pane clone-view)
  (:documentation "Perform split-setup operations `new-pane',
which is supposed to be a pane that has been freshly split from
`orig-pane'. If `clone-view' is true, set the view of the new
pane to a clone of the view in `orig-pane', provided that
`orig-pane' has a view."))

(defmethod setup-split-pane ((orig-pane climacs-pane) (new-pane climacs-pane) clone-view)
  (when (buffer-view-p (view orig-pane))
    (setf (offset (point (esa-io:buffer (view orig-pane)))) (offset (point (view orig-pane)))))
  (setf (view new-pane) (if clone-view
                            (clone-view-for-climacs (pane-frame orig-pane) (view orig-pane))
                            (any-preferably-undisplayed-view))))

(defun split-window (&optional (vertically-p nil) (clone-view nil) (pane (esa:current-window)))
  (with-look-and-feel-realization
      ((frame-manager esa:*esa-instance*) esa:*esa-instance*)
    (multiple-value-bind (vbox new-pane) (make-pane-constellation)
      (let* ((current-window pane)
	     (constellation-root (find-parent current-window)))
        (setup-split-pane current-window new-pane clone-view)
	(push new-pane (rest (esa:windows esa:*esa-instance*)))
	(replace-constellation constellation-root vbox vertically-p)
	(full-redisplay current-window)
	(full-redisplay new-pane)
        (activate-window pane)
	new-pane))))

(defun delete-window (&optional (window (esa:current-window)))
  (unless (null (cdr (esa:windows esa:*esa-instance*)))
    (let* ((constellation (find-parent window))
	   (box (sheet-parent constellation))
	   (box-children (sheet-children box))
	   (other (if (eq constellation (first box-children))
		      (third box-children)
		      (first box-children)))
	   (parent (sheet-parent box))
	   (children (sheet-children parent))
	   (first (first children))
	   (second (second children))
	   (third (third children)))
      (setf (esa:windows esa:*esa-instance*)
	    (delete window (esa:windows esa:*esa-instance*)))
      (sheet-disown-child box other)
      (sheet-adopt-child parent other)
      (sheet-disown-child parent box)
      (reorder-sheets parent (if (eq box first)
				 (if third
				     (list other second third)
				     (list other second))
				 (if third
				     (list first second other)
				     (list first other)))))))

(defun other-window (&optional pane)
  (if (and pane (find pane (esa:windows esa:*esa-instance*)))
      (setf (esa:windows esa:*esa-instance*)
            (append (list pane)
                    (remove pane (esa:windows esa:*esa-instance*))))
      (setf (esa:windows esa:*esa-instance*)
            (append (rest (esa:windows esa:*esa-instance*))
                    (list (esa:esa-current-window esa:*esa-instance*)))))
  (activate-window (esa:esa-current-window esa:*esa-instance*)))

;;; For the ESA help functions.

(defmethod esa:invoke-with-help-stream ((frame climacs) title continuation)
  (with-typeout-view (stream title t)
    (funcall continuation stream)))

(defmethod frame-make-new-buffer ((application-frame climacs)
                                  &key (name "*scratch*"))
  (make-instance 'climacs-buffer :name name))

(defmethod frame-exit :around ((frame climacs) #-mcclim &key)
  (dolist (view (views frame))
    (handler-case
        (when (and (climacs-core:buffer-of-view-needs-saving view)
                   (handler-case (accept 'boolean
                                  :prompt (format nil "Save buffer of view: ~a ?" (name view)))
                     (error () (progn (beep)
                                      (esa:display-message "Invalid answer")
                                      (return-from frame-exit nil)))))
          (esa-io:save-buffer (esa-io:buffer view)))
      (file-error (e)
        (esa:display-message "~A (hit a key to continue)" e)
        (read-gesture))))
  (when (or (notany #'climacs-core:buffer-of-view-needs-saving (views frame))
	    (handler-case (accept 'boolean :prompt "Modified buffers of views exist.  Quit anyway?")
	      (error () (progn (beep)
			       (esa:display-message "Invalid answer")
			       (return-from frame-exit nil)))))
    (call-next-method)))

(defmethod switch-to-view ((drei climacs-pane) (view drei-view))
  (setf (view drei) view))

(defmethod esa-io:frame-find-file ((application-frame climacs) filepath)
  (climacs-core:find-file-impl filepath nil))

(defmethod esa-io:frame-find-file-read-only ((application-frame climacs) filepath)
  (climacs-core:find-file-impl filepath t))

(defmethod frame-set-visited-filename ((application-frame climacs) filepath buffer)
  (setf (filepath buffer) (pathname filepath)
	(file-saved-p buffer) nil
	(file-write-time buffer) nil
	(name buffer) (climacs-core:filepath-filename filepath)
	(needs-saving buffer) t))
