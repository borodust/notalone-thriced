(cl:in-package :notalone-thriced.tools)

(declaim (special *tools*))


(defclass game-tools ()
  ((ui :initarg :ui)
   (ui-hidden-p :initform t)
   (windows :initform (list 'eden-window))
   (context :initform (make-hash-table :test 'equal))
   (last-time-delta :initform 0)))


(defun open-tool-window (display-function-name)
  (with-slots (windows) *tools*
    (pushnew display-function-name windows)))


(defun close-tool-window (display-function-name)
  (with-slots (windows) *tools*
    (a:deletef windows display-function-name)))


(defun context-property (name)
  (with-slots (context) *tools*
    (gethash name context)))


(defun (setf context-property) (value name)
  (with-slots (context) *tools*
    (setf (gethash name context) value)))


(defmethod notalone-thriced::make-tools ((name (eql :notalone-thriced-tools)) &key renderer)
  (make-instance 'game-tools
                 :ui (awt:make-ui renderer)))


(defmethod notalone-thriced::destroy-tools ((tools game-tools))
  (with-slots (ui) tools
    (awt:destroy-ui ui)))


(defmethod notalone-thriced::call-with-tools ((this game-tools) body)
  (with-slots (ui) this
    (let ((*tools* this))
      (funcall body))))


(defmethod notalone-thriced::handle-tool-event ((this game-tools) event)
  (with-slots (ui-hidden-p ui) this
    (awt:handle-ui-event ui event)
    (case (aw:event-type event)
      (:keyboard-button-down
       (when (eq :grave (aw:event-key-scan-code event))
         (setf ui-hidden-p (not ui-hidden-p)))))))


(defmethod notalone-thriced::update-tools (tools time-delta)
  (with-slots (last-time-delta ui) tools
    (setf last-time-delta time-delta)
    (awt:update-ui-input ui)))


(defmethod notalone-thriced::render-tools (tools)
  (with-slots (last-time-delta windows ui ui-hidden-p) tools
    (awt:ui (ui 1280 960 last-time-delta)
      (unless ui-hidden-p
        (loop for window in windows
              do (funcall window))))
    (setf last-time-delta 0)))


(defmacro define-tool-window ((id title &key on-close width height) &body body)
  (a:with-gensyms (result)
    `(defun ,id ()
       (awt:with-panel (,title
                        :on-close (lambda (,result)
                                    (declare (ignore ,result))
                                    ,@(when on-close
                                        `((funcall ,on-close)))
                                    (close-tool-window ',id))
                        ,@(when width `(:width ,width))
                        ,@(when height `(:height ,height)))
         ,@body))))
