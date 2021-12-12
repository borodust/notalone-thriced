(cl:in-package :notalone-thriced)


(declaim (special *renderer*
                  *scene*
                  *overlay*
                  *width*
                  *height*
                  *framebuffer-width*
                  *framebuffer-height*))

(defvar *unloaded-foreign-libraries* nil)


(defparameter *debug-table* (make-hash-table :test #'equal))


(defun debug-print (name value)
  (setf (gethash name *debug-table*) value))


(defun debug-format (name control &rest args)
  (setf (gethash name *debug-table*) (apply #'format nil control args)))


(defun shout (control &rest args)
  (format *standard-output* "~&")
  (apply #'format *standard-output* control args)
  (finish-output *standard-output*))


(defun unload-foreign-libraries ()
  (bodge-blobs-support:close-foreign-libraries)
  (handler-bind ((style-warning #'muffle-warning))
    (loop for lib in (cffi:list-foreign-libraries :loaded-only t)
          do (progn
               (pushnew (cffi:foreign-library-name lib) *unloaded-foreign-libraries*
                        :test #'equal)
               (cffi:close-foreign-library lib)))))


(defun reload-foreign-libraries ()
  (bodge-blobs-support:load-foreign-libraries)
  (loop for lib-name in *unloaded-foreign-libraries*
        do (cffi:load-foreign-library lib-name))
  (setf *unloaded-foreign-libraries* nil))


(uiop:register-image-dump-hook 'unload-foreign-libraries)


(defmacro with-transform ((transform &rest operations) &body body)
  (alexandria:with-gensyms (transform0 transform1 vec)
    (flet ((%expand-transform (result source operation-desc)
             (let* ((operation (first operation-desc)))
               (if (eq operation :transform)
                   `(aw:mat4-mult ,result ,source ,(second operation-desc))
                   (let ((vec-config (if (eq operation :rotation)
                                         (cddr operation-desc)
                                         (rest operation-desc))))
                     (destructuring-bind (&key x y z) vec-config
                       `(aw:with-vec3 (,vec
                                       ,@(when x `(:x ,x))
                                       ,@(when y `(:y ,y))
                                       ,@(when z `(:z ,z)))
                          ,(ecase operation
                             (:rotation `(aw:rotate-mat4 ,result ,source ,(second operation-desc) ,vec))
                             (:translation `(aw:translate-mat4 ,result ,source ,vec))
                             (:scale `(aw:scale-mat4 ,result ,source ,vec))))))))))
      `(aw:with-mat4* (,transform0
                       ,transform1)
         ,@(loop with result = transform0 and source = transform1
                 for operation in operations
                 collect (prog1 (%expand-transform result source operation)
                           (rotatef result source))
                   into transforms
                 finally (return (append transforms
                                         `((let ((,transform ,source))
                                             ,@body)))))))))


(defun real-time-seconds ()
  (float (/ (get-internal-real-time) internal-time-units-per-second) 0f0))
