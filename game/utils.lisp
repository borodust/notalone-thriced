(cl:in-package :notalone-thriced)


(declaim (special *renderer*))

(defvar *unloaded-foreign-libraries* nil)


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
