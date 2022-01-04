(cl:in-package :notalone-thriced)


(declaim (special *canvas*
                  *title-typeface*
                  *menu-typeface*))


(defclass notalone-state ()
  (canvas
   banner
   title-typeface
   menu-typeface))


(defmethod initialize-instance :after ((this notalone-state) &key )
  (with-slots (canvas banner banner-entity title-typeface menu-typeface) this
    (setf canvas (aw:make-canvas *width* *height*
                                 :framebuffer-width *framebuffer-width*
                                 :framebuffer-height *framebuffer-height*)
          banner (make-banner (find-asset :material "banner") *width* *height*)
          title-typeface (find-asset :typeface "sector17")
          menu-typeface (find-asset :typeface "sector34"))
    (aw:add-scene-entity *overlay* (banner-entity banner))))


(defmethod withdraw :before ((this notalone-state))
  (with-slots (canvas banner) this
    (aw:remove-scene-entity *overlay* (banner-entity banner))
    (destroy-banner banner)
    (aw:destroy-canvas canvas)))


(defmethod draw :around ((this notalone-state))
  (with-slots (canvas banner title-typeface menu-typeface) this
    (setf (banner-texture banner) (aw:canvas-texture canvas))
    (aw:with-canvas (canvas)
      (let ((*title-typeface* title-typeface)
            (*menu-typeface* menu-typeface))
        (call-next-method)))))


(defun title-paint-color ()
  (aw:paint-color 0.5 0.7 0))


(defun menu-paint-color ()
  (aw:paint-color 0.75 0.6 0))
