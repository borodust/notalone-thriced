(cl:in-package :notalone-thriced)

;;;
;;; GAME
;;;
(cffi:defcstruct banner-vertex
  (pos :float :count 2)
  (uv :float :count 2))


(cffi:defcstruct banner-data
  (vertices (:struct banner-vertex) :count 4)
  (indices :uint16 :count 6))


(defun make-banner-data (width height)
  (flet ((init-vertex (vertex x0 y0 x1 y1 )
           (cref:c-val ((vertex (:struct banner-vertex)))
             (setf (vertex :pos 0) (float x0 0f0)
                   (vertex :pos 1) (float y0 0f0)

                   (vertex :uv 0) (float x1 0f0)
                   (vertex :uv 1) (float y1 0f0)))))
    (cref:c-let ((data (:struct banner-data) :alloc t))
      (init-vertex (data :vertices 0 &)
                   0 0
                   0 1)
      (init-vertex (data :vertices 1 &)
                   width 0
                   1 1)
      (init-vertex (data :vertices 2 &)
                   width height
                   1 0)
      (init-vertex (data :vertices 3 &)
                   0 height
                   0 0)

      (setf (data :indices 0) 0
            (data :indices 1) 1
            (data :indices 2) 2

            (data :indices 3) 0
            (data :indices 4) 2
            (data :indices 5) 3)
      (data &))))


(defstruct (banner
            (:constructor %make-banner)
            (:conc-name %banner-))
  sampler
  data
  mat-instance
  renderable
  vbuf
  ibuf)


(defun make-banner (renderer material width height)
  (let* ((vertex-size (cffi:foreign-type-size '(:struct banner-vertex)))
         (index-size (cffi:foreign-type-size :uint16))
         (pos-size (* 2 (cffi:foreign-type-size :float)))

         (vbuf (aw:make-vertex-buffer renderer 4
                                      (aw:.attribute :position :float2 0 vertex-size)
                                      (aw:.attribute :uv0 :float2 pos-size vertex-size)))
         (ibuf (aw:make-index-buffer renderer 6
                                     (aw:.type :ushort)))
         (data (make-banner-data width height))
         (sampler (aw:make-sampler))
         (mat-instance (aw:make-material-instance material)))
    (cref:c-val ((data (:struct banner-data)))
      (aw:fill-vertex-buffer renderer vbuf (data :vertices &)
                             (* 4 vertex-size))
      (aw:fill-index-buffer renderer ibuf
                            (data :indices &)
                            (* 6 index-size)))
    (%make-banner :vbuf vbuf
                  :ibuf ibuf
                  :data data
                  :sampler sampler
                  :mat-instance mat-instance
                  :renderable (aw:make-renderable renderer 1
                                                  (aw:.bounding-box 0 0 0 1 1 1)
                                                  (aw:.culling nil)
                                                  (aw:.receive-shadows nil)
                                                  (aw:.cast-shadows nil)

                                                  (aw:.geometry 0 :triangles vbuf ibuf)
                                                  (aw:.material 0 mat-instance)))))


(defun destroy-banner (banner))


(defun banner-entity (banner)
  (%banner-renderable banner))


(defun banner-texture (banner)
  (declare (ignore banner))
  (error "Not implemented"))


(defun (setf banner-texture) (texture banner)
  (setf
   (aw:material-instance-parameter-sampler (%banner-mat-instance banner) "banner" texture)
   (%banner-sampler banner))
  texture)
