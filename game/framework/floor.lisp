(cl:in-package :notalone-thriced)

;;;
;;; GAME
;;;
(defstruct (%floor
            (:constructor %make-floor)
            (:conc-name %floor-))
  samplers
  data
  mat-instance
  renderable
  vbuf
  ibuf)


(defun make-floor (renderer material base-color-texture normal-texture arm-texture)
  (let* ((vertex-size (cffi:foreign-type-size '(:struct banner-vertex)))
         (index-size (cffi:foreign-type-size :uint16))
         (pos-size (* 2 (cffi:foreign-type-size :float)))

         (vbuf (aw:make-vertex-buffer renderer 4
                                      (aw:.attribute :position :float2 0 vertex-size)
                                      (aw:.attribute :uv0 :float2 pos-size vertex-size)))
         (ibuf (aw:make-index-buffer renderer 6
                                     (aw:.type :ushort)))
         (data (make-banner-data 1 1))
         (samplers (list (aw:make-sampler)
                         (aw:make-sampler)
                         (aw:make-sampler)))
         (mat-instance (aw:make-material-instance material)))
    (cref:c-val ((data (:struct banner-data)))
      (aw:fill-vertex-buffer renderer vbuf (data :vertices &)
                             (* 4 vertex-size))
      (aw:fill-index-buffer renderer ibuf
                            (data :indices &)
                            (* 6 index-size)))
    (loop for (name . tex) in `(("baseColor" . ,base-color-texture)
                                ("normal" . ,normal-texture)
                                ("arm" . ,arm-texture))
          for sampler in samplers
          do (setf
              (aw:material-instance-parameter-sampler mat-instance name tex)
              sampler))
    (%make-floor :vbuf vbuf
                 :ibuf ibuf
                 :data data
                 :samplers samplers
                 :mat-instance mat-instance
                 :renderable (aw:make-renderable renderer 1
                                                 (aw:.bounding-box -0.001 -0.001 -0.001
                                                                   1.001 1.001 0.001)
                                                 (aw:.culling t)
                                                 (aw:.receive-shadows t)
                                                 (aw:.cast-shadows nil)

                                                 (aw:.geometry 0 :triangles vbuf ibuf)
                                                 (aw:.material 0 mat-instance)))))


(defun destroy-floor (banner))


(defun floor-entity (banner)
  (%floor-renderable banner))
