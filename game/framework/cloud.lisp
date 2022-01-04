(cl:in-package :notalone-thriced)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (a:define-constant +particle-count+ 20))


(cffi:defcstruct particle-cloud-vertex
  (pos :float :count 3)
  (col :float :count 4)
  (vel :float :count 3))


(cffi:defcstruct particle-cloud-data
  (vertices (:struct particle-cloud-vertex) :count #.+particle-count+)
  (indices :uint16 :count #.+particle-count+))


(defun make-particle-cloud-data (x y angle vel-x vel-y)
  (with-transform (pos-transform
                   (:translation :x x :y y))
    (with-transform (vel-transform
                     (:rotation angle :z 1))
      (aw:with-vec4* ((tmp :x 0 :y 0 :z 0.2 :w 1)
                      (tvel)
                      (tpos))
        (aw:mat4-vec-mult tpos pos-transform tmp)

        (aw:with-vec4 (col :x 100  :y 0 :z 0 :w 1)
          (aw:with-vec3* ((pos :x (aw:vec4 tpos 0)
                               :y (aw:vec4 tpos 1)
                               :z (aw:vec4 tpos 2))
                          (vel))
            (flet ((update-velocity ()
                     (let ((radius 0.5))
                       (setf (aw:vec4 tmp 0) 2
                             (aw:vec4 tmp 1) (- radius (random (float (* 2 radius) 0f0)))
                             (aw:vec4 tmp 2) (+ (aw:vec3 pos 2)
                                                (- radius (random (float (* 2 radius) 0f0))))
                             (aw:vec4 tmp 3) 1)
                       (aw:mat4-vec-mult tvel vel-transform tmp)
                       (setf (aw:vec3 vel 0) (+ (* vel-x 80) (aw:vec4 tvel 0))
                             (aw:vec3 vel 1) (+ (* vel-y 80) (aw:vec4 tvel 1))
                             (aw:vec3 vel 2) (aw:vec4 tvel 2))))
                   (init-vertex (vertex
                                 pos-x pos-y pos-z
                                 vel-x vel-y vel-z
                                 col-r col-g col-b col-a)
                     (cref:c-val ((vertex (:struct particle-cloud-vertex)))
                       (setf (vertex :pos 0) (float pos-x 0f0)
                             (vertex :pos 1) (float pos-y 0f0)
                             (vertex :pos 2) (float pos-z 0f0)

                             (vertex :col 0) (float col-r 0f0)
                             (vertex :col 1) (float col-g 0f0)
                             (vertex :col 2) (float col-b 0f0)
                             (vertex :col 3) (float col-a 0f0)

                             (vertex :vel 0) (float vel-x 0f0)
                             (vertex :vel 1) (float vel-y 0f0)
                             (vertex :vel 2) (float vel-z 0f0)))))
              (cref:c-let ((data (:struct particle-cloud-data) :alloc t))
                (loop for i below +particle-count+
                      do (update-velocity)
                         (init-vertex
                          (data :vertices i &)
                          (aw:vec3 pos 0) (aw:vec3 pos 1) (aw:vec3 pos 2)
                          (aw:vec3 vel 0) (aw:vec3 vel 1) (aw:vec3 vel 2)
                          (aw:vec4 col 0) (aw:vec4 col 1) (aw:vec4 col 2) (aw:vec4 col 3))
                         (setf (data :indices i) i))
                (data &)))))))))


(defstruct (particle-cloud
            (:constructor %make-particle-cloud)
            (:conc-name %particle-cloud-))
  data
  mat-instance
  renderable
  vbuf
  ibuf)


(defun make-particle-cloud (material x y angle vel-x vel-y)
  (let* ((vertex-size (cffi:foreign-type-size '(:struct particle-cloud-vertex)))
         (index-size (cffi:foreign-type-size :uint16))
         (color-offset (* 3 (cffi:foreign-type-size :float)))
         (velocity-offset (+ color-offset (* 4 (cffi:foreign-type-size :float))))

         (vbuf (aw:make-vertex-buffer +particle-count+
                                      (aw:.attribute :position :float3 0 vertex-size)
                                      (aw:.attribute :color :float4 color-offset vertex-size)
                                      (aw:.attribute :custom0 :float3 velocity-offset vertex-size)))
         (ibuf (aw:make-index-buffer +particle-count+
                                     (aw:.type :ushort)))
         (data (make-particle-cloud-data x y angle vel-x vel-y))
         (mat-instance (aw:make-material-instance material)))
    (cref:c-val ((data (:struct particle-cloud-data)))
      (aw:fill-vertex-buffer vbuf (data :vertices &)
                             (* +particle-count+ vertex-size))
      (aw:fill-index-buffer ibuf
                            (data :indices &)
                            (* +particle-count+ index-size)))
    (%make-particle-cloud :vbuf vbuf
                          :ibuf ibuf
                          :data data
                          :mat-instance mat-instance
                          :renderable (aw:make-renderable 1
                                                          (aw:.bounding-box -0.001 -0.001 -0.001
                                                                            0.001 0.001 0.001)
                                                          (aw:.culling nil)
                                                          (aw:.receive-shadows nil)
                                                          (aw:.cast-shadows nil)

                                                          (aw:.geometry 0 :points vbuf ibuf)
                                                          (aw:.material 0 mat-instance)))))


(defun destroy-particle-cloud (particle-cloud))


(defun particle-cloud-entity (particle-cloud)
  (%particle-cloud-renderable particle-cloud))


(defun (setf particle-cloud-delta) (delta particle-cloud)
  (setf
   (aw:material-instance-parameter-float (%particle-cloud-mat-instance particle-cloud) "delta")
   delta)
  delta)
