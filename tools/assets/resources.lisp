(cl:in-package :notalone-thriced.tools)


(defparameter *banner-material-source*
  "
material {
    name : banner,
    parameters: [
      { type: sampler2d, name: banner }
    ],
    requires : [
      uv0
    ],
    shadingModel : unlit,
    culling : none,
    blending : transparent
}
fragment {
    void material(inout MaterialInputs material) {
        prepareMaterial(material);
        material.baseColor = textureLod(materialParams_banner, getUV0(), 0.0);
    }
}
")

(defun asset-path (name)
  (notalone-thriced::asset-path name))


(defun asset-image (asset-path &key name)
  (let ((name (if name name (file-namestring asset-path))))
    (alien-works.tools:load-image name (asset-path asset-path))))


(defun load-cubemap (px-path nx-path py-path ny-path pz-path nz-path)
  (let* ((images (loop for path in (list px-path nx-path py-path ny-path pz-path nz-path)
                       collect (awt:load-image (file-namestring path) path)))
         (width (awt:image-width (first images)))
         (height (awt:image-height (first images)))
         (channels (awt:image-channels (first images)))
         (sizes (loop for image in images
                      unless (and (= (awt:image-width image) width)
                                  (= (awt:image-height image) height)
                                  (= (awt:image-channels image) channels))
                        do (error "Cubemap face image with wrong dimensions found")
                      collect (* (awt:image-width image)
                                 (awt:image-height image)
                                 (awt:image-channels image))))
         (total-size (reduce #'+ sizes))
         (data (let ((data (cffi:foreign-alloc :char :count total-size)))
                 (loop with offset = 0
                       for size in sizes
                       for image in images
                       do (aw:memcpy (cffi:inc-pointer data offset) (awt:image-data image) size)
                          (incf offset size))
                 data))
         (pixel-buffer (aw:make-pixel-buffer data
                                             total-size
                                             (ecase channels
                                               (1 :r)
                                               (2 :rg)
                                               (3 :rgb)
                                               (4 :rgba))
                                             :ubyte
                                             (lambda () (cffi:foreign-free data))))
         (texture (aw:make-texture notalone-thriced::*renderer*
                                   (aw:.width width)
                                   (aw:.height height)
                                   (aw:.format (ecase channels
                                                 (1 :r8)
                                                 (2 :rg8)
                                                 (3 :rgb8)
                                                 (4 :rgba8)))
                                   (aw:.sampler :cubemap))))
    (unwind-protect
         (progn
           (apply #'aw:update-cubemap-images notalone-thriced::*renderer* texture 0 pixel-buffer sizes)
           (aw:generate-texture-mipmaps notalone-thriced::*renderer* texture))
      (aw:destroy-pixel-buffer pixel-buffer))
    texture))


(defun convert-helmet ()
  (let ((resources (notalone-thriced.tools::parse-gltf
                    (asset-path "src/helmet/DamagedHelmet.gltf"))))
    (apply #'notalone-thriced::save-resources (asset-path "helmet.bin") resources)))


(defun generate-indirect-light-map (&key (cmgen "cmgen"))
  (uiop:run-program `(,cmgen
                      ,(format nil "--deploy=~A" (namestring (asset-path "src/indirect/")))
                      "--format=png"
                      ,(namestring (asset-path "local/skybox.png")))
                    :force-shell t
                    :error-output *error-output*))


(defun convert-indirect ()
  (let ((resources (notalone-thriced.tools::cubemap->resources
                    "indirect"
                    (asset-image "src/indirect/skybox/m1_px.png")
                    (asset-image "src/indirect/skybox/m1_nx.png")
                    (asset-image "src/indirect/skybox/m1_py.png")
                    (asset-image "src/indirect/skybox/m1_ny.png")
                    (asset-image "src/indirect/skybox/m1_pz.png")
                    (asset-image "src/indirect/skybox/m1_nz.png"))))
    (apply #'notalone-thriced::save-resources (asset-path "indirect.bin") resources)))


(defun convert-audio-with-ffmpeg (dst src)
  (uiop:run-program `("ffmpeg" "-y"
                      "-i" ,(namestring src)
                      "-f" "s16le"
                      "-ar" "48000"
                      "-ac" "1"
                      "-acodec" "pcm_s16le"
                      ,(namestring dst))
                    :output nil
                    :error-output nil))


(defun convert-audio ()
  (uiop:with-temporary-file (:pathname tmpfile)
    (apply #'notalone-thriced::save-resources (asset-path "audio.bin")
           (nconc
            (progn
              (convert-audio-with-ffmpeg tmpfile
                                         (asset-path "src/audio/53933__meutecee__trumpethit03.wav"))
              (alexandria:with-input-from-file (in tmpfile :element-type '(signed-byte 16))
                (notalone-thriced.tools::pcm->resources "booo" in)))
            (progn
              (convert-audio-with-ffmpeg tmpfile (asset-path "src/audio/kai_engel_moonlight_reprise.mp3"))
              (alexandria:with-input-from-file (in tmpfile :element-type '(signed-byte 16))
                (notalone-thriced.tools::pcm->resources "theme" in)))))))


(defun update-assets ()
  (let (assets)
    (multiple-value-bind (data size)
        (awt:parse-material *banner-material-source*) assets
      (push (notalone-thriced::make-material-resource "banner" data size) assets))
    (apply #'notalone-thriced::save-resources (asset-path "assets.bin") assets)))
