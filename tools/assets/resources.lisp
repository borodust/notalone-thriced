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


(defparameter *floor-material-source*
  "
material {
    name : floor,
    parameters: [
      { type: sampler2d, name: baseColor },
      { type: sampler2d, name: normal },
      { type: sampler2d, name: arm }
    ],
    requires : [
      uv0
    ],
    shadingModel : lit,
    blending : opaque
}
fragment {
    void material(inout MaterialInputs material) {
        prepareMaterial(material);
        material.baseColor = textureLod(materialParams_baseColor, getUV0(), 0.0);
        material.normal = texture(materialParams_normal, getUV0()).xyz * 2.0 - 1.0;

        vec3 arm = textureLod(materialParams_arm, getUV0(), 0.0).rgb;
        material.ambientOcclusion = arm.r;
        material.roughness = arm.g;
        material.metallic = arm.b;
    }
}
")


(defparameter *particle-material-source*
  "
material {
    name : particle_cloud,
    requires : [
        custom0,
        color
    ],
    shadingModel : unlit,
    blending : add,
    parameters : [
      { type: float, name: delta }
    ]
}

vertex {
    void materialVertex(inout MaterialVertexInputs material) {
        float3 offset = vec3(getCustom0().xyz * materialParams.delta);
        material.worldPosition = vec4(material.worldPosition.xyz + offset.xyz, material.worldPosition.w);
        gl_PointSize = 10.0 - 25.0 * materialParams.delta;
    }
}

fragment {
    void material(inout MaterialInputs material) {
        vec2 coord = gl_PointCoord - vec2(0.5);
        if(length(coord) > 0.5)
           discard;

        prepareMaterial(material);
        material.baseColor = getColor();
    }
}
")

(defun asset-path (name)
  (notalone-thriced::asset-path name))


(defun asset-image (asset-path &key name)
  (let ((name (if name name (file-namestring asset-path))))
    (awt:load-image name (asset-path asset-path))))


(defun convert-audio-with-ffmpeg (dst src &key (channels 1))
  (uiop:run-program `("ffmpeg" "-y"
                               "-i" ,(namestring src)
                               "-f" "s16le"
                               "-ar" "48000"
                               "-ac" ,(format nil "~A" channels)
                               "-acodec" "pcm_s16le"
                               ,(namestring dst))
                    :output nil
                    :error-output *error-output*))


(defun parse-audio (name path &key (channels 1))
  (uiop:with-temporary-file (:pathname tmpfile)
    (convert-audio-with-ffmpeg tmpfile path :channels channels)
    (alexandria:with-input-from-file (in tmpfile :element-type '(signed-byte 16))
      (pcm->resources name in :channels channels))))


(defun update-assets ()
  (let (assets)
    (labels ((%add (&rest resources)
               (a:nconcf assets resources))
             (%add-image (name path)
               (apply #'%add
                      (image->resources (asset-image (merge-pathnames path "src/textures/")
                                                     :name name)))))
      ;; banner
      (multiple-value-bind (data size)
          (awt:parse-material *banner-material-source*) assets
        (%add (notalone-thriced::make-material-resource "banner" data size)))

      ;; floor
      (multiple-value-bind (data size)
          (awt:parse-material *floor-material-source*) assets
        (%add (notalone-thriced::make-material-resource "floor" data size)))
      (%add-image "floor_baseColor" "floors/main/Sci-fi_Floor_001_basecolor.jpg")
      (%add-image "floor_normal" "floors/main/Sci-fi_Floor_001_normal.jpg")
      (%add-image "floor_arm" "floors/main/Sci-fi_Floor_001_arm.png")

      ;; particles
      (multiple-value-bind (data size)
          (awt:parse-material *particle-material-source*) assets
        (%add (notalone-thriced::make-material-resource "particle" data size)))

      ;; alien
      (apply #'%add (parse-gltf "alien" (asset-path "src/models/alien/scene.gltf")))

      ;; audio
      (apply #'%add (parse-audio "crawl" (asset-path "src/audio/3/slime_001.ogg")))
      (apply #'%add (parse-audio "spawn" (asset-path "src/audio/2/deathb.wav")))
      (apply #'%add (parse-audio "shot" (asset-path "src/audio/5/shotgun.wav")))
      (apply #'%add (parse-audio "menu"
                                 (asset-path "src/audio/4/Iwan_Gabovitch_Dark_Ambience_Loop.ogg")
                                 :channels 2))
      (apply #'%add (parse-audio "action"
                                 (asset-path "src/audio/4/HorrorPen_Dramatic_Action.ogg")
                                 :channels 2)))
    (apply #'notalone-thriced::save-resources (asset-path "assets.bin") assets)))
