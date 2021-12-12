(cl:in-package :notalone-thriced)


(defun calc-angle (vec)
  (aw:with-vec2* ((ref-vec :x 1 :y 0)
                  (nvec))
    (aw:vec2-normalize nvec vec)
    (let ((pre-angle (acos (aw:vec2-dot ref-vec nvec))))
      (if (> (aw:vec2 vec 1) 0)
          pre-angle
          (- (* 2 pi) pre-angle)))))

;;;
;;; ENEMY
;;;
(defstruct (enemy
            (:constructor %make-enemy))
  position
  created
  entity
  shot-p
  (sound-sources (list)))


(defun make-enemy (x y)
  (let ((renderable (funcall (find-asset :renderable "alien.mesh.0.renderable"))))
    (aw:add-scene-entity *scene* renderable)
    (%make-enemy :position (aw:make-vec2 x y)
                 :created (real-time-seconds)
                 :entity renderable
                 :sound-sources (list (let ((source (aw:make-audio-source (find-asset :audio "spawn"))))
                                        (aw:with-vec3 (pos :x x :y y)
                                          (setf (aw:audio-source-position source) pos))
                                        (aw:play-audio-source source)
                                        source)
                                      (let ((source (aw:make-audio-source (find-asset :audio "crawl"))))
                                        (aw:with-vec3 (pos :x x :y y)
                                          (setf (aw:audio-source-position source) pos
                                                (aw:audio-source-looping-p source) t))
                                        (aw:play-audio-source source)
                                        source)))))


(defun update-enemy (enemy)
  (with-slots (player-position) *state*
    (let* ((enemy-position (enemy-position enemy))
           (life (- (real-time-seconds) (enemy-created enemy)))
           (scale (* 0.08 (+ 0.5 (* 0.5 (abs (cos (* life 3))))))))
      (aw:with-vec2* (tmp vel)
        (aw:vec2-subt tmp player-position enemy-position)
        (aw:vec2-normalize vel tmp)
        (let ((distance (aw:vec2-length tmp)))
          (aw:vec2-scalar-mult tmp vel (if (> distance 0.8)
                                           0.015
                                           0.005)))
        (aw:vec2-copy vel tmp)
        (aw:vec2-copy tmp enemy-position)
        (aw:vec2-add enemy-position tmp vel))
      (with-transform (transform
                       (:translation :x (aw:vec2 enemy-position 0)
                                     :y (aw:vec2 enemy-position 1)
                                     :z 0.01)
                       (:rotation life :z 1)
                       (:scale :x scale :y scale :z scale))
        (aw:transform-entity *renderer* (enemy-entity enemy) transform))
      (loop for source in (enemy-sound-sources enemy)
            if (eq :playing (aw:audio-source-state source))
              do (aw:with-vec3 (pos :x (aw:vec2 enemy-position 0)
                                    :y (aw:vec2 enemy-position 1))
                   (setf (aw:audio-source-position source) pos))
            else
              collect source into stopped-sources
            finally (loop for stopped-source in stopped-sources
                          do (aw:destroy-audio-source stopped-source)
                             (a:deletef (enemy-sound-sources enemy)
                                        stopped-source)))))
  (not (enemy-shot-p enemy)))


(defun update-if-enemy-shot (enemy)
  (with-slots (player-position player-angle) *state*
    (let ((enemy-position (enemy-position enemy)))
      (aw:with-vec2* (tmp)
        (aw:vec2-subt tmp enemy-position player-position)
        (let ((enemy-angle (calc-angle tmp)))
          (when (and (< (- player-angle (* (/ pi 180) 15))
                        enemy-angle
                        (+ player-angle (* (/ pi 180) 15)))
                     (> 0.6 (aw:vec2-length tmp)))
            (setf (enemy-shot-p enemy) t)))))))


(defun destroy-enemy (enemy)
  (loop for source in (enemy-sound-sources enemy)
        do (aw:stop-audio-source source)
           (aw:destroy-audio-source source))
  (aw:remove-scene-entity *scene* (enemy-entity enemy))
  (aw:destroy-vec2 (enemy-position enemy)))

;;;
;;; PARTICLE
;;;
(defstruct (particle-cloud-instance
            (:constructor %make-particle-cloud-instance))
  instance
  created)


(defun make-particle-cloud-instance (x y angle vel-x vel-y)
  (let ((particles (make-particle-cloud *renderer* (find-asset :material "particle")
                                        x y angle vel-x vel-y)))
    (setf (particle-cloud-delta particles) 0)
    (aw:add-scene-entity *scene* (particle-cloud-entity particles))
    (%make-particle-cloud-instance :instance particles)))


(defun update-particle-cloud-instance (instance)
  (let ((particles (particle-cloud-instance-instance instance))
        (current-time (real-time-seconds)))
    (unless (particle-cloud-instance-created instance)
      (setf (particle-cloud-instance-created instance) current-time))
    (let ((delta (- current-time
                    (particle-cloud-instance-created instance))))
      (setf (particle-cloud-delta particles) (+ 0.05
                                                (if (zerop delta)
                                                    0
                                                    (* 1/4 (log (+ 1 (* 25 delta)) 10))))))))


(defun particle-cloud-instance-life (instance)
  (if (particle-cloud-instance-created instance)
      (- (real-time-seconds) (particle-cloud-instance-created instance))
      0))


(defun destroy-particle-cloud-instance (instance)
  (let ((instance (particle-cloud-instance-instance instance)))
    (aw:remove-scene-entity *scene* (particle-cloud-entity instance))
    (destroy-particle-cloud instance)))


;;;
;;; ENDLESS FLOOR
;;;
(defstruct (endless-floor
            (:constructor %make-endless-floor))
  (patches nil))


(defun make-endless-floor (width height)
  (let ((patches (make-array (list width height))))
    (loop for i below width
          do (loop for j below height
                   do (setf
                       (aref patches i j)
                       (make-floor *renderer*
                                   (find-asset :material "floor")
                                   (find-asset :texture "floor_baseColor")
                                   (find-asset :texture "floor_normal")
                                   (find-asset :texture "floor_arm")))
                      (aw:add-scene-entity *scene* (floor-entity (aref patches i j)))))
    (%make-endless-floor :patches patches)))


(defun update-endless-floor (floor x y)
  (let* ((patches (endless-floor-patches floor))
         (width (array-dimension patches 0))
         (height (array-dimension patches 1))
         (offset-x (+ (/ width -2) (floor x)))
         (offset-y (+ (/ height -2) (floor y))))

    (with-transform (root-transform
                     (:translation :x offset-x :y offset-y :z 0))

      (loop for i below width
            do (loop for j below height
                     for patch = (aref patches i j)
                     do (with-transform (transform
                                         (:transform root-transform)
                                         (:translation :x i :y j))
                          (aw:transform-entity *renderer* (floor-entity patch) transform)))))))


(defun destroy-endless-floor (floor)
  (let ((patches (endless-floor-patches floor)))
    (loop for i below (array-dimension patches 0)
          do (loop for j below (array-dimension patches 1)
                   for patch = (aref patches i j)
                   do (aw:remove-scene-entity *scene* (floor-entity patch))
                      (destroy-floor patch)))))

;;;
;;; STATE
;;;
(defclass gameplay-state (notalone-state)
  ((mouse-state :initform (aw:make-mouse-state))
   (key-bag :initform (list))
   (player-angle :initform (/ pi 2))
   (player-velocity :initform (aw:make-vec2 0 0))
   (player-position :initform (aw:make-vec2 0 0))
   (particles :initform (list))
   (enemies :initform (list))
   (player-sources :initform (list))
   (kill-count :initform 0)
   (started :initform (real-time-seconds))
   floor flashlight))


(defmethod initialize-instance :after ((this gameplay-state) &key)
  (with-slots (floor flashlight player-sources) this
    (setf floor (make-endless-floor 5 4)
          flashlight (aw:with-vec3 (light-direction :x 0f0 :y 0f0 :z -1f0)
                       (aw:make-light *renderer* :focused-spot
                                      (aw:.cast-shadows t)
                                      (aw:.intensity 100000)
                                      (aw:.direction light-direction)
                                      (aw:.falloff 0.9)
                                      (aw:.spot-light-cone (/ pi 8) (/ pi 4)))))

    (let ((music (aw:make-audio-source (find-asset :audio "action"))))
      (push music player-sources)
      (aw:play-audio-source music)
      (setf (aw:audio-source-looping-p music) t))
    (aw:add-scene-entity *scene* flashlight)))


(defmethod withdraw ((this gameplay-state))
  (with-slots (floor flashlight particles player-velocity player-position enemies
               player-sources)
      this

    (loop for instance in particles
          do (destroy-particle-cloud-instance instance))

    (aw:remove-scene-entity *scene* flashlight)
    (loop for enemy in enemies
          do (destroy-enemy enemy))
    (loop for source in player-sources
          do (aw:destroy-audio-source source))

    (aw:destroy-light *renderer* flashlight)
    (destroy-endless-floor floor)

    (aw:destroy-vec2 player-velocity)
    (aw:destroy-vec2 player-position)))


(defmethod react ((this gameplay-state) event)
  (with-slots (key-bag particles enemies player-position player-angle player-velocity
               player-sources kill-count)
      this
    (let ((event-type (aw:event-type event)))
      (case event-type
        ((:keyboard-button-down :keyboard-button-up)
         (let ((pressed-p (eq  event-type :keyboard-button-down))
               (key (aw:event-key-scan-code event)))
           (case key
             (:escape (when pressed-p
                        (transition-to 'initial-state :kill-count kill-count)))
             ((:w :a :s :d) (if pressed-p
                                (pushnew key key-bag)
                                (a:deletef key-bag key))))))
        (:mouse-button-down
         (when (eq :left (aw:event-mouse-button event))
           (push (make-particle-cloud-instance (aw:vec2 player-position 0)
                                               (aw:vec2 player-position 1)
                                               player-angle
                                               (aw:vec2 player-velocity 0)
                                               (aw:vec2 player-velocity 1))
                 particles)
           (let ((shot (aw:make-audio-source (find-asset :audio "shot"))))
             (aw:play-audio-source shot)
             (push shot player-sources))
           (loop for enemy in enemies
                 do (update-if-enemy-shot enemy))))))))


(defun spawn-enemy ()
  (with-slots (player-position enemies) *state*
    (let ((rand-x (- 1 (random 2f0)))
          (rand-y (- 1 (random 2f0))))
      (push (make-enemy (+ (+ rand-x (if (> rand-x 0) 1 -1)) (aw:vec2 player-position 0))
                        (+ (+ rand-y (if (> rand-y 0) 1 -1)) (aw:vec2 player-position 1)))
            enemies))))


(defun player-dead-p ()
  (with-slots (player-position enemies) *state*
    (aw:with-vec2 (tmp)
      (loop for enemy in enemies
              thereis (progn
                        (aw:vec2-subt tmp (enemy-position enemy) player-position)
                        (<= (aw:vec2-length tmp) 0.1))))))


(defmethod act ((this gameplay-state))
  (with-slots (floor flashlight mouse-state key-bag particles enemies started
               player-angle player-position player-velocity player-sources kill-count)
      this
    (aw:mouse-state mouse-state)

    (let* ((time-delta (- (real-time-seconds) started))
           (enemies-expected (1+ (floor (/ time-delta 10))))
           (enemies-needed (- enemies-expected (length enemies))))
      (when (> time-delta 3)
        (loop repeat enemies-needed
              do (spawn-enemy))))


    (loop for instance in particles
          do (update-particle-cloud-instance instance)
          when (> (particle-cloud-instance-life instance) 2)
            collect instance into for-removal
          finally (loop for instance in for-removal
                        do (destroy-particle-cloud-instance instance)
                           (a:deletef particles instance)))


    (aw:with-vec2* ((tmp :x 0 :y 0)
                    (mov-dir :x 0 :y 0))
      (loop for key in key-bag
            do (case key
                 (:w (incf (aw:vec2 tmp 1)))
                 (:a (decf (aw:vec2 tmp 0)))
                 (:s (decf (aw:vec2 tmp 1)))
                 (:d (incf (aw:vec2 tmp 0)))))
      (if (aw:vec2-equal tmp mov-dir)
          (setf (aw:vec2 player-velocity 0) 0
                (aw:vec2 player-velocity 1) 0)
          (progn
            (aw:vec2-normalize mov-dir tmp)
            (aw:vec2-scalar-mult player-velocity mov-dir 0.01)
            (aw:vec2-copy tmp player-position)
            (aw:vec2-add player-position tmp player-velocity))))

    (update-endless-floor floor
                          (aw:vec2 player-position 0)
                          (aw:vec2 player-position 1))

    (with-transform (transform
                     (:translation :x (aw:vec2 player-position 0)
                                   :y (+ (aw:vec2 player-position 1) -0.1)
                                   :z 2)
                     (:rotation (* (/ pi 180) 3) :x 1))
      (aw:transform-scene-camera *scene* transform))

    (aw:with-vec3 (pos :x (aw:vec2 player-position 0)
                       :y (aw:vec2 player-position 1))
      (setf (aw:audio-listener-position) pos)
      (loop for source in player-sources
            if (eq :playing (aw:audio-source-state source))
              do (setf (aw:audio-source-position source) pos)
            else
              collect source into stopped-sources
            finally (loop for stopped-source in stopped-sources
                          do (aw:destroy-audio-source stopped-source)
                             (a:deletef player-sources stopped-source))))

    (loop for enemy in enemies
          unless (update-enemy enemy)
            do (incf kill-count) and collect enemy into dead-enemies
          finally (loop for dead-enemy in dead-enemies
                        do (a:deletef enemies dead-enemy)
                           (destroy-enemy dead-enemy)))

    (aw:with-vec2 (mouse-vec :x (- (aw:mouse-state-x mouse-state) (/ *width* 2))
                             :y (- (- *height* (aw:mouse-state-y mouse-state)) (/ *height* 2)))
      (setf player-angle (calc-angle mouse-vec)))

    (with-transform (root-transform
                     (:translation :x (aw:vec2 player-position 0)
                                   :y (aw:vec2 player-position 1))
                     (:rotation player-angle :z 1))
      (with-transform (transform
                       (:transform root-transform)
                       (:translation :z 0.05)
                       (:rotation  (* (/ pi 180) -80) :y 1))
        (aw:transform-entity *renderer* flashlight transform)))

    (when (player-dead-p)
      (transition-to 'end-state :kill-count kill-count))))


(defmethod draw ((this gameplay-state))
  (with-slots (kill-count started) this
    (menu-paint-color)

    (aw:with-font (*menu-typeface*)
      (aw:font-size 26)
      (aw:with-saved-transform ()
        (aw:translate (- *width* 120) 40)
        (aw:text 0 0 "~3,'0D" kill-count))

      (aw:with-saved-transform ()
        (let* ((play-time-seconds (- (real-time-seconds) started))
               (minutes (floor (/ play-time-seconds 60)))
               (seconds (floor (mod play-time-seconds 60))))
          (aw:translate 14 40)
          (aw:text 0 0 "~1,'0D:~2,'0D" minutes seconds))))))
