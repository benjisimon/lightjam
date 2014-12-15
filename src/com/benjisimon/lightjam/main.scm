;;
;; Our main app
;;

(require 'android-defs)

(define-alias Sensor android.hardware.Sensor)
(define-alias SensorManager android.hardware.SensorManager)
(define-alias SensorEventListener android.hardware.SensorEventListener)
(define-alias SensorEvent android.hardware.SensorEvent)
(define-alias AudioTrack android.media.AudioTrack)
(define-alias AudioManager android.media.AudioManager)
(define-alias AudioFormat android.media.AudioFormat)
(define-alias Math java.lang.Math)

(activity main
  (on-create-view
   (let* ((sensor-mgr (as SensorManager ((this):getSystemService "sensor")))
          (light-sensor (sensor-mgr:getDefaultSensor Sensor:TYPE_LIGHT))
          (feedback (android.widget.TextView (this) text: "--"))
          (track (AudioTrack AudioManager:STREAM_MUSIC
                             8000
                             AudioFormat:CHANNEL_CONFIGURATION_MONO
                             AudioFormat:ENCODING_PCM_16BIT
                             4
                             AudioTrack:MODE_STREAM)))
                            
     (sensor-mgr:register-listener
      (object (SensorEventListener)
              ((on-accuracy-changed sensor accuracy) #!void)
              ((on-sensor-changed (evt :: SensorEvent))

               (define (tone light-level)
                 (define (s i)
                   (sin (/ (* 2 Math:PI i)
                           8000
                           100)))
                 (let ((samples (list (s 0)
                                      (s 1)
                                      (s 2)
                                      (s 3)))
                       (buffer (make byte[] length: 8))
                       (i 0))
                   (for-each (lambda (s)
                               (set! (buffer i) (bitwise-and s #x00FF))
                               (set! i (+ 1 i))
                               (set! (buffer i) (bitwise-arithmetic-shift-right (bitwise-and s #xFF00) 8))
                               (set! i (+ 1 i)))
                             samples)
                   (track:write buffer 0 4)))

               (let ((light-level (evt:values 0)))
                 (tone light-level)
                 (feedback:setText (number->string light-level))
                 #!void)))
      light-sensor
      SensorManager:SENSOR_DELAY_FASTEST)
     
     (track:play)
      
     feedback)))

   
