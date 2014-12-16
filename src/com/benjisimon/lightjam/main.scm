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
(define-alias ByteVector gnu.lists.ByteVector)

(define (logi . entries)
  (for-each (lambda (e) 
              (android.util.Log:i "com.benjisimon.lightjam" e))
            entries))

(activity 
 main
 (feedback       :: TextView)
 (handler        :: SensorEventListener)
 (track          :: AudioTrack)
 (sensor-mgr     :: SensorManager)

 (on-create-view
  (set! (this):feedback  (TextView (this) text: "--"))
  (set! (this):track     (AudioTrack AudioManager:STREAM_MUSIC
                                     8000
                                     AudioFormat:CHANNEL_CONFIGURATION_MONO
                                     AudioFormat:ENCODING_PCM_16BIT
                                     4
                                     AudioTrack:MODE_STREAM))
  (set! (this):sensor-mgr (as SensorManager ((this):getSystemService "sensor")))

  ((as AudioTrack (this):track):play)
  (this):feedback)

 ((on-resume)
  (invoke-special android.app.Activity (this) 'onResume)
  (let* ((light-sensor ((this):sensor-mgr:getDefaultSensor Sensor:TYPE_LIGHT))
         (feedback (this):feedback))
     (set! (this):handler 
           (object (SensorEventListener)
                   ((on-accuracy-changed sensor accuracy) #!void)
                   ((on-sensor-changed (evt :: SensorEvent))
                    (let* ((light-level (evt:values 0)))
                      (feedback:setText (string-append (number->string light-level)))
                      #!void))))
     ((this):sensor-mgr:register-listener (this):handler
                                   light-sensor
                                   SensorManager:SENSOR_DELAY_FASTEST)))
     

 ((on-pause)
  (invoke-special android.app.Activity (this) 'onPause)  
  (this):sensor-mgr:unregister-listener (this):handler)

 )

   
