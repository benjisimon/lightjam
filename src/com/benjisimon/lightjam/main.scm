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
(define-alias Date java.util.Date)

(define (logi . entries)
  (for-each (lambda (e) 
              (android.util.Log:i "com.benjisimon.lightjam" e))
            entries))



(define (note)
  (define (amplitude i)
    (inexact->exact (floor (* (sin (/ (* 2 Math:PI i)
                                      (/ 8000
                                         440)))
                              32767)))) 
  (define (byteify x)
    (let ((lower (bitwise-and x #x00FF))
          (upper (bitwise-arithmetic-shift-right (bitwise-and x #xFF00) 8)))
      ((as ByteVector (bytevector lower upper)):get-buffer)))

  (let ((now ((Date):getTime)))
    (byteify (amplitude (modulo now 100)))))


(define (stringify x)
  (let ((out (open-output-string)))
    (display x out)
    (get-output-string out)))




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
                                     20
                                     AudioTrack:MODE_STREAM))
  (set! (this):sensor-mgr (as SensorManager ((this):getSystemService "sensor")))

  ((as AudioTrack (this):track):play)
  (this):feedback)

 ((on-resume)
  (invoke-special android.app.Activity (this) 'onResume)
  (let* ((light-sensor ((this):sensor-mgr:getDefaultSensor Sensor:TYPE_LIGHT))
         (feedback (this):feedback)
         (track (this):track))
     (set! (this):handler 
           (object (SensorEventListener)
                   ((on-accuracy-changed sensor accuracy) #!void)
                   ((on-sensor-changed (evt :: SensorEvent))
                    (let* ((light-level (evt:values 0))
                           (note        (note)))
                      (feedback:setText (stringify (list light-level note)))
                      (track:write note 0 1)
                      #!void))))
     ((this):sensor-mgr:register-listener (this):handler
                                   light-sensor
                                   SensorManager:SENSOR_DELAY_FASTEST)))
     

 ((on-pause)
  (invoke-special android.app.Activity (this) 'onPause)  
  (this):sensor-mgr:unregister-listener (this):handler)

 )

   
