;;
;; Our main app
;;

(require 'android-defs)

(define-alias Sensor android.hardware.Sensor)
(define-alias SensorManager android.hardware.SensorManager)
(define-alias SensorEventListener android.hardware.SensorEventListener)
(define-alias SensorEvent android.hardware.SensorEvent)

(activity main
  (on-create-view
   (let* ((sensor-mgr (as SensorManager ((this):getSystemService "sensor")))
          (light-sensor (sensor-mgr:getDefaultSensor Sensor:TYPE_LIGHT))
          (feedback (android.widget.TextView (this) text: "--")))

     (sensor-mgr:register-listener
      (object (SensorEventListener)
              ((on-accuracy-changed sensor accuracy) #!void)
              ((on-sensor-changed (evt :: SensorEvent))
               (feedback:setText (number->string (evt:values 0)))
               #!void))
      light-sensor
      SensorManager:SENSOR_DELAY_FASTEST)
      
     feedback)))

   
