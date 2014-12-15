;;
;; Our main app
;;

(require 'android-defs)
(activity main
  (on-create-view
   (android.widget.TextView (this)
    text: "Hello, Android from Kawa Scheme!")))
