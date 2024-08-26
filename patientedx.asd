(defsystem "patientedx"
  :author "Ninx technology limited"
  :description "a simple friendly patient education application."
  :depends-on (:cl-redis :str :com.inuoe.jzon :hunchentoot :drakma :easy-routes :cl-who :cl-css :cl-base64 :frugal-uuid :trivia :flexi-streams :parenscript :local-time)
  :components ((:module "src"
		:components ((:file "packages")
			     (:file "patientedx.nlp")
			     (:file "patientedx.apis")
			     (:file "patientedx.kvrocks")
			     (:file "patientedx" :depends-on ("patientedx.apis" "patientedx.nlp"))))
	       )
  :build-operation "program-op" ;; leave as is
  :build-pathname "patientedx"
  :entry-point "patientedx:start-server"
  )
