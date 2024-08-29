(defsystem "patientedu"
  :author "Ninx technology limited"
  :description "a simple friendly patient education application."
  :depends-on (:cl-redis :str :com.inuoe.jzon :hunchentoot :hunchensocket :drakma :easy-routes :cl-who :cl-css :cl-base64 :frugal-uuid :trivia :flexi-streams :parenscript :local-time)
  :components ((:module "src"
		:components ((:file "packages")
			     (:file "patientedu.nlp")
			     (:file "patientedu.sexp")
			     (:file "patientedu.apis")
			     (:file "patientedu.kvrocks")
			     (:file "patientedu" :depends-on ("patientedu.apis" "patientedu.nlp"))
			     (:file "patientedu.app")))
	       )
  :build-operation "program-op" ;; leave as is
  :build-pathname "patientedu"
  :entry-point "patientedu:start-server"
  )
