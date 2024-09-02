(defsystem "curaedu"
  :author "Ninx technology limited"
  :description "a simple friendly patient education application."
  :depends-on (:cl-redis :str :com.inuoe.jzon :hunchentoot :hunchensocket :drakma :easy-routes :cl-who :cl-css :cl-base64 :frugal-uuid :trivia :flexi-streams :parenscript :local-time)
  :components ((:module "src"
		:components ((:file "packages")
			     (:file "curaedu.nlp")
			     (:file "curaedu.sexp")
			     (:file "curaedu.apis")
			     (:file "curaedu.kvrocks")
			     (:file "curaedu" :depends-on ("curaedu.apis" "curaedu.nlp"))
			     (:file "curaedu.app")))
	       )
  :build-operation "program-op" ;; leave as is
  :build-pathname "curaedu"
  :entry-point "curaedu:start-server"
  )
