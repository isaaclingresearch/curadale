(defpackage :palpul.sexp
  (:use :cl)
  (:nicknames :sexp)
  (:documentation "Processes strings and sexps")
  (:export :parse :stringify :ensure-plist :plistp))

(defpackage :curaedu.nlp
  (:use :cl :str :trivia)
  (:nicknames :nlp)
  (:shadow str:match)
  (:documentation "This package for processing text")
  (:export :remove-json-encapsulation :remove-lisp-encapsulation :make-strict-json :tokenize :count-terms :compute-tf))

(defpackage :curaedu.apis
  (:use :cl :drakma :com.inuoe.jzon)
  (:nicknames :llms)
  (:documentation "This package for making rest api calls to llm endpoints")
  (:local-nicknames (:jzon :com.inuoe.jzon))
  (:export :make-gemini-multiturns
   :query-gemini
	   :query-together
	   :query-groq
   :query-ollama
	   :query-azure-ai
	   *index-prompt*
	   *system-prompt*
	   *sexp-index-prompt*
	   *sexp-title-prompt*
	   *json-chat-prompt*
	   *default-together-model*
	   *qwen-system-prompt*))

(defpackage :curaedu
  (:use :cl :redis :easy-routes :hunchentoot :hunchensocket :cl-who :cl-css :cl-base64 :frugal-uuid :trivia :com.inuoe.jzon :parenscript :local-time)
  (:shadow easy-routes:redirect hunchentoot:reply parenscript:@ parenscript:stringify parenscript:% redis:close-connection cl-who:fmt redis:tell str:match)
  (:documentation "The main package of the curaedu application.")
  (:local-nicknames (:jzon :com.inuoe.jzon))
  (:export :start-server :restart-server :create-tables))
