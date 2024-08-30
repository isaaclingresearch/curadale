(in-package :patientedu)

;;; HTTP(S) 
(setq *show-lisp-errors-p* t) ;; set this to show error files in /priv/errors

;; define server config
;;;; these are set in $HOME/.bashrc to be accessible in the sbcl repl 
(defvar *patientedu-http-port* (parse-integer (uiop:getenv "PATIENTEDU_HTTP_PORT")))
(defvar *patientedu-https-port* (parse-integer (uiop:getenv "PATIENTEDU_HTTPS_PORT")))
(defvar *patientedu-ssl-cert* (uiop:getenv "PATIENTEDU_SSL_CERT"))
(defvar *patientedu-ssl-key* (uiop:getenv "PATIENTEDU_SSL_KEY"))


;; WEBSOCKET SERVER AND FUNCTIONS
(defclass ws-endpoint (hunchensocket:websocket-resource)
  ((name :initarg :name :initform (error "Name this server") :reader :name :accessor name))
  (:default-initargs :client-class 'ws-user)
  )

(defclass ws-user (hunchensocket:websocket-client)
  ((name :initarg :user-agent :reader name :initform (error "Name this user!") :accessor name)))

(defvar *ws-endpoints* (list (make-instance 'ws-endpoint :name "/ws")))

(defun find-ws-endpoint (request)
  (find (hunchentoot:script-name request) *ws-endpoints* :test #'string= :key #'name))

(pushnew 'find-ws-endpoint hunchensocket:*websocket-dispatch-table*)


;; we need to use easy-routes over websockets, so we will create children of both
(defclass ws-routes-acceptor (easy-routes-acceptor acceptor-websocket)
  ()
  (:documentation "a subclass of routes-acceptor and hunchentsocket"))

(defclass ws-routes-ssl-acceptor (easy-routes-ssl-acceptor websocket-ssl-acceptor)
  ()
  (:documentation "routes and websockets over ssl"))


;; redirect all traffic to https
(defclass http-to-https-acceptor (hunchentoot:acceptor) ())
(defmethod hunchentoot:acceptor-dispatch-request ((acceptor http-to-https-acceptor) request)
  (hunchentoot:redirect (hunchentoot:request-uri request)
                        :protocol :https :port *patientedu-https-port*))

(defvar *patientedu-wss-acceptor* (make-instance 'ws-routes-ssl-acceptor :port *patientedu-https-port*
									 :ssl-certificate-file *patientedu-ssl-cert*
									 :ssl-privatekey-file *patientedu-ssl-key*
									 :document-root (truename "~/common-lisp/patientedu/priv/")
									 :error-template-directory (truename "~/common-lisp/patientedu/priv/errors/")))

(defvar *patientedu-http-acceptor* (make-instance 'http-to-https-acceptor :port *patientedu-http-port*))

;; set logging to files
					;(setf (acceptor-message-log-destination *patientedu-wss-acceptor*) (truename "~/common-lisp/patientedu/logs/message.log"))
					;(setf (acceptor-access-log-destination *patientedu-wss-acceptor*) (truename "~/common-lisp/patientedu/logs/access.log"))
;; don't allow persistent connections
;; this is because the server was not responding to requests, with a 503, and the error logs were showing too many threads.
;; still investigation, but maybe the connections were sending a keep alive header.
(setf (acceptor-persistent-connections-p *patientedu-http-acceptor*) nil)
(setf (acceptor-persistent-connections-p *patientedu-wss-acceptor*) nil)

;; after reviewing the taskmaster section of the docs, either of two things happened, because i was having one active connections
;; 1). the connections persisted, I don't why that is, but i have stopped persistent connections.
;; 2). The taskmaster ran out of threads, or the max accept was exceeded by the active requests.
;; 3). this is the solution, stop persistent connections above, then increase the threads to 1000, and max accept to 1500.

(let ((http-taskmaster (slot-value *patientedu-http-acceptor* 'taskmaster))
      (https-taskmaster (slot-value *patientedu-wss-acceptor* 'taskmaster)))
  (setf (slot-value http-taskmaster 'hunchentoot::max-thread-count) 10000)
  (setf (slot-value http-taskmaster 'hunchentoot::max-accept-count) 15000)
  (setf (slot-value https-taskmaster 'hunchentoot::max-thread-count) 10000)
  (setf (slot-value https-taskmaster 'hunchentoot::max-accept-count) 15000))

(defun start-server ()
  "Start the server"
  (stop-server)
  (start-kvrocks)
  (hunchentoot:start *patientedu-http-acceptor*)
  (hunchentoot:start *patientedu-wss-acceptor*)
  )

(defun stop-server ()
  "Stop the server"
  (when (started-p *patientedu-http-acceptor*)
    (stop *patientedu-http-acceptor*))
  (when (started-p *patientedu-wss-acceptor*)
    (stop *patientedu-wss-acceptor*))
  (handler-case (stop-kvrocks)
    (redis:redis-connection-error (err)
      (declare (ignore err)))))

(defun restart-server ()
  (stop-server)
  (start-server))

;; websocket methods to handle communication via websocket
(defmethod hunchensocket:client-connected ((endpoint ws-endpoint) ws-user))

(defmethod hunchensocket:text-message-received ((endpoint ws-endpoint) ws-user message-json)
  (let* ((message (jzon:parse message-json))
	 (message-type (gethash "type" message nil)))
    (trivia:match message-type
      ("auto-complete"
       (let ((auto-words (or (get-autocomplete (gethash "fragment" message)) #())))
	 (hunchensocket:send-text-message ws-user  (jzon:stringify auto-words)))))))


(defun ws-js-code ()
  "generate the code for websockets and handling of the back and forth between client and server"
  (parenscript:ps
    (setf *socket* (new (-web-socket "/ws")))
    (setf (chain *socket* onopen) (lambda () ((chain console log) "connected to server")))
    (setf (chain *socket* onmessage) (lambda (event) (display-suggestions ((chain -j-s-o-n parse) (chain event data)))))
    (setf (chain *socket* onclose) (lambda () ((chain console log) "socket closed!")))
    (setf (chain *socket* onerror) (lambda (err) ((chain console log) err)))
    ;; capture user input
    (defvar *input* (chain document (get-element-by-id "autocomplete-input")))
    (chain *input* (add-event-listener "input" (lambda ()
						 (let ((query (chain this value)))
						   (if (> (chain query length) 0)
						       (chain *socket* (send (chain -j-s-o-n (stringify (create fragment query
														type "auto-complete")))))
						       (clear-suggestions))))))
    (defun clear-suggestions ()
      (setf (chain document (get-element-by-id "suggestions") inner-h-t-m-l) ""))
    (defun display-suggestions (suggestions)
      (let ((suggestions-container ((chain document get-element-by-id) "suggestions")))
	(setf (chain suggestions-container inner-h-t-m-l) "")
	(chain suggestions (for-each (lambda (suggestion)
				       (let ((suggestion-div (chain document (create-element "div"))))
					 (setf (getprop suggestion-div 'class-name) "autocomplete-suggestion")
					 (setf (getprop suggestion-div 'text-content) suggestion)
					 (chain suggestion-div (add-event-listener "click" (lambda ()
											     (setf (getprop *input* 'value) suggestion)
											     (clear-suggestions))))
					 (chain suggestions-container (append-child suggestion-div))))))))))

(defroute index-page ("/" :method :get :decorators ()) ()
  (with-html-output-to-string (*standard-output*)
    (:html
     (:head
      (:title "PatientEdu")
      (:meta :charset "UTF-8")
      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
      (:link :rel "icon" :href "/static/icons/web/favicon.ico" :sizes "any")
      (:link :rel "apple-touch-icon" :href "/static/icons/web/apple-touch-icon.png")
      ;; Include CSS with background colors, borders, shadows, and mobile adjustments
      (:style
       (str (cl-css:css
             '((body :font-family "Arial, sans-serif" :margin "0" :padding "0" :display "flex" :flex-direction "column" :justify-content "center" :align-items "center" :min-height "100vh" :background "linear-gradient(to bottom, #f0f0f0, #e0e0e0)")
               (".container" :text-align "center" :width "90%" :max-width "600px" :display "flex" :flex-direction "column" :align-items "center" :justify-content "center" :flex "1")
               (".logo" :font-size "36px" :font-weight "bold" :margin-bottom "20px" :color "#0044cc" :padding "10px" :background-color "#e6f0ff" :border-radius "8px")
	       (".logo a" :decoration none)
	       (".logo a:visited" :color "#0044cc")
               (".search-form" :margin-top "20px" :display "flex" :flex-direction "column" :align-items "center" :position "relative")
               (".search-input" :width "calc(100% - 40px)" :padding "10px" :font-size "16px" :border "1px solid #ccc" :border-radius "4px" :margin-bottom "5px" :box-shadow "0 2px 5px rgba(0,0,0,0.1)")
               (".autocomplete-suggestions" :border "1px solid #ccc" :max-height "150px" :overflow-y "auto" :position "absolute" :background-color "white" :z-index "1000" :width "calc(100% - 40px)" :left "0" :box-shadow "0 2px 5px rgba(0,0,0,0.1)" :margin-left 19)
               (".autocomplete-suggestion" :padding "10px" :cursor "pointer" :border-bottom "1px solid #ddd")
               (".autocomplete-suggestion:hover" :background-color "#f0f0f0")
               (".search-button" :padding "10px 20px" :font-size "16px" :color "white" :background-color "#28a745" :border "none" :border-radius "4px" :cursor "pointer" :box-shadow "0 2px 5px rgba(0,0,0,0.2)" :margin-top "5px")
               (".search-button:hover" :background-color "#218838" :transform "scale(1.05)" :transition "transform 0.2s")
               (".footer" :margin-top "auto" :padding "10px 0" :text-align "center" :width "100%" :background-color "#0044cc" :color "white")
               (".footer a" :color "white" :text-decoration "none" :margin "0 10px")
               (".footer a:hover" :text-decoration "underline")
               ;; Mobile adjustments
               ("@media (max-width: 600px)"
                (".logo" :font-size "30px")
                (".search-input" :width "90%")
                (".search-button" :width "90%")
                (".autocomplete-suggestions" :width "90%")))))) ;; Responsive adjustments
      (:body
       (:div :class "container"
	     (:div :class "logo" (:a :href "/" "PatientEdu"))
	     (:div :class "search-form"
		   (:form :action "/search" :method "get"
			  (:input :type "text" :name "query" :id "autocomplete-input" :placeholder "Search..." :class "search-input" :autocomplete "off" :required t)
			  ;; Move the suggestions div directly below the input field
			  (:div :id "suggestions" :class "autocomplete-suggestions")
			  (:button :type "submit" :class "search-button" "Search"))))
       (:script (str (ws-js-code)))
       ;; Footer Section
       (:div :class "footer"
	     (:a :href "/about" "About")
	     (:a :href "/privacy" "Privacy Policy")))))))

(defroute search-page ("/search" :method :get :decorators ()) (query)
  (with-html-output-to-string (*standard-output*)
    (:html
     (:head
      (:title "Search | PatientEdu")
      (:meta :charset "UTF-8")
      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
      (:link :rel "icon" :href "/static/icons/web/favicon.ico" :sizes "any")
      (:link :rel "apple-touch-icon" :href "/static/icons/web/apple-touch-icon.png")
      ;; Include CSS
      (:style
       (str (cl-css:css
	     '((body :font-family "Arial, sans-serif" :margin "0" :padding "0" :display "flex" :flex-direction "column" :justify-content "center" :align-items "center" :min-height "100vh" :background "linear-gradient(to bottom, #f0f0f0, #e0e0e0)")
	       (".container" :text-align "center" :width "90%" :max-width "600px" :display "flex" :flex-direction "column" :align-items "center" :justify-content "center" :flex "1")
	       (".logo" :font-size "36px" :font-weight "bold" :margin-bottom "20px" :color "#0044cc" :padding "10px" :background-color "#e6f0ff" :border-radius "8px")
	       (".logo a" :decoration none)
	       (".logo a:visited" :color "#0044cc")

	       (".footer" :margin-top "auto" :padding "10px 0" :text-align "center" :width "100%" :background-color "#0044cc" :color "white")
	       (".footer a" :color "white" :text-decoration "none" :margin "0 10px")
	       (".footer a:hover" :text-decoration "underline")
	       (".search-result" :margin "10px 0" :width "100%" :text-align "center")
	       (".search-result a" :display "block" :padding "15px" :margin "5px 0" :background-color "#f5f5f5" :color "black" :border-radius "5px" :text-decoration "none" :text-align "left")
	       (".search-result a:hover" :background-color "#e0e0e0")
	       (".search-result a:visited" :color "#800080") ;; Visited link color (purple)
	       (".search-result .heading" :font-weight "bold" :margin-bottom "5px" :font-size "18px" :color "blue")
	       (".search-result .description" :font-size "14px" :color "#666666")
	       ;; Mobile adjustments
	       ("@media (max-width: 600px)"
		(".logo" :font-size "30px")
		(".search-result a" :padding "10px")
		(".search-result .description" :font-size "16px" :color "#666666")))))))
     (:body
      (:div :class "container"
	    (:div :class "logo" (:a :href "/" "PatientEdu"))
	    ;; Search Results Section
	    
	    (loop for result in (build-links query) do
	      (htm (:div :class "search-result"
			 (:a :href (str (format nil "/disease/~a" (make-url (car result))))
	                     (:div :class "heading" (str (car result)))
	                     (:div :class "description" (str (cdr result)) "..."))))))
      ;; Footer Section
      (:div :class "footer"
	    (:a :href "/about" "About")
	    (:a :href "/privacy" "Privacy Policy"))))))

(defun build-links (query)
  "given a query, find all possible data that corresponds to it and return a list of (title . description)"
  (let* (acc
	 sacc
	 (tokens (nlp:tokenize query))
	 (h (make-hash-table :test #'equal)))
    (dolist (token tokens)
      (setq acc `(,@acc ,@(search-disease-data token))))
    (dolist (token acc)
      (setf (gethash token h) (+ (gethash token h 0) 1)))
    (maphash (lambda (k v)
	       (setq sacc `(,@sacc (,k . ,v)))) h)
    (mapcar (lambda (k) (cons (disease-hget k "name")
			      (disease-hget k "introduction")))
	    (mapcar #'car (sort sacc #'> :key #'cdr)))))

(defun make-url (text)
  "given a text, replace spaces with -, then make lowercase"
  (str:downcase (str:replace-all " " "-" text)))


(defroute disease-page ("/disease/:url" :method :get :decorators ()) ()
  (let* ((id (get-id-from-url url))
	 (proper-name (disease-hget id "name"))
	 (intro (disease-hget id "introduction"))
	 (cause (disease-hget id "cause"))
	 (epidemiology (disease-hget id "epidemiology"))
	 (risk-factors (sexp:parse (disease-hget id "risk-factors")))
	 (diagnosis (sexp:parse (disease-hget id "diagnosis")))
	 (differential-diagnoses (sexp:parse (disease-hget id "differential-diagnoses")))
	 (pathophysiology (disease-hget id "pathophysiology"))
	 (signs-and-symptoms (sexp:parse (disease-hget id "signs-and-symptoms")))
	 (complications (sexp:parse (disease-hget id "complications")))
	 (alternative-names (sexp:parse (disease-hget id "alternative-names")))
	 (prevention (sexp:parse (disease-hget id "prevention")))
	 (living-with (sexp:parse (disease-hget id "living-with"))))
    (format nil "~% ~a ~%" risk-factors)
    (princ id)
    (with-html-output-to-string (*standard-output*)
      (:html
       (:head
	(:title (format nil "~a | PatientEdu" proper-name))
	(:meta :charset "UTF-8")
	(:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
	(:link :rel "icon" :href "/static/icons/web/favicon.ico" :sizes "any")
	(:link :rel "apple-touch-icon" :href "/static/icons/web/apple-touch-icon.png")
	;; Include CSS
	(:style
	 (str (cl-css:css
	       '((body :font-family "Arial, sans-serif" :margin "0" :padding "0" :display "flex" :flex-direction "column" :justify-content "center" :align-items "center" :min-height "100vh" :background "linear-gradient(to bottom, #f0f0f0, #e0e0e0)" :font-size "18px")
		 (".container" :text-align "left" :width "90%" :max-width "800px" :display "flex" :flex-direction "column" :align-items "flex-start" :justify-content "center" :flex "1")
		 (".logo" :font-size "40px" :font-weight "bold" :margin-bottom "20px" :color "#0044cc" :padding "10px" :background-color "#e6f0ff" :border-radius "8px")
		 (".logo a" :decoration none)
		 (".logo a:visited" :color "#0044cc")

		 (".section" :margin-bottom "20px")
		 (".section h2" :font-size "26px" :color "#0044cc" :margin-bottom "10px" :border-bottom "2px solid #0044cc" :padding-bottom "5px")
		 (".section p" :font-size "18px" :line-height "1.6" :color "#333333")
		 (".section ul" :list-style-type "disc" :padding-left "20px")
		 (".section ul li" :font-size "18px" :line-height "1.6" :color "#333333")
		 (".footer" :margin-top "auto" :padding "10px 0" :text-align "center" :width "100%" :background-color "#0044cc" :color "white")
		 (".footer a" :color "white" :text-decoration "none" :margin "0 10px")
		 (".footer a:hover" :text-decoration "underline")
		 ;; Mobile adjustments
		 ("@media (max-width: 600px)"
		  (".logo" :font-size "34px")
		  (".section h2" :font-size "22px")
		  (".section p" :font-size "16px")
		  (".section ul li" :font-size "16px")))

	       ))))
       (:body
	(:div :class "container"
              (:div :class "logo" (:a :href "/" "PatientEdu"))
              ;; Disease Information Sections
              (:div :class "section"
                    (:h2 (str proper-name))
                    (:p (str intro)))
              (:div :class "section"
                    (:h2 "Cause")
                    (:p (str cause)))
              (:div :class "section"
                    (:h2 "Epidemiology")
                    (:p (str epidemiology)))
              (:div :class "section"
                    (:h2 "Risk Factors")
                    (:ul (dolist (r risk-factors)
			   (htm (:ul (str r))))))
	      (:div :class "section"
                    (:h2 "Pathophysiology")
                    (:p (str pathophysiology)))
	      (:div :class "section"
                    (:h2 "Diagnosis")
                    (:ul (dolist (dd diagnosis)
			   (htm (:ul (str dd))))))
              (:div :class "section"
                    (:h2 "Differential Diagnoses")
                    (:ul (dolist (dd differential-diagnoses)
			   (htm (:ul (str dd))))))
              
              (:div :class "section"
                    (:h2 "Signs and Symptoms")
                    (:ul (dolist (ss signs-and-symptoms)
			   (htm (:ul (str ss))))))
              (:div :class "section"
                    (:h2 "Complications")
		    (:ul (dolist (c complications)
			   (htm (:ul (str c))))))
              (:div :class "section"
                    (:h2 "Alternative Names")
                    (:ul (dolist (an alternative-names)
			   (htm (:ul (str an))))))
              (:div :class "section"
                    (:h2 "Prevention")
                    (:ul (dolist (p prevention)
			   (htm (:ul (str p))))))
              (:div :class "section"
                    (:h2 (str (format nil "Living with ~a" proper-name)))
                    (:ul (dolist (lw living-with)
			   (htm (:ul (str lw))))))
	      )
	;; Footer Section
	(:div :class "footer"
              (:a :href "/about" "About")
              (:a :href "/privacy" "Privacy Policy")))))))
