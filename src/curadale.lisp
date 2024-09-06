(in-package :curadale)

(defun make-disease-prompt (disease)
  "create a prompt to get disease details."
  (format nil "You will act as a patient educator who communicates in common lisp. You will generate a summary, cause, epidemiology, signs and symptoms, compilcations, differential diagnoses of ~a. The data should be detailed, but when you don't know a thing, that will be nil. Return a plist (:proper-name ... :introduction ... :cause ... :risk-factors (...) :epidemiology ... :diagnosis (...) :pathophysiology ... :signs-and-symptoms (...) :complications (...) :differential-diagnoses (...) :alternative-names (...) :prevention () :living-with (...)). The data is to be used in blog posts so act accordingly. 

Example of desired output: (:proper-name \"Some times the disease name given is not well organised, this is the organised name\" :introduction \"Introduce the disease\" :cause \"Detailed disease cause\" :epidemiology \"Detailed disease epidemiology, qouting the latest statistics you have access too. Don't make numbers up.\" :risk-factors (\"Risk factor 1\" ...) :diagnosis (\"How disease is diagnosed 1\" ...) :pathophysiology \"A few details about how the process occurs\" :signs-and-symptoms (\"Sign 1 with a few details of what it is or the patient feels\" ...) :complications (\"Complication with a brief explanation of how it comes about\" ...) :differential-diagnosis (\"Disease with similar signs and symptoms 1; just the name please, don't give any additional information\" ...) :alternative-names (\"Other name of disease 1\" ...) :prevention (\"Way 1\" ...) :living-with (\"Way 1\" ...))

If you don't have any data about the disease: return nil (as in common lisp's nil). Do not make up any data as people's lives are at stake.
" disease))

(defun get-disease-details (disease &key force)
  "when force is true, the old disease data is overwritten, otherwise preserved, makes a call to the best meodel we have right now, now we will be using sexp to get the data, the data will be returned as plists.
use the best model available to us, currently gpt-4o. generate a disease summary, cause, signs and symptoms, complications, differential diagnoses, effective drugs to the disease.

the data returned should be markdown, to allow embedding of links: the differential diagnoses are links to the disease page, the treatments section is also a link to the disease page. this will improve visibility if all pages of the site.

we should also return the links in a separate list. forexample: link disease to its treatment and then link each possible treatment. but let's handle diseases first. then we'll see about the treatments.

when the model returns non plist data, repeat until it returns the correct data. also do so when the model returns an error.

when the model returns differentials, these will be the internal links, run the differentials through the get-disease-details, such that all diseases have some data to them. check if a disease has been saved before running to preserve compute.

if a disease countn't be found, then save it's url with \"not-found\" to prevent further reading. "
  (format t "~% Working on: ~a ~%" disease)
  (let ((id (get-id-from-url (make-url disease)))
	(is-in-disease-set (redis:red-sismember "{disease-set}" disease)))
    (if (or (and (null id) (null is-in-disease-set)) ; no id, not in disease set.
	    (and force (or id is-in-disease-set)) ; either id or in disease-set
	    )
	(trivia:match (llms:query-azure-ai () :system-prompt (make-disease-prompt disease))
	  ((list :error _) (get-disease-details disease))
	  ((list data _ _ _)
	   (let ((sexp-data (handler-case (sexp:parse (nlp:remove-lisp-encapsulation data))
			      (error (err)
				(declare (ignore err))))))
	     (if sexp-data
		 (if (sexp:plistp sexp-data)
		     (progn
		       (redis:red-sadd "{disease-set}" disease )
		       (save-disease-data disease sexp-data)
		       (let ((differentials (getf sexp-data :differential-diagnoses nil)))
			 (when differentials
			   (dolist (differential differentials)
			     (unless (get-id-from-url (make-url differential))
			       (get-disease-details differential))))))
		     (get-disease-details disease))
		 (progn
		   (format t "~% failed to get data for: ~a ~%" disease)
		   (redis:red-sadd "{disease-set}" disease))))))
	(format t "~% ~a already saved.~%" disease))))

(defun read-disease-data ()
  "get the diseases from the diseases file into a list."
  (with-open-file (file #p"~/common-lisp/curadale/data/diseases.txt")
    (loop for line = (read-line file nil)
	  while line
	  do (get-disease-details line))))

(defun create-site-map ()
  "get all disease urls and create a sitemap with them."
  (let* ((host (uiop:getenv "CURADALE_HOST"))
	 (urls `("https://curadale.com"
		 "https://curadale.com/privacy"
		 "https://curadale.com/about"
		 ,@(mapcar (lambda (url)
			     (format nil "https://~a/disease/~a" host (cadr (str:split ":" url))))
			   (redis:red-keys "{url}*")))))
    (generate-sitemap urls)))

(defun generate-sitemap (urls)
  "Generate a sitemap.xml from a list of URLs and save it to the specified output-file."
  (with-open-file (stream (truename #p"~/common-lisp/curadale/priv/sitemap.xml")
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
    (format stream "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">~%")
    (dolist (url urls)
      (format stream "  <url>~%")
      (format stream "    <loc>~a</loc>~%"
              (escape-xml url))
      (format stream "    <lastmod>~a</lastmod>~%" (make-date))
      (format stream "  </url>~%"))
    (format stream "</urlset>~%")))

(defun escape-xml (str)
  "Escape XML special characters in STR."
  (let ((escaped-str (copy-seq str)))
    (setf escaped-str (cl-ppcre:regex-replace-all "&" escaped-str "&amp;"))
    (setf escaped-str (cl-ppcre:regex-replace-all "<" escaped-str "&lt;"))
    (setf escaped-str (cl-ppcre:regex-replace-all ">" escaped-str "&gt;"))
    (setf escaped-str (cl-ppcre:regex-replace-all "\"" escaped-str "&quot;"))
    (setf escaped-str (cl-ppcre:regex-replace-all "'" escaped-str "&apos;"))
    escaped-str))

