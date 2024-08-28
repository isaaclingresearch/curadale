(in-package :patientedu)

(defun make-disease-prompt (disease)
  "create a prompt to get disease details."
  (format nil "You will act as a patient educator who communicates in common lisp. You will generate a summary, cause, epidemiology, signs and symptoms, compilcations, differential diagnoses of ~a. The data should be detailed, but when you don't know a thing, that will be nil. Return a plist (:introduction ... :cause ... :risk-factors (...) :epidemiology ... :diagnosis (...) :pathophysiology ... :signs-and-symptoms (...) :complications (...) :differential-diagnoses (...) :alternative-names (...) :prevention () :living-with (...)). The data is to be used in blog posts so act accordingly. 

Example of desired output: (:introduction \"Introduce the disease\" :cause \"Detailed disease cause\" :epidemiology \"Detailed disease epidemiology, qouting the latest statistics you have access too. Don't make numbers up.\" :risk-factors (\"Risk factor 1\" ...) :diagnosis (\"How disease is diagnosed 1\" ...) :pathophysiology \"A few details about how the process occurs\" :signs-and-symptoms (\"Sign 1\" ...) :complications (\"Complication with a brief explanation of how it comes about\" ...) :differential-diagnosis (\"Ddx 1\" ...) :alternative-names (\"Other name of disease 1\" ...) :prevention (\"Way 1\" ...) :living-with (\"Way 1\" ...))

If you don't have any data about the disease: return nil (as in common lisp's nil). Do not make up any data as people's lives are at stake.
" disease))

(defun get-disease-details (disease)
  "makes a call to the best meodel we have right now, now we will be using sexp to get the data, the data will be returned as plists.
use the best model available to us, currently gpt-4o. generate a disease summary, cause, signs and symptoms, complications, differential diagnoses, effective drugs to the disease.

the data returned should be markdown, to allow embedding of links: the differential diagnoses are links to the disease page, the treatments section is also a link to the disease page. this will improve visibility if all pages of the site.

we should also return the links in a separate list. forexample: link disease to its treatment and then link each possible treatment. but let's handle diseases first. then we'll see about the treatments.

when the model returns non plist data, repeat until it returns the correct data. also do so when the model returns an error."
  (trivia:match (llms:query-azure-ai () :system-prompt (make-disease-prompt disease))
    ((list :error _) (get-disease-details disease))
    ((list data _ _ _)
     (let ((sexp-data (sexp:parse (nlp:remove-lisp-encapsulation data))))
       (when sexp-data
	 (if (sexp:plistp sexp-data)
	     (save-disease-data disease sexp-data)
	     (get-disease-details disease)))))))
