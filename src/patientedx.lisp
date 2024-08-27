(in-package :patientedx)

(defun make-disease-prompt (disease)
  "create a prompt to get disease details."
  (format nil "You will act as a patient educator who communicates in common lisp. You will generate a summary, cause, epidemiology, signs and symptoms, compilcations, differential diagnoses of ~a. The data should be detailed, but when you don't know a thing, that will be nil. Return a plist (:summary ... :cause ... :epidemiology ... :pathology ... :signs-and-symptoms (...) :complications (...) :differential-diagnoses (...)). The data is to be used in blog posts so act accordingly. Example of desired output: (:summary \"Disease summary\" :cause \"Detailed disease cause\" :epidemiology \"Detailed disease epidemiology, qouting the latest statistics you have access too. Don't make numbers up.\" :pathology \"Detailed pathology of the disease\" :signs-and-symptoms (\"Sign 1\" ...) :complications (\"Cimplication 1\" ...) :differential-diagnosis (\"Ddx 1\" ...)) " disease))

(defun get-disease-details (disease)
  "makes a call to the best meodel we have right now, now we will be using sexp to get the data, the data will be returned as plists.
use the best model available to us, currently gpt-4o. generate a disease summary, cause, signs and symptoms, complications, differential diagnoses, effective drugs to the disease.

the data returned should be markdown, to allow embedding of links: the differential diagnoses are links to the disease page, the treatments section is also a link to the disease page. this will improve visibility if all pages of the site.

we should also return the links in a separate list. forexample: link disease to its treatment and then link each possible treatment. but let's handle diseases first. then we'll see about the treatments."
  (llms:query-azure-ai () :system-prompt (make-disease-prompt disease)))

(defun get-disease-lists ()
  "the disease lists are found on wikipedia. all we have to do is scrap them from there and then feed them to gpt-4o to create a good enough detail about each disease. we also have to clean the data."
  (multiple-value-bind (response response-code response-headers request-uri flexi-response response-bool status-text)
      (drakma:http-request "https://en.wikipedia.org/wiki/List_of_diseases_(0%E2%80%939)")
    (declare (ignore response-code response-headers request-uri flexi-response response-bool status-text))
    response))
