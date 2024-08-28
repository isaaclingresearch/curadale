(in-package :patientedu)

(defun start-kvrocks ()
  (sb-ext:run-program (namestring (truename "~/.bin/kvrocks"))
		      (list "-c" (namestring (truename "~/common-lisp/patientedu/conf/kvrocks.conf")))
		      :wait nil)
  (sleep 3); allow some time for the program to start
  (handler-case (connect-kvrocks)
    (error (err)
      (print err))))

(defun stop-kvrocks ()
  (redis:red-shutdown)
  (sleep 3)
  (redis:disconnect))

(defun connect-kvrocks ()
  "connect to the server"
  (redis:connect :port 6666 :auth (uiop:getenv "KVROCKS_DEFAULT_PASSWORD")))

(defmacro with-kvrocks-txn ((&key namespace-token) &body body)
  "when given a namespace, switch to that namespace, else run the commands in a 'redis transaction'"
  `(progn
     (redis:red-multi)
     (when ,namespace-token
       (redis:red-auth ,namespace-token))
     ,@body
     (when ,namespace-token
       (redis:red-auth ,(uiop:getenv "KVROCKS_DEFAULT_PASSWORD")))
     (redis:red-exec)))

(defun make-date ()
  "return date as YYYY-MM-DD"
  (car (str:split "T" (format nil "~a" (local-time:today)))))

(defun save-disease-data (disease data)
  "save data returned from the api, data must be a valid plist with keys :introduction :cause :epidemiology :risk-factors :differential-diagnoses :pathophysiology :signs-and-symptoms :complications :alternative-names :prevention and :living-with

the data is saved in the following process
1. A disease id (uuid v7) is saved as key and a hash as value with fields corresponding to the fields of data.
2. All the data is tokenised, normalised and tokens stored in reverse indexes for search."
  (let* ((id (to-string (make-v7)))
	 (intro (getf data :introduction))
	 (cause (getf data :cause))
	 (epidemiology (getf data :epidemiology))
	 (risk-factors (getf data :risk-factors))
	 (differential-diagnoses (getf data :differential-diagnoses))
	 (pathophysiology (getf data :pathophysiology))
	 (signs-and-symptoms (getf data :signs-and-symptoms))
	 (complications (getf data :complications))
	 (alternative-names (getf data :alternative-names))
	 (prevention (getf data :prevention))
	 (living-with (getf data :living-with))
	 (one-string (format nil "~a ~a ~a ~a ~{~a ~} ~{~a ~} ~a ~{~a ~} ~{~a ~} ~{~a ~} ~{~a ~} ~{~a ~}" disease intro cause epidemiology risk-factors differential-diagnoses pathophysiology signs-and-symptoms complications alternative-names prevention living-with))
	 (tokens (nlp:tokenize one-string)))
    ;; save the data
    (redis:red-hset id "introduction" intro)
    (redis:red-hset id "cause" cause)
    (redis:red-hset id "epidemiology" epidemiology)
    (redis:red-hset id "risk-factors" risk-factors)
    (redis:red-hset id "differential-diagnoses" differential-diagnoses)
    (redis:red-hset id "pathophysiology" pathophysiology)
    (redis:red-hset id "signs-and-symptoms" signs-and-symptoms)
    (redis:red-hset id "complications" complications)
    (redis:red-hset id "alternative-names" alternative-names)
    (redis:red-hset id "prevention" prevention)
    (redis:red-hset id "living-with" living-with)
    (index-disease-data id disease tokens)
    id))

(defun index-disease-data (id disease tokens)
  "words in disease titles are indexed independently. then all the other tokens.

indexes are stored in sets, reverse indexes with key as disease-title-index:word or disease-index:word and ids in the set
finally save the tokens from both to the autocomplete sets"
  (let* ((disease-tokens (nlp:tokenize disease))
	 (all-tokens (remove-duplicates `(,@disease-tokens ,@tokens))))
    (dolist (token tokens)
      (redis:red-sadd (format nil "{disease-index}:~a" token) id))
    (dolist (token disease-tokens)
      (redis:red-sadd (format nil "{disease-title-index}:~a" token) id))
    (create-autocomplete all-tokens)))

(defun create-autocomplete (tokens)
  "given a list of tokens, add them to autocomplete sets starting at words with 1 character.
forexample: tokens is saved in tok, toke, token, tokens"
  (dolist (token tokens)
    (when (>= (length token) 1)
      (save-to-autocomplete token))))

(defun save-to-autocomplete (token &key (pos 1))
  "given a word, start at length 1 then save the word fragments to autoincrement, we use sorted sets, such that we can track the words appearing most in the dataset."
  (unless (> pos (1- (length token)))
    (let ((subtoken (str:substring 0 pos token)))
      (redis:red-zincrby (format nil "{auto-complete}:~a" subtoken) 1 token))
    (save-to-autocomplete token :pos (1+ pos))))

(defun search-disease-data (word)
  "given word, find any keys in disease title and disease indexes corresponding to it. 
since data is going to be stored in different slots, sunion will not work
merge them and remove duplicates, return the resultant list"
  (let* ((title-keys (redis:red-smembers (format nil "{disease-title-index}:~a" word)))
	 (index-keys (redis:red-smembers (format nil "{disease-index}:~a" word))))
    (remove-duplicates (nconc title-keys index-keys) :test #'string=)))

(defun get-autocomplete (fragment)
  "given a fragment of a word, find the autocomplete for it."
  (redis:red-zrange (format nil "{auto-complete}:~a" fragment) 0 10))
