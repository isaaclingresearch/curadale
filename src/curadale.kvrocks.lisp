(in-package :redis)

(def-cmd ZUNION (num &rest keys) :anything
  "Return the union between the Zsets stored at key1, key2, ..., keyN.")

(in-package :curadale)

(defun start-kvrocks ()
  (sb-ext:run-program (namestring (truename "~/.bin/kvrocks"))
		      (list "-c" (namestring (truename "~/common-lisp/curadale/conf/kvrocks.conf")))
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
1. A disease id (uuid v7) is saved as key and a hash as value with fields corresponding to the fields of data. use the proper name to comfirm that the disease with that name has not been saved otherwise use the same id to prevent data duplication.
2. All the data is tokenised, normalised and tokens stored in reverse indexes for search.

The urls will be based on the proper name, you save a url with lowercase and no spaces to be able to trace the id from url later.
as we will be presenting human readable urls to the readers."
  (let* ((proper-name (getf data :proper-name disease))
	 (id (or (get-id-from-url (make-url proper-name))
		 (format nil "{disease}:~a" (to-string (make-v7)))))
	 (intro (getf data :introduction))
	 (cause (getf data :cause))
	 (epidemiology (getf data :epidemiology))
	 (risk-factors (getf data :risk-factors))
	 (diagnosis (getf data :diagnosis))
	 (differential-diagnoses (getf data :differential-diagnoses))
	 (pathophysiology (getf data :pathophysiology))
	 (signs-and-symptoms (getf data :signs-and-symptoms))
	 (complications (getf data :complications))
	 (alternative-names (getf data :alternative-names))
	 (prevention (getf data :prevention))
	 (living-with (getf data :living-with))
	 (one-string (format nil "~a ~a ~a ~a ~a ~{~a ~} ~{~a ~} ~a ~{~a ~} ~{~a ~} ~{~a ~} ~{~a ~} ~{~a ~}" proper-name disease intro cause epidemiology risk-factors differential-diagnoses pathophysiology signs-and-symptoms complications alternative-names prevention living-with)))
    ;; save the data
    (redis:red-hset id "name" proper-name)
    (redis:red-hset id "introduction" intro)
    (redis:red-hset id "cause" cause)
    (redis:red-hset id "epidemiology" epidemiology)
    (redis:red-hset id "risk-factors" risk-factors)
    (redis:red-hset id "diagnosis" diagnosis)
    (redis:red-hset id "differential-diagnoses" differential-diagnoses)
    (redis:red-hset id "pathophysiology" pathophysiology)
    (redis:red-hset id "signs-and-symptoms" signs-and-symptoms)
    (redis:red-hset id "complications" complications)
    (redis:red-hset id "alternative-names" alternative-names)
    (redis:red-hset id "prevention" prevention)
    (redis:red-hset id "living-with" living-with)
    (redis:red-set (format nil "{url}:~a" (make-url proper-name)) id)
    (create-zset-index id proper-name one-string) ;; create indexes for both autocomplete and search
    id))

(defun disease-hget (id field)
  (redis:red-hget id field))

(defun save-disease-id (url id)
  (redis:red-set (format nil "{url}:~a" url) id))

(defun get-id-from-url (url)
  (redis:red-get (format nil "{url}:~a" url)))

(defun get-search (txt)
  "given a fragment, get all product ids of it in order of which contain many instances of tokens in txt"
  (let* ((tokens (nlp:tokenize txt))
	 (names (mapcar (lambda (token)
			  (to-alist
			   (redis:red-zrange (format nil "{index}{search}:~a" (string-downcase token)) 0 -1 :withscores)))
			tokens)))
    (unless (equal '(nil) names)
      (mapcar #'car (combine-alists names)))))

(defun create-zset-index (&optional key proper-name sdata)
  "from the disease data saved, create a zset for index using the title and all other data"
  (let* ((keys (if key (list key) (redis:red-keys "{disease}:*"))))
    (dolist (key keys)
      (let* ((data (remove-if (lambda (d) (equal "NIL" d)) (to-alist (redis:red-hgetall key)) :key #'cdr))
	     ;; add name tokens to boost rank of specific id when searching as they will appear twice, once in general and in name
	     (name (or proper-name (cdr (find "name" data :key #'car :test #'equal))))
	     (name-tokens (nlp:tokenize name))
	     (string-data (or sdata (format nil "~{~a ~}" data)))
	     (tokens `(,@(nlp:tokenize string-data) ,@name-tokens)))
	(dolist (token tokens)
	 (save-to-index key name token))))))

(defun save-to-index (id name token &key (pos 1))
  "given a word, start at length 1 then save the word fragments to {index}{search}
   we use sorted sets, such that we can track the words appearing most in the dataset."
  (unless (> pos (length token))
    (let ((subtoken (str:substring 0 pos token)))
      (redis:red-zincrby (format nil "{index}{autocomplete}:~a" subtoken) 1 name)
      (redis:red-zincrby (format nil "{index}{search}:~a" subtoken) 1 id))
    (save-to-index id name token :pos (1+ pos))))

(defun reset-index ()
  "delete all data in {index} and recreate it"
  (let ((keys `(,@(redis:red-keys "{index}*") ,@(redis:red-keys "{auto-complete}*"))))
    (dolist (key keys)
      (redis:red-del key))
    (create-zset-index)))

(defun get-autocomplete (txt)
  "given a fragment, get all disease names for which it is part, return only 10 of the most frequent"
  (let* ((tokens (nlp:tokenize txt))
	 (names (mapcar (lambda (token)
			  (to-alist
			   (redis:red-zrange (format nil "{index}{autocomplete}:~a" (string-downcase token)) 0 -1 :withscores)))
			tokens)))
    (unless (equal '(nil) names)
      (let ((combined-alist (combine-alists names)))
	(if (<= (length combined-alist) 10)
	    (mapcar #'car combined-alist)
	    (mapcar #'car (subseq combined-alist 0 10)))))))


(defun to-alist (lst)
  "Convert a list of elements into an alist assuming alternating key-value pairs."
  (loop for (key value) on lst by #'cddr
        collect (cons key value)))

(defun combine-alists (alist-list)
  "Combine a list of alists, summing integer values for each key."
  (let ((result (make-hash-table :test 'equal)))
    (dolist (alist alist-list)
      (dolist (pair alist)
        (let* ((key (car pair))
               (value (parse-integer (cdr pair)))
               (current (gethash key result 0)))
          (setf (gethash key result) (+ current value)))))
    ;; Convert hash table to alist
    (let ((combined-alist nil))
      (maphash (lambda (key value)
                 (push (cons key value) combined-alist))
               result)
      (sort combined-alist (lambda (a b) (> (cdr a) (cdr b)))))))
