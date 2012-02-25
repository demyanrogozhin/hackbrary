;;;; Download video lectures (from C0RS3RA)
;; (ql:quickload :drakma) (ql:quickload :closure-html)
;; (ql:quickload :cl-ppcre) (ql:quickload :cxml)
(defpackage :hackbrary.vidload
  (:use :cl :cl-ppcre :chtml :cxml-dom :drakma :puri)
  (:export :vidload))
(in-package :hackbrary.vidload)

(defun get-video-links (url)
  (let ((search-tag-name "a")
		(search-tag-attr "rel")
		(search-tag-attr-value "lecture-link")
		(value-attr "href"))
	(loop for tag
	   across (dom:get-elements-by-tag-name
			   (parse (http-request (uri url)) (make-dom-builder))
			   search-tag-name)
	   when (and (dom:has-attribute tag value-attr)
				 (string= search-tag-attr-value
						  (dom:get-attribute tag search-tag-attr)))
										; puri didn't accept spaces in url
	   collect  `((url . ,(uri (remove #\SPACE (dom:get-attribute
												tag value-attr))))
				  (txt . ,(remove #\NEWLINE
								  (dom:data (dom:first-child tag))))))))

(defun grab-video-file (pair)
  (let* ((page-url (cdr (assoc 'url pair)))
		 (html (http-request page-url))
		 (url (multiple-value-bind (match src)
				  (scan-to-strings
				   "src=\"\([^\"]*\.mp4\)\""
				   (remove #\NewLine html))
				(declare (ignore match))
				(uri (aref src 0))))
		 (name  (remove #\/ (remove #\? (cdr (assoc 'txt pair)))))
		 (stream (http-request url :want-stream t))
		 (file (merge-pathnames (concatenate 'string name ".mp4"))))
    (with-open-file (out file :direction :output :if-exists :supersede
                         :element-type '(unsigned-byte 8))
	  ;; fixme: #<FLEXI-STREAMS:FLEXI-IO-STREAM> is not of type SEQUENCE
										; (write-sequence stream out))))
	  (loop for byte = (read-byte stream nil nil)
		 while byte do (write-byte byte out)))))

(defun vidload (url-str)
  (loop for pair in (get-video-links (uri url-str))
	 while pair do (print (cdr (assoc 'txt pair))) (grab-video-file pair)))
