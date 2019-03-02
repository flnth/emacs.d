;;; modules/feature/config-json/config-json.el       -*- lexical-binding: t -*-

;; TODO:  use new emacs-27 json library, ...maybe switch to yaml?

(defstruct (config-json
			(:type vector)
			(:constructor config-json-create)
			:named
			(:copier config-json-copy)
			)
  "Always up-to-date json file contents."
  path			  ; path to json file
  (last-access 0) ; last access of file, checked against real json modification time
  contents		  ; parsed content, always kept up-to-date
  )

(defun config-json-last-mod (json)
  "Get last-modified file attribute of json file."
  (cadr (nth 5 (file-attributes (config-json-path json)))))

(defun config-json-load (json)
  "Parse json file and return new json instance, if necessary."
  (if (null json)
	  (message "--config-json-load: Warning: json nil.")
	(let* ((file-exists (f-exists? (config-json-path json))))
	  ;; does not exist - create new
	  (when (not file-exists)
		(f-mkdir (f-dirname (config-json-path json)))
		(f-write-bytes "" (config-json-path json)))
	  ;; exists - copy, check modification, reload if necessary
	  (if (> (config-json-last-mod json) (config-json-last-access json))
		  (let ((json-copy (config-json-copy json)))
			(setf (config-json-contents json-copy) (json-read-file (config-json-path json-copy)))
			(setf (config-json-last-access json-copy) (nth 1 (current-time)))
			json-copy)
		json))))

(defun config-json-write! (json)
  "Write content of JSON back to json file.
Modifes last-access of json as side-effect."
  ;; precondition:  path and content valid
  (with-temp-file (config-json-path json)
	(insert (json-encode (config-json-contents json)))
	(json-pretty-print (point-min) (point-max)))
  (setf (config-json-last-access json) (nth 1 (current-time))))

(defun config-json-synchronize! (json)
  "Synchronize json contents with the json file on disk.
Modifies last-access of json as side-effect. Assumes that it is
called after every local state change.

Will synchronize local state from json file if it has changed
recently. Will synchronize upstream otherwise."
  (let* ((last-access (config-json-last-access json))
		 (last-modified (config-json-last-mod json))
		 (json-from-file (config-json-load json))
		 (new-content
		  (if (>= last-access last-modified)
			  (config-json-contents json)
			(config-json-contents json-from-file))))
	(setf (config-json-contents json) new-content)
	(config-json-write! json)))

(defun config-json-ensure-valid-format (json-content)
  "Make sure that json-objects are consed together. If they are
normal lists, json library writes something else than it reads
out. With no apparent way to fix that :/."
  (loop for (obj-ident . obj-content) in json-content
		collect (obj-ident . obj-content))
  )

(provide 'feature/config-json/config-json)
