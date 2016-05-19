;;;
;;; buffer-cycle.el --- defuns to cycle between several buffers of the same
;;;                     type.
;;;
;; Author: David M. Karr <dkarr@nmo.gtegsc.com>
;; Keywords: buffers

;;; This package defines a small number of utility defuns to be used by other
;;; packages to provide the facility of cycling quickly between several buffers
;;; of the same type.  It assumes the first buffer of the type will be named
;;; "*something*", and following ones will be named "*something-1*", etc.
;;;
;;; The defun "next-bufname" takes a BUFNAME and a CORESTRING and returns what
;;; would be the next buffer name in the cycle.
;;;
;;; The defun "cycle-next-buffer" takes a CORESTRING.  It looks at the current
;;; buffer and gets the result of "next-bufname" given the current buffer and
;;; the CORESTRING.  If that buffer exists, then it tries to switch to that
;;; new buffer.  If buffers in the middle of the ring are deleted, then the
;;; buffers at the end are unreachable by this method.  It could be made more
;;; robust by storing the buffer names in a list and tracking buffer deletes,
;;; etc., but this is probably good enough.

(provide 'buffer-cycle)

(defun next-bufname (bufname corestring)
  "Given a particular BUFNAME and a CORESTRING (like \"*CORESTRING*\"), return
the next buffer in line."
  (if (string-equal (concat "*" corestring "*") bufname)
      (concat "*" corestring "-1*")
    (progn
      (string-match (concat "\*" corestring "\\(\*\\|-\\([0-9][0-9]*\\)\*\\)")
                    bufname)
      (concat "*" corestring "-"
				  (int-to-string
					(+ (string-to-int
						 (substring bufname (match-beginning 2) (match-end 2)))
						1))
				  "*")
      )
    )
  )

(defun prev-bufname (bufname corestring)
  "Given a particular BUFNAME and a CORESTRING (like \"*CORESTRING*\"), return
the previous buffer in line."
  (string-match (concat "\*" corestring "\\(\*\\|-\\([0-9][0-9]*\\)\*\\)")
					 bufname)
  (setq new-bufname 
		  (concat "*" corestring "-"
					 (int-to-string
					  (- (string-to-int
							(substring bufname
										  (match-beginning 2) (match-end 2)))
						  1))
					 "*"))
  (if (string= new-bufname (concat "\*" corestring "-0\*"))
      (concat "\*" corestring "\*")
    new-bufname)
  )

(defun cycle-next-buffer (corestring)
  (let* ((bufname (buffer-name (current-buffer))))
    (if (string-match
         (concat "\*" corestring "\\(\*\\|-\\([0-9][0-9]*\\)\*\\)") bufname)
        (progn
          (setq new-bufname (next-bufname bufname corestring))
          (if (bufferp (get-buffer new-bufname))
              (switch-to-buffer new-bufname)
            (switch-to-buffer (concat "*" corestring "*"))
            )
          )
      (switch-to-buffer (concat "*" corestring "*"))
      )
    )
  )

(defun cycle-prev-buffer (corestring)
  (let* ((bufname (buffer-name (current-buffer))))
    (if (string-match
         (concat "\*" corestring "\\(\*\\|-\\([0-9][0-9]*\\)\*\\)") bufname)
        (progn
	  (if (string= (concat "\*" corestring "\*") bufname)
	      (let ((last-bufname bufname))
		(while (bufferp (get-buffer
				 (setq new-bufname
				       (next-bufname last-bufname corestring))))
		  (setq last-bufname new-bufname)
		  )
		(switch-to-buffer last-bufname)
		)
	    (let ((new-bufname (prev-bufname bufname corestring)))
	      (if (bufferp (get-buffer new-bufname))
		  (switch-to-buffer new-bufname)
		(switch-to-buffer (concat "*" corestring "*"))
		)
	      )
	    )
	  )
      (switch-to-buffer (concat "*" corestring "*"))
      )
    )
  )

(defun cycle-go-buffer (corestring bufnum)
  (setq new-bufname
	(concat "\*" corestring
		(if (not (eq bufnum 0))
		  (concat "-" (number-to-string bufnum)))
		"\*"))
  (if (bufferp (get-buffer new-bufname))
      (switch-to-buffer new-bufname)
    (error (concat "No " corestring " buffer in " new-bufname ".")))
  )
