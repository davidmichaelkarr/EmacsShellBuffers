;;;
;;; cycle-shell.el --- Package to allow creation and cycling between multiple
;;;                    "shell" buffers.  This requires the "buffer-cycle"
;;;                    package and the "shell-for-cycle" package (which is just
;;;                    a barely modified version of shell.el from the v19
;;;                    distribution).
;;;
;; Author: David M. Karr <dkarr@nmo.gtegsc.com>
;; Keywords: buffers shell

;;; This package allows the user to create multiple shell buffers and quickly
;;; move between them.

(require 'buffer-cycle)

(defvar last-shell-buffer nil "BUFFER object, last shell buffer")

(defun cycle-make-shell ()
  "Implements a ring of *shell* buffers.  If current buffer is not a shell
buffer, then it will just execute 'shell'.  If it IS a shell buffer, then
create a new shell buffer, starting with '*shell-1*', but skipping to the next
number if that already exists."
  (interactive)
  (let* ((bufname (buffer-name (current-buffer))))
    (if (string-match "\*shell\\(\*\\|-\\([0-9][0-9]*\\)\*\\)" bufname)
        (progn
          (setq change-dir default-directory)
          (setq done nil)
          (while (not done)
            (progn
              (setq new-bufname (next-bufname bufname "shell"))
              (if (bufferp (get-buffer new-bufname))
                  (setq bufname new-bufname)
                (setq done t)
                )
              )
            )
          (if (bufferp (get-buffer "*shell*"))
              (progn
                (set-buffer "*shell*")
                (rename-uniquely)
                (setq tmp-bufname (buffer-name))
                ))
          (shell)
	  (sit-for 1)
          (set-buffer "*shell*")
          (rename-buffer new-bufname)
          ;; Now we need to somehow change the directory to "change-dir".
          (set-buffer tmp-bufname)
          (rename-buffer "*shell*")
          (set-buffer new-bufname)
          )
      (progn
        ;; check for existence of buffer last-shell-buffer.  If it exists,
        ;; go to it.  If not, then execute "shell".

        (if (and (bufferp last-shell-buffer)
                 (not (killed-buffer-p last-shell-buffer)))
            (switch-to-buffer last-shell-buffer)
          (shell))
        )
      )
    )
  )

(defun cycle-go-shell (&optional arg)
  "If in a shell buffer, go to the next shell buffer in the ring."
  (interactive "P")
  (if arg
      (if (numberp arg)
	  (cycle-go-buffer "shell" arg)
	(cycle-prev-buffer "shell"))
    (cycle-next-buffer "shell"))
  (if (not (eq major-mode 'shell-mode))
      (shell))
  (setq last-shell-buffer (current-buffer))
  )

(defun cycle-find-shell (&optional string)
  (interactive "sEnter directory substring: ")
  (let* ((buflist (buffer-list))
	 (next-buffer nil)
	 (chosen-buffer nil)
	 )
    (while (setq next-buffer (car buflist))
      (setq buflist (cdr buflist))
      (setq bufname (buffer-name next-buffer))
      (setq substring "*shell")
      (if (string-equal
	   (substring
	    bufname 0 (min (length substring) (length bufname))) substring)
	  (progn
	    (set-buffer next-buffer)
	    (if (string-match string default-directory)
		(setq chosen-buffer next-buffer)
	      )
	    )
	)
      )
    (if chosen-buffer
	(progn
	  (switch-to-buffer chosen-buffer)
	  (setq last-shell-buffer chosen-buffer)
	  )
      (error (concat "PWD with \"" string "\" not found."))
      )
    )
  )

(defun killed-buffer-p (buffer)
  "Return t if BUFFER is killed."
  (not (buffer-name buffer)))

(defun shells-buffer-list ()
  (interactive)
  (electric-buffer-list "^\*shell")
  )

(defun cycle-find-shell-or-shells-buffer-list (&optional arg)
  (interactive "P")
  (if arg
                (shells-buffer-list)
         (call-interactively 'cycle-find-shell)
                )
  )
