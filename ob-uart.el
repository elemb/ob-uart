;;; ob-uart.el --- org-babel support for UART communication
;; Copyright (C) Andreas Müller
;; Author: Andreas Müller
;; Keywords: tools, comm, org-mode, UART, literate programming, reproducible development
;; Homepage: https://www.0x7.ch
;; Version: 0.0.2

;;; License:
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Code:
(require 'ob)

(defvar org-babel-default-header-args:uart
  '((:port . "/dev/cu.usbmodem01")
    (:speed . 115200)
    (:timeout . 1)
    (:lineend . "\n"))
  "Default arguments for evaluating a UART block.")

;;;###autoload
(defun org-babel-execute:uart (body params)
  "Execute UART communication block.
BODY is the content to send.
PARAMS are the communication parameters."
  (let* ((port (or (cdr (assoc :port params)) "/dev/cu.usbmodem01"))
         (speed (let ((s (cdr (assoc :speed params))))
                  (if (numberp s) s (string-to-number (or s "115200")))))
         (timeout (let ((t (cdr (assoc :timeout params))))
                    (if (numberp t) t (string-to-number (or t "1")))))
         (lineend (or (cdr (assoc :lineend params)) "\n"))
         (process-name (format "ob-uart-%s" (replace-regexp-in-string "/" "-" port)))
         (buffer-name (format " *%s*" process-name)))
    
    ;; Kill existing process and buffer
    (when (get-process process-name)
      (delete-process process-name))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    
    ;; Create serial process
    (let ((process (make-serial-process
                    :name process-name
                    :buffer buffer-name
                    :port port
                    :speed speed
                    :noquery t)))
      
      ;; Send command
      (process-send-string process (concat body lineend))
      
      ;; Wait for response
      (sleep-for timeout)
      
      ;; Get result
      (let ((result ""))
        (when (buffer-live-p (get-buffer buffer-name))
          (with-current-buffer buffer-name
            (setq result (buffer-string))))
        
        ;; Clean up
        (when (process-live-p process)
          (delete-process process))
        (when (get-buffer buffer-name)
          (kill-buffer buffer-name))
        
        ;; Return result
        (if (string-empty-p result)
            "No response"
          result)))))

(provide 'ob-uart)
;;; ob-uart.el ends here
