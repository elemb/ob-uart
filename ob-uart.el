;;; ob-uart.el --- org-babel support for UART communication

;; Copyright (C) Andreas Müller

;; Original Author and Idea and 100% credit to: Andreas Müller
;; Revision Author: elemb
;; Keywords: tools, comm, org-mode, UART, literate programming, reproducible development
;; Homepage: https://github.com/elemb/ob-uart
;; Version: 0.1.0

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; Org-babel support for UART/serial communication with embedded devices.
;; Supports Forth systems like Mecrisp and ZeptoForth.

;;; Code:
(require 'ob)

(defgroup ob-uart nil
  "UART support for org babel."
  :group 'org-babel)

(defcustom ob-uart-debug nil
  "Whether to show debug messages for UART communication."
  :group 'ob-uart
  :type 'boolean)

(defvar org-babel-default-header-args:uart
  '((:port . "/dev/ttyUSB0")
    (:speed . 115200)
    (:bytesize . 8)
    (:parity . nil)
    (:stopbits . 1)
    (:flowcontrol . nil)
    (:timeout . 2)
    (:lineend . "\n")
    (:ienc . "raw")
    (:oenc . "raw"))
  "Default arguments for evaluating a UART block.")

(defun ob-uart--to-number (val default)
  "Convert VAL to number. If already number, return it. If string, convert. Else DEFAULT."
  (cond
   ((numberp val) val)
   ((and (stringp val) (not (string-empty-p val)))
    (let ((num (string-to-number val)))
      (if (zerop num)
          default
        num)))
   (t default)))

(defun ob-uart--to-string (val)
  "Convert VAL to string if it's a symbol, otherwise return as-is."
  (cond
   ((stringp val) val)
   ((symbolp val) (symbol-name val))
   ((numberp val) (number-to-string val))
   (t (format "%s" val))))

(defun ob-uart--to-symbol (val valid-syms)
  "Convert VAL to symbol if in VALID-SYMS, else nil."
  (let ((sym (cond
              ((symbolp val) val)
              ((stringp val) 
               (let ((s (downcase val)))
                 (cond
                  ((member s '("nil" "none")) nil)
                  (t (intern s)))))
              (t nil))))
    (if (memq sym valid-syms)
        sym
      nil)))

;;;###autoload
(defun org-babel-execute:uart (body params)
  "Execute UART code block with BODY and PARAMS."
  (when ob-uart-debug
    (message "ob-uart executing with params: %S" params))
  
  (let* ((port (ob-uart--to-string 
                (or (cdr (assq :port params)) "/dev/ttyUSB0")))
         (speed (ob-uart--to-number 
                 (cdr (assq :speed params)) 115200))
         (bytesize (ob-uart--to-number 
                    (cdr (assq :bytesize params)) 8))
         (parity (ob-uart--to-symbol 
                  (cdr (assq :parity params))
                  '(odd even)))
         (stopbits (ob-uart--to-number 
                    (cdr (assq :stopbits params)) 1))
         (flowcontrol (ob-uart--to-symbol 
                       (cdr (assq :flowcontrol params))
                       '(hw sw)))
         (timeout (ob-uart--to-number 
                   (cdr (assq :timeout params)) 2))
         (lineend (ob-uart--to-string 
                   (or (cdr (assq :lineend params)) "\n")))
         (ienc (ob-uart--to-string 
                (or (cdr (assq :ienc params)) "raw")))
         (oenc (ob-uart--to-string 
                (or (cdr (assq :oenc params)) "raw")))
         (proc-name (format "ob-uart-%s" 
                            (replace-regexp-in-string "[/:]" "-" port)))
         (proc-buf (format " *ob-uart-%s*" 
                           (replace-regexp-in-string "[/:]" "-" port))))
    
    (when ob-uart-debug
      (message "ob-uart: port=%s speed=%d bytesize=%d parity=%s stopbits=%d flow=%s timeout=%d"
               port speed bytesize parity stopbits flowcontrol timeout))
    
    ;; Clean up existing process
    (when (get-process proc-name)
      (delete-process proc-name))
    (when (get-buffer proc-buf)
      (kill-buffer proc-buf))
    
    ;; Execute with error handling
    (condition-case err
        (let ((proc (make-serial-process
                     :name proc-name
                     :buffer proc-buf
                     :port port
                     :speed speed
                     :bytesize bytesize
                     :parity parity
                     :stopbits stopbits
                     :flowcontrol flowcontrol)))
          
          ;; Encode input if needed
          (let ((encoded-body body))
            (when (string= ienc "hex")
              (setq encoded-body 
                    (mapconcat
                     (lambda (hex)
                       (char-to-string (string-to-number hex 16)))
                     (split-string body "[ \t\n]+" t)
                     "")))
            
            ;; Send data
            (process-send-string proc (concat encoded-body lineend))
            
            ;; Wait for response using non-blocking I/O
            (let ((start-time (float-time))
                  (elapsed 0))
              (while (and (< elapsed timeout)
                         (process-live-p proc))
                (accept-process-output proc 0.1 nil t)
                (setq elapsed (- (float-time) start-time))))
            
            ;; Get result
            (let ((result (with-current-buffer proc-buf
                           (buffer-string))))
              
              ;; Clean up
              (when (process-live-p proc)
                (delete-process proc))
              (when (get-buffer proc-buf)
                (kill-buffer proc-buf))
              
              ;; Strip control characters (except newlines and tabs)
              (setq result (replace-regexp-in-string "[\x00-\x08\x0B-\x1F\x7F]" "" result))
              
              ;; Decode output if needed
              (when (string= oenc "hex")
                (setq result 
                      (mapconcat 
                       (lambda (c) (format "%02x" c))
                       result " ")))
              (when (string= oenc "HEX")
                (setq result 
                      (mapconcat 
                       (lambda (c) (format "%02X" c))
                       result " ")))
              
              result)))
      
      (error
       (format "Error in UART communication: %s" 
               (error-message-string err))))))

(provide 'ob-uart)
;;; ob-uart.el ends here
