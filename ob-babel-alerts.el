;;; ob-babel-alerts.el --- Alerts for org-babel code blocks -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024
;;
;; Author:  <elle@etude>
;; Maintainer:  <elle@etude>
;; Created: October 28, 2024
;; Modified: October 28, 2024
;; Version: 0.0.1
;; Keywords: org babel alerts convenience
;; Homepage: https://github.com/elle/ob-babel-alerts
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Provides alert notifications when org-babel code blocks finish executing.
;;  Works with any language supported by org-babel, not just Python.
;;  Features:
;;  - Desktop notifications when blocks finish
;;  - Clickable links to jump to the completed block
;;  - Automatic alerts for long-running blocks
;;
;;; Code:

;;;; Customization
(defgroup ob-babel-alerts nil
  "Customization options for org-babel alerts."
  :group 'org-babel
  :prefix "ob-babel-alerts/")

(defcustom ob-babel-alerts/notification-command "notify-send -i emacs \"Org Block Finished\" \"Block in %b completed with result: %r\""
  "Command to run when a code block finishes.
Special format specifiers:
%b - buffer name
%f - buffer file name (or empty if no file)
%r - result content (or empty if no result)
%l - line number"
  :type 'string
  :group 'ob-babel-alerts)

(defcustom ob-babel-alerts/long-running-threshold 10
  "Threshold in seconds for considering a block as long-running."
  :type 'integer
  :group 'ob-babel-alerts)

;;;; Alerts
(require 'doom-keybinds)


(define-derived-mode ob-babel-alerts/cell-alerts-mode special-mode "Block Alerts"
  "Major mode for displaying code block completion alerts.")

(defun ob-babel-alerts/format-notification-command (buffer-name buffer-file line-number result-content)
  "Format the notification command with the given parameters.
BUFFER-NAME is the name of the buffer.
BUFFER-FILE is the file path of the buffer.
LINE-NUMBER is the line number of the code block.
RESULT-CONTENT is the content of the results block."
  (let ((cmd ob-babel-alerts/notification-command)
        (result-summary (if result-content
                            (if (> (length result-content) 100)
                                (concat (substring result-content 0 97) "...")
                              result-content)
                          "No result")))
    ;; Replace format specifiers
    (setq cmd (replace-regexp-in-string "%b" (shell-quote-argument buffer-name) cmd))
    (setq cmd (replace-regexp-in-string "%f" (if buffer-file 
                                                (shell-quote-argument buffer-file) 
                                              "") cmd))
    (setq cmd (replace-regexp-in-string "%l" (number-to-string line-number) cmd))
    (setq cmd (replace-regexp-in-string "%r" 
                                        (replace-regexp-in-string "\\\\" "\\\\\\\\" 
                                                                 (shell-quote-argument result-summary))
                                        cmd))
    cmd))

(defun ob-babel-alerts/block-finished-alert (&optional result-content)
  "Create an alert with an Emacs-native clickable link in a pop-up buffer when a code block finishes.
Optional RESULT-CONTENT is the content of the results block to display in the alert."
  (let* ((buffer-name (buffer-name))
         (buffer-file (buffer-file-name))
         (line-number (line-number-at-pos))
         (link-text (if buffer-file
                        (format "%s:%d" buffer-file line-number)
                      buffer-name))
         (alerts-buffer-name "*Block Completion Alerts*")
         (notification-cmd (ob-babel-alerts/format-notification-command 
                            buffer-name buffer-file line-number result-content)))

    (with-current-buffer (get-buffer-create alerts-buffer-name)
      (unless (eq major-mode 'ob-babel-alerts/cell-alerts-mode)
        (ob-babel-alerts/cell-alerts-mode))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (let ((start (point)))
          (insert "\n\n")
          (insert (format-time-string "[%Y-%m-%d %H:%M:%S]\n"))
          (insert "A code block finished at:\n")
          (shell-command notification-cmd)
          (insert-text-button link-text
                              'action (lambda (_)
                                        (if buffer-file
                                            (find-file-other-window buffer-file)
                                          (switch-to-buffer-other-window buffer-name))
                                        (when buffer-file
                                          (goto-char (point-min))
                                          (forward-line (1- line-number))))
                              'follow-link t
                              'help-echo "Click to go to the code block location")
          (put-text-property start (point) 'read-only t)))
      (let ((window (display-buffer-in-side-window (current-buffer) '((side . bottom)))))
        (when window
          (with-selected-window window
            (goto-char (point-max))
            (recenter -1))))))
  (message "Finished code block!"))

;; Doom Emacs specific configuration
(add-to-list 'evil-escape-excluded-major-modes 'ob-babel-alerts/cell-alerts-mode)
(evil-set-initial-state 'ob-babel-alerts/cell-alerts-mode 'normal)

(map! :map ob-babel-alerts/cell-alerts-mode-map
      :n "q" #'quit-window
      :n [escape] #'quit-window)

;; Function to close the alerts buffer
(defun ob-babel-alerts/close-alerts-buffer ()
  "Close the Block Completion Alerts buffer from anywhere."
  (interactive)
  (when-let ((buffer (get-buffer "*Block Completion Alerts*")))
    (when-let ((window (get-buffer-window buffer t)))
      (quit-window nil window))))

;; ESC key handling
(defadvice! my-universal-esc-handler (&rest _)
  :before #'keyboard-quit
  (when (get-buffer-window "*Block Completion Alerts*" t)
    (ob-babel-alerts/close-alerts-buffer)))

;; Set up the display rules for the alerts buffer
(set-popup-rule! "^\\*Block Completion Alerts\\*$"
  :side 'bottom
  :size 0.3
  :select nil
  :quit t)

;;;;; Alerts for long running blocks

(defun ob-babel-alerts/notify-if-took-a-while (alert-threshold result-content)
  "Notify if block execution took longer than ALERT-THRESHOLD seconds.
RESULT-CONTENT is the content to display in the notification."
  (interactive)
  (save-excursion
    (let ((case-fold-search t))
      (if (search-forward-regexp "^[ \t]*#\\+RESULTS:" nil t)
          (let ((end (save-excursion
                       (if (search-forward-regexp "^[ \t]*#\\:END:" nil t)
                           (match-beginning 0)
                         (point-max)))))
            (when (search-forward-regexp "^Cell Timer:\\s-*\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)" end t)
              (let ((hours (string-to-number (match-string 1)))
                    (minutes (string-to-number (match-string 2)))
                    (seconds (string-to-number (match-string 3))))
                (setq total-seconds (+ (* hours 3600) (* minutes 60) seconds))
                (when (>= total-seconds alert-threshold)
                  (message "Block execution took %d seconds (threshold: %d)" 
                           total-seconds alert-threshold)
                  t))))
        (message "No results block found.")
        nil))))

(defun ob-babel-alerts/extract-result-content (results-start results-end)
  "Extract the content of a results block between RESULTS-START and RESULTS-END.
Strips away the #+RESULTS:, #+BEGIN_*, #+END_*, :RESULTS:, :result:, and :END: markers."
  (when (and results-start results-end)
    (save-excursion
      (let ((content "")
            (in-results nil))
        (goto-char results-start)
        (forward-line 1) ;; Skip the #+RESULTS: line
        
        ;; Check if we have a drawer or block
        (cond
         ;; Handle #+BEGIN_* ... #+END_* blocks
         ((looking-at "^[ \t]*#\\+BEGIN_\\(.*\\)$")
          (forward-line 1) ;; Skip the #+BEGIN line
          (let ((begin-pos (point))
                (end-regexp (format "^[ \t]*#\\+END_%s$" (match-string 1))))
            (when (re-search-forward end-regexp results-end t)
              (setq content (buffer-substring-no-properties 
                             begin-pos (match-beginning 0))))))
         
         ;; Handle :RESULTS: ... :END: drawers
         ((looking-at "^[ \t]*:RESULTS:")
          (forward-line 1) ;; Skip the :RESULTS: line
          (let ((begin-pos (point)))
            (when (re-search-forward "^[ \t]*:END:" results-end t)
              (setq content (buffer-substring-no-properties 
                             begin-pos (match-beginning 0))))))
         
         ;; Handle plain results (no special delimiters)
         (t
          (let ((begin-pos (point)))
            (setq content (buffer-substring-no-properties 
                           begin-pos results-end)))))
        
        ;; Clean up the content - remove any :result: markers
        (setq content (replace-regexp-in-string "^[ \t]*:result:[ \t]*" "" content))
        ;; Trim whitespace
        (string-trim content)))))

(defun ob-babel-alerts/org-src-block-results-end (src-block)
  "Find the end position of results for SRC-BLOCK."
  (save-excursion
    (goto-char (org-element-property :begin src-block))
    (let ((results-start (org-babel-where-is-src-block-result)))
      (when results-start
        (goto-char results-start)
        (if (looking-at "^[ \t]*#\\+RESULTS:")
            (progn
              (forward-line)
              (if (looking-at "^[ \t]*#\\+BEGIN_\\(.*\\)$")
                  (progn
                    (re-search-forward "^[ \t]*#\\+END_\\1$" nil t)
                    (point))
                (org-babel-result-end)))
          (point-max))))))

(defun ob-babel-alerts/alert-advice-after-org-babel-results (orig-fun params &rest args)
  (let* ((options (nth 2 (car args)))
         (alert-finish (if (string= "yes" (cdr (assq :alert options))) t nil))
         (src-block (org-element-at-point))
         (results-start (org-babel-where-is-src-block-result))
         (results-end (ob-babel-alerts/org-src-block-results-end src-block))
         (result-content nil))

    (when results-start
      (save-excursion
        (goto-char results-start)
        (if (re-search-forward "^[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}$" results-end t)
            ()
          (progn
            ;; Extract the content from the results block
            (setq result-content (ob-babel-alerts/extract-result-content results-start results-end))
            
            ;; Only show one notification - either from explicit :alert or from long-running
            (if alert-finish
                ;; User explicitly requested alert
                (ob-babel-alerts/block-finished-alert result-content)
              ;; Check if it was a long-running block
              (when (ob-babel-alerts/notify-if-took-a-while 
                     ob-babel-alerts/long-running-threshold result-content)
                (ob-babel-alerts/block-finished-alert result-content)))))))))


(advice-add 'org-babel-insert-result :after #'ob-babel-alerts/alert-advice-after-org-babel-results)
;; (setq debug-on-message "Code block evaluation complete\\.")




(provide 'ob-babel-alerts)
;;; ob-babel-alerts.el ends here
