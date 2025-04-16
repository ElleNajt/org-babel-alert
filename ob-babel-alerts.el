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

;;;; Alerts
(require 'doom-keybinds)


(define-derived-mode ob-babel-alerts/cell-alerts-mode special-mode "Block Alerts"
  "Major mode for displaying code block completion alerts.")

(defun ob-babel-alerts/block-finished-alert ()
  "Create an alert with an Emacs-native clickable link in a pop-up buffer when a code block finishes."
  (let* ((buffer-name (buffer-name))
         (buffer-file (buffer-file-name))
         (line-number (line-number-at-pos))
         (link-text (if buffer-file
                        (format "%s:%d" buffer-file line-number)
                      buffer-name))
         (alerts-buffer-name "*Block Completion Alerts*"))

    (with-current-buffer (get-buffer-create alerts-buffer-name)
      (unless (eq major-mode 'ob-babel-alerts/cell-alerts-mode)
        (ob-babel-alerts/cell-alerts-mode))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (let ((start (point)))
          (insert "\n\n")
          (insert (format-time-string "[%Y-%m-%d %H:%M:%S]\n"))
          (insert "A code block finished at:\n")
          (shell-command (format  "notify-send \"An org code block in %s finished!\"" buffer-name))
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

(defun ob-babel-alerts/notify-if-took-a-while (alert-threshold)
  "Scan through a results block to find a 'Cell Timer:' line and parse the time in seconds."
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
                (+ (* hours 3600) (* minutes 60) seconds)
                (if (>= seconds alert-threshold)
                    (ob-babel-alerts/block-finished-alert)
                  ()))))
        (message "No results block found.")
        nil))))

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
         (results-end (ob-babel-alerts/org-src-block-results-end src-block)))

    (when results-start
      (save-excursion
        (goto-char results-start)
        (if (re-search-forward "^[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}$" results-end t)
            ()
          (progn
            (when alert-finish
              (ob-babel-alerts/block-finished-alert))
            (ob-babel-alerts/notify-if-took-a-while 10)))))))


(advice-add 'org-babel-insert-result :after #'ob-babel-alerts/alert-advice-after-org-babel-results)
;; (setq debug-on-message "Code block evaluation complete\\.")




(provide 'ob-babel-alerts)
;;; ob-babel-alerts.el ends here
