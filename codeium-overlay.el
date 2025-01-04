;;; codeium-overlay --- Extends codeium.el to provide completion suggestions in overlays -*- lexical-binding: t -*-

;;; License:

;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; After installing codeium.el, use M-x `codeium-install' to install binaries automatically, and set up your key.
;; Go to https://github.com/Exafunction/codeium.el for more info.

;; Enable the minor mode `codeium-overlay-mode` to get suggestions in real time that you can accept with TAB.

;; Make sure you are loading codeium before codeium-overlay, for example:

;; (add-to-list 'load-path "~/.emacs.d/elisp/codeium.el")
;; (require 'codeium)

;; (add-to-list 'load-path "~/.emacs.d/elisp")
;; (require 'codeium-overlay)

;;; Code:

(defvar-local codeium-current-state nil
  "The Codeium state.")

(defvar-local codeium-overlay nil
  "The buffer-specific Codeium overlay.")

(defvar-local codeium-overlay-suggested-completion nil
  "The current suggested completion.")

(defvar-local codeium-overlay-timer nil
  "Timer for debouncing completion suggestions.")

(defvar-local codeium-overlay-last-point nil
  "The position of point recorded after the last command.")

(defvar-local codeium-overlay-last-mark nil
  "The position of mark recorded after the last command.")

(defvar codeium-overlay-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "C-c C-s") 'codeium-overlay-suggest-completion)
    ;; (define-key map (kbd "C-c C-a") 'codeium-overlay-accept-suggested-completion)
    ;; (define-key map (kbd "C-c C-r") 'codeium-overlay-reject-suggested-completion)
    map)
  "Keymap for Codeium overlay mode.")

(defun codeium-overlay-get-cursor-offset ()
  (codeium-utf8-byte-length
   (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))

(defun codeium-overlay-get-completion (callback)
  "Get a completion from Codeium and send it to CALLBACK."
  (unless codeium-current-state
    (setq codeium-current-state (codeium-state-make :name (concat "codeium-state-" (buffer-name)))))
  (when
      (and (codeium-state-proc codeium-current-state)
	   (not (process-live-p (codeium-state-proc codeium-current-state))))
    (codeium-reset codeium-current-state))
  (unless (codeium-state-alive-tracker codeium-current-state)
    (codeium-init codeium-current-state))
  (cl-letf*
      (
       ((codeium-config 'codeium/document/text codeium-current-state) (lambda ()
									(buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max)))))
       ((codeium-config 'codeium/document/cursor_offset codeium-current-state) (codeium-overlay-get-cursor-offset))
       ((codeium-config 'codeium-api-enabled codeium-current-state) (lambda (api) (eq api 'GetCompletions)))
       (response (codeium-request-synchronously 'GetCompletions codeium-current-state nil)))
    (when response
      (let ((completion-items (alist-get 'completionItems response)))
	(when (arrayp completion-items)
	  (let*
	      ((first-completion (aref completion-items 0))
	       (completion-data (alist-get 'completion first-completion))
	       (range-data (alist-get 'range first-completion))
	       (text (alist-get 'text completion-data))
	       (start-offset (alist-get 'startOffset range-data))
	       (end-offset (alist-get 'endOffset range-data)))
	    (funcall callback (list :text text
				    :start-offset start-offset
				    :end-offset end-offset))))))))

(defun codeium-overlay-accept-completion (completion)
  (let* ((start-offset (string-to-number (plist-get completion :start-offset)))
         (end-offset (string-to-number (plist-get completion :end-offset)))
         (start-offset-absolute (+ (max (- (point) 3000) (point-min)) start-offset))
         (end-offset-absolute (+ (max (- (point) 3000) (point-min)) end-offset))
         (text (plist-get completion :text)))
    (goto-char start-offset-absolute)
    (delete-region start-offset-absolute end-offset-absolute)
    (insert text)
    (goto-char (+ start-offset-absolute (length text)))))

(defun codeium-overlay-accept-suggested-completion ()
  "Accept a suggested completion."
  (interactive)
  (when codeium-overlay-suggested-completion
    (codeium-overlay-accept-completion codeium-overlay-suggested-completion)
    (setq codeium-overlay-suggested-completion nil))
  (delete-overlay codeium-overlay))

(defun codeium-overlay-reject-suggested-completion ()
  "Reject a suggested completion."
  (interactive)
  (setq codeium-overlay-suggested-completion nil)
  (delete-overlay codeium-overlay))

(defun codeium-overlay-suggest-completion ()
  "Suggest a completion from Codeium with only new text shown in the overlay."
  (interactive)
  (codeium-overlay-track-point-and-mark)
  (codeium-overlay-get-completion
   #'(lambda (completion)
       (let* ((start-offset (string-to-number (plist-get completion :start-offset)))
              (end-offset (string-to-number (plist-get completion :end-offset)))
              (start-offset-absolute (+ (max (- (point) 3000) (point-min)) start-offset))
              (end-offset-absolute (+ (max (- (point) 3000) (point-min)) end-offset))
              (text (plist-get completion :text))
              (existing-text (buffer-substring-no-properties start-offset-absolute end-offset-absolute))
	      (new-text (substring text (length existing-text))))
         (setq codeium-overlay-suggested-completion completion)
         (codeium-overlay-show-completion-in-overlay new-text end-offset-absolute end-offset-absolute)))))

(defun codeium-overlay-show-completion-in-overlay (text start-offset-absolute end-offset-absolute)
  "Show the completion TEXT in the overlay, starting at START-OFFSET-ABSOLUTE and ending at END-OFFSET-ABSOLUTE."
  (when codeium-overlay
    (delete-overlay codeium-overlay))
  (setq codeium-overlay (make-overlay start-offset-absolute end-offset-absolute))
  (overlay-put codeium-overlay 'after-string
               (propertize text 'face '(:foreground "darkgrey"))))

(defun codeium-overlay-after-change-function (&rest _)
  "Function to be called after buffer changes."
  (when codeium-overlay-suggested-completion
    (setq codeium-overlay-suggested-completion nil)
    (delete-overlay codeium-overlay))
  (when codeium-overlay-timer
    (cancel-timer codeium-overlay-timer))
  (setq codeium-overlay-timer
        (run-with-idle-timer 0.5 nil #'codeium-overlay-suggest-completion)))

(defun codeium-overlay-tab-command ()
  "Custom TAB command for Codeium overlay mode."
  (interactive)
  (if codeium-overlay-suggested-completion
      (codeium-overlay-accept-suggested-completion)
    (call-interactively (global-key-binding (kbd "TAB")))))

(defun codeium-overlay-track-point-and-mark ()
  "Reject suggestion if point or mark position changes. Track the current point and mark positions."
  (when (or (not (eq (point) codeium-overlay-last-point))
            (not (eq (mark t) codeium-overlay-last-mark)))
    (when codeium-overlay-suggested-completion
      (codeium-overlay-reject-suggested-completion)))
  (setq codeium-overlay-last-point (point))
  (setq codeium-overlay-last-mark (mark t)))

(define-minor-mode codeium-overlay-mode
  "A minor mode that shows Codeium completions as an overlay."
  :init-value nil
  :lighter " Codeium (ov)"
  :keymap codeium-overlay-mode-map
  (if codeium-overlay-mode
      (progn
        (unless codeium-current-state
          (setq codeium-current-state (codeium-state-make :name (concat "codeium-state-" (buffer-name)))))
        (add-hook 'after-change-functions #'codeium-overlay-after-change-function nil t)
        (add-hook 'post-command-hook #'codeium-overlay-track-point-and-mark nil t)
	(keymap-set codeium-overlay-mode-map "TAB" #'codeium-overlay-tab-command))
    (codeium-overlay-reject-suggested-completion)
    (remove-hook 'after-change-functions #'codeium-overlay-after-change-function t)
    (remove-hook 'post-command-hook #'codeium-overlay-track-point-and-mark t)
    (keymap-unset codeium-overlay-mode-map "TAB")))

(provide 'codeium-overlay)
;;; codeium-overlay.el ends here.
