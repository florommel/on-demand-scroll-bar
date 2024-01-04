;;; on-demand-scroll-bar.el --- Show scroll bars on demand -*- lexical-binding: t -*-

;; Copyright (C) 2023 Florian Rommel

;; Author: Florian Rommel <mail@florommel.de>
;; Maintainer: Florian Rommel <mail@florommel.de>
;; Url: https://github.com/florommel/on-demand-scroll-bar
;; Created: 2023-12-17
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: frames scrolling

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Show scroll bars only when needed.
;;
;; Enable `on-demand-scroll-bar-mode' to only show scroll bars when
;; the buffer is not fully visible.

;;; Code:

(defgroup on-demand-scroll-bar nil
  "Show scroll bars on demand."
  :group 'scrolling)

(defcustom on-demand-scroll-bar-exclude-major-modes nil
  "Major modes in which scroll-bars schould not be shown."
  :type '(repeat symbol))

(defcustom on-demand-scroll-bar-exclude-minor-modes nil
  "Minor modes in which scroll-bars schould not be shown."
  :type '(repeat symbol))

(defvar on-demand-scroll-bar--current-timer nil)

(defun on-demand-scroll-bar--update (&optional force)
  "Update the scroll bars for all windows.
If FORCE is non-nil, set the scroll bars even if they are already present."
  (setq on-demand-scroll-bar--current-timer nil)
  (dolist (frame (frame-list))
    (dolist (window (window-list frame t))
      (let* ((buf (window-buffer window))
             (bar-pos
              (when (with-current-buffer buf
                      (and (not (and (= (window-start window) (point-min))
                                     (= (window-end window) (point-max))))
                           (not (apply 'derived-mode-p
                                       on-demand-scroll-bar-exclude-major-modes))
                           (not (seq-find (lambda (m)
                                            (and (buffer-local-boundp m buf)
                                                 (buffer-local-value m buf)))
                                          on-demand-scroll-bar-exclude-minor-modes))))
                'right)))
        (when (or force (not (eq bar-pos (nth 2 (window-scroll-bars window)))))
          (set-window-scroll-bars window
                                  (with-current-buffer buf scroll-bar-width)
                                  bar-pos)
          (force-window-update window))))))

(defun on-demand-scroll-bar--maybe-update ()
  "Update scroll bars if Emacs is idle."
  (unless on-demand-scroll-bar--current-timer
    (setq on-demand-scroll-bar--current-timer
          (run-with-idle-timer 0 nil #'on-demand-scroll-bar--update))))

(defun on-demand-scroll-bar--cleanup ()
  "Clean up scroll bars and timers."
  (when on-demand-scroll-bar--current-timer
    (cancel-timer on-demand-scroll-bar--current-timer)
    (setq on-demand-scroll-bar--current-timer nil))
  (dolist (frame (frame-list))
    (dolist (window (window-list frame t))
      (when (nth 2 (window-scroll-bars window))
        (set-window-scroll-bars window nil nil)))))

;;;###autoload
(define-minor-mode on-demand-scroll-bar-mode
  "Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When `on-demand-scroll-bar-mode' is enabled, the scroll-bar is
shown only if the buffer is not fully visible.
Conflicts with `scroll-bar-mode'."
  :init-value nil
  :group 'on-demand-scroll-bar
  :global t
  (if on-demand-scroll-bar-mode
      (progn
        (add-hook 'post-command-hook #'on-demand-scroll-bar--maybe-update)
        (add-hook 'buffer-list-update-hook #'on-demand-scroll-bar--maybe-update)
        (add-hook 'window-configuration-change-hook #'on-demand-scroll-bar--update)
        (on-demand-scroll-bar--update t))
    (remove-hook 'post-command-hook #'on-demand-scroll-bar--maybe-update)
    (remove-hook 'buffer-list-update-hook #'on-demand-scroll-bar--maybe-update)
    (remove-hook 'window-configuration-change-hook #'on-demand-scroll-bar--update)
    (on-demand-scroll-bar--cleanup)))

(provide 'on-demand-scroll-bar)

;;; on-demand-scroll-bar.el ends here
