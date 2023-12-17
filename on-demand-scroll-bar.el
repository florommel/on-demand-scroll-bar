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

(defcustom on-demand-scroll-bar-custom-css
  "scrollbar.vertical, scrollbar {
    background: transparent;
}
scrollbar.vertical slider {
    border: 0;
    border-radius: 0;
    min-width: calc(%width - 1px);
}"
  "Custom scroll-bar CSS.
Set `on-demand-scroll-bar-custom-css-enabled' to a non-nil value to enable
CSS styling.  The string \"%width\" is replaced with the value of
`on-demand-scroll-bar-custom-css-width' + \"px\".
Changes to this variable will not take effect until `on-demand-scroll-bar-mode'
is (re-)enabled.
Note that you can also change the color of your scroll bars by customizing
the `scroll-bar' face."
  :type 'string)

(defcustom on-demand-scroll-bar-custom-css-width 5
  "Width of the scroll-bar when custom-css is enabled."
  :type 'integer)

(defcustom on-demand-scroll-bar-custom-css-enabled nil
  "Enable custom scroll-bar CSS.
This requires the `custom-css' library to be installed.
Reenable `on-demand-scroll-bar-mode' to apply changes.
See https://github.com/florommel/custom-css"
  :type 'boolean)

(defvar on-demand-scroll-bar--current-timer nil)

(defun on-demand-scroll-bar--update (&optional force)
  "Update the scroll bars for all windows.
If FORCE is non-nil, set the scroll bars even if they are already present."
  (setq on-demand-scroll-bar--current-timer nil)
  (dolist (frame (frame-list))
    (dolist (window (window-list frame))
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
          (set-window-scroll-bars
           window
           (if (and on-demand-scroll-bar-custom-css-enabled custom-css-available)
               on-demand-scroll-bar-custom-css-width scroll-bar-width)
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
    (dolist (window (window-list frame))
      (when (nth 2 (window-scroll-bars window))
        (set-window-scroll-bars window nil nil)))))

(defun on-demand-scroll-bar--load-custom-css ()
  "Use the `custom-css' library to customize the scroll-bar visuals."
  (when on-demand-scroll-bar-custom-css-enabled
    (if (and (require 'custom-css nil t) custom-css-available)
        (let ((expanded-css
               (replace-regexp-in-string
                "%width"
                (format "%spx" on-demand-scroll-bar-custom-css-width)
                on-demand-scroll-bar-custom-css nil 'literal)))
          (custom-css-load 'on-demand-scroll-bar expanded-css))
      (warn (concat "`on-demand-scroll-bar': Library `custom-css' not found or not working."
                    "  Install it or disable `on-demand-scroll-bar-custom-css-enabled'")))))

(defun on-demand-scroll-bar--unload-custom-css ()
  "Clean up `custom-css' snippet."
  ;; Intentionally no (when on-demand-scroll-bar-custom-css-enabled)
  (when (and (featurep 'custom-css) custom-css-available)
    (custom-css-unload 'on-demand-scroll-bar)))

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
        (on-demand-scroll-bar--load-custom-css)
        (add-hook 'post-command-hook #'on-demand-scroll-bar--maybe-update)
        (add-hook 'buffer-list-update-hook #'on-demand-scroll-bar--maybe-update)
        (add-hook 'window-configuration-change-hook #'on-demand-scroll-bar--update)
        (on-demand-scroll-bar--update t))
    (on-demand-scroll-bar--unload-custom-css)
    (remove-hook 'post-command-hook #'on-demand-scroll-bar--maybe-update)
    (remove-hook 'buffer-list-update-hook #'on-demand-scroll-bar--maybe-update)
    (remove-hook 'window-configuration-change-hook #'on-demand-scroll-bar--update)
    (on-demand-scroll-bar--cleanup)))

(provide 'on-demand-scroll-bar)

;;; on-demand-scroll-bar.el ends here
