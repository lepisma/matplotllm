;;; matplotllm.el --- LLM Assisted Matplotlib -*- lexical-binding: t; -*-

;; Copyright (c) 2023 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;; Version: 0.0.1
;; Package-Requires: ((emacs "27"))
;; Keywords: 
;; URL: https://github.com/lepisma/matplotllm.el

;;; Commentary:

;; LLM Assisted Matplotlib
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'shell-maker)

(defvar matplotllm-shell--config
  (make-shell-maker-config
   :name "MatplotLLM"
   :execute-command
   (lambda (command _history callback error-callback)
     (funcall callback (format "Hello \"%s\"" command) nil))))

(defun matplotllm-shell ()
  "Start MatplotLLM."
  (interactive)
  (shell-maker-start matplotllm-shell--config))

(provide 'matplotllm)

;;; matplotllm.el ends here
