;;; matplotllm.el --- LLM Assisted Matplotlib -*- lexical-binding: t; -*-

;; Copyright (c) 2023 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28") (org "9.6.6") (request "0.3.2") (s "1.13.0"))
;; Keywords: 
;; URL: https://github.com/lepisma/matplotllm

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

(require 'ob)
(require 'request)
(require 's)

(defcustom matplotllm-image-filename "matplotllm.png"
  "Filename where the plotting code should put the image in.")

(defcustom matplotllm-openai-key nil
  "OpenAI key for calling LLMs.")

(defcustom matplotllm-system-message
  "You have to produce complete code for plotting a graph using
matplotlib which should save the image in a file matplotllm.png
when run. Don't call plt.show() in the end. Also you don't have
to explain anything, just write the code which can be executed.
No extra comments of any sort. Provide output in markdown.

You will be given the description of the data source that you
have to use. Note that if the description talks about a filename
just hard code reading from file in the code and don't read
anything from the command line."
  "System prompt message for use in OpenAI requests.")

(defcustom matplotllm-user-message-template
  "Data Description: %s

Ask: %s"
  "Prompt template to be used while asking for code.")

(defvar matplotllm-file-name "matplotllm.py"
  "Name of the file to dump the code in.")

(defun matplotllm-parse-code (llm-response)
  "Return Python code from LLM response."
  (with-temp-buffer
    (insert llm-response)
    (goto-char (point-min))
    (let (start end)
      (re-search-forward "^```python$")
      (setq start (match-end 0))
      (re-search-forward "^```$")
      (setq end (match-beginning 0))
      (buffer-substring-no-properties start end))))

(defun matplotllm-run (code)
  "Run given python `code' snippet. It's assumed that this snippet
is complete code to plot the desired graphics."
  (with-temp-buffer
    (insert code)
    (write-file matplotllm-file-name))
  (call-process "python" nil "*matplotllm*" nil matplotllm-file-name))

(defun matplotllm-openai-request (system-message user-message callback)
  "Simple request function for OpenAI LLMs that uses given messages
and runs `callback' on the first LLM response."
  (request "https://api.openai.com/v1/chat/completions"
    :type "POST"
    :data (json-encode `(("model" . "gpt-4")
                         ("messages" . [(("role" . "system") ("content" . ,system-message))
                                        (("role" . "user") ("content" . ,user-message))])))
    :headers `(("Content-Type" . "application/json")
               ("Authorization" . ,(format "Bearer %s" matplotllm-openai-key)))
    :parser 'json-read
    :error (cl-function
            (lambda (&rest args &key error-thrown &allow-other-keys)
              (message "Got error: %S" error-thrown)))
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let ((llm-response (alist-get 'content (alist-get 'message (aref (alist-get 'choices data) 0)))))
                  (funcall callback llm-response))))))

(defun matplotllm-request (data-description ask callback)
  "Send request to an LLM with requirements and get output back.
The output is raw LLM generation and will need parsing to strip
non-code portions as needed."
  (matplotllm-openai-request matplotllm-system-message (format matplotllm-user-message-template data-description ask) callback))

(defun matplotllm-parse-babel-block (body)
  "Return data and plot description in a list org-babel `body'
text. These two are separated by an org line break `-----`.
Further iterative plot descriptions are separated by one or more
empty lines."
  (let ((sections (s-split "-----" body)))
    (unless (= (length sections) 2)
      (error "Ill-formed matplotllm description, please separate data and plot description with a `-----' separator."))
    (cons (s-trim (car sections)) (mapcar #'s-trim (s-split "\n\n" (cadr sections))))))

(defun matplotllm-summarize-iterative-description (texts)
  "Summarize a sequence of texts describing a specification given
in iterative conversational way. For now this is just
concatenation but will use another LLM call."
  (s-join "\n\n" texts))

(defun org-babel-execute:matplotllm (body params)
  "Execute matplotllm description and generate plots."
  (let ((desc (matplotllm-parse-babel-block body)))
    (matplotllm-request (car desc) (matplotllm-summarize-iterative-description (cdr desc))
                        (lambda (response)
                          (matplotllm-run (matplotllm-parse-code response))
                          (org-redisplay-inline-images))))
  matplotllm-image-filename)

(provide 'matplotllm)

;;; matplotllm.el ends here
