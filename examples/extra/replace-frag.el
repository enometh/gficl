;;  -*- lexical-binding: t -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Tue Aug 12 21:54:32 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;

(defun %gficl-replace-frag (string)
  (sly-eval-async `(gficl-examples/shadertoy::replace-frag
		    (cl:elt gficl-app::*apps* 0)
		    ,string)
      (lambda (result)
	(message "%%gficl-replace frag returned %S" result))))

(defvar gficl-replace-frag-function '%gficl-replace-frag)

(defun gficl-replace-frag (&optional beg end)
  "Treat the region as a fragment shader and load it into the current
gficl app which should be running an instance of
gficl-examples/shadertoy:shadertoy-app."
  (interactive)
  (let ((beg (or beg (if (region-active-p) (region-beginning) (point-min))))
	(end (or end (if (region-active-p) (region-end) (point-max)))))
    (funcall gficl-replace-frag-function (buffer-substring-no-properties beg end))))


;; ;madhu 250817 glsl-mode integration with org-babel.  (require
;; 'glsl-mode) this lets you C-c c-c to send your current org-mode
;; "glsl" code block to the currently running gficl shadertoy.

(defvar org-babel-default-header-args:glsl '())
(defvar org-babel-header-args:glsl '())

(defun org-babel-execute:glsl (body _params)
  (funcall gficl-replace-frag-function body))
