(package.seed:define-seed-package :string.seed :export-capitalized t)

(in-package :string.seed)

(defun Escape-string (string characters-to-escape escape-char)
  "Escapes CHARACTERS-TO-ESCAPE in STRING with ESCAPE-CHAR.

Examples:

  (escape-string \"abc\\\"abc\" \"\\\"\" #\\\\)
  => \"abc\\\\\\\"abc\"

  (escape-string \"foo,b!ar\" \"!,\" #\\\\)
  => \"foo\\\\,b\\\\!ar\"

  (escape-string \"abc'abc\" \"'\" #\\')
  => \"abc''abc\""
  (let* ((escape-count 0)
         (new-len 0)
         (new-string))
    (loop :for c :across string
          :do (cond ((find c characters-to-escape)
                     (incf escape-count)
                     (incf new-len 2))
                    (t (incf new-len))))
    (if (zerop escape-count)
        (return-from escape-string string)
        (setf new-string (make-string new-len)))
    (loop :for c :across string
          :for i :from 0
          :do (cond ((find c characters-to-escape)
                     (setf (aref new-string i) escape-char)
                     (incf i)
                     (setf (aref new-string i) c))
                    (t (setf (aref new-string i) c))))
    new-string))

#+self-test.seed
(self-test.seed:define-self-test escape-string
  (equal (escape-string "foo,bar" "," #\\) "foo\\,bar")
  (equal (escape-string "foo,bar" "!" #\\) "foo,bar")
  (equal (escape-string "foo,b!ar" "!," #\\) "foo\\,b\\!ar")
  (equal (escape-string "foo\\bar" "\\" #\\) "foo\\\\bar"))

(defun Split-string-escaped (seq delimiter escape-char)
  (let ((len (length seq))
        (temp)
        (result))
    (flet ((collect ()
             (push (coerce (nreverse temp) (class-of seq)) result)
             (setf temp nil)))
      (loop :for k :from 0 :to (1- len)
            :for c = (aref seq k)
            :while (< k len)
            :do (cond ((eql c delimiter)
                       (collect))
                      ((eql c escape-char)
                       (if (= k (1- len)) ;; last element
                           ;; Ignore
                           (collect)
                           (progn
                             (incf k)
                             (push (aref seq k) temp))))
                      (t (push c temp)))
            :finally (collect))
      (nreverse result))))

#+self-test.seed
(self-test.seed:define-self-test split-string-escaped
  (equal (split-string-escaped "foo,bar," #\, #\\)
         '("foo" "bar" ""))
  (equal (split-string-escaped ",foo\\,bar,qwe," #\, #\\)
         '("" "foo,bar" "qwe" "")))
