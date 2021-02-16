(defun mlpost ()
  "launch a remote xpdf for use with mlpost -xpdf"
  (interactive)
  (call-process 
   "xpdf" nil 0 nil
   "-remote" "mlpost" "_mlpost.pdf")
)
