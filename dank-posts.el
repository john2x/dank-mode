(defun dank-posts-render (post)
  "Render POST into the current buffer."
  (insert (plist-get (plist-get post :data) :title))
  (insert "\n"))


(provide 'dank-posts)
