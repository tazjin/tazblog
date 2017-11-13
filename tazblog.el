;; tazjin's blog

(require 'elblog)
(require 'ht)

(defun map-entry (entry)
  "Automatically map an entry to its corresponding org-mode file."
  `(,entry . ,(concat entry ".org")))

(defvar tazblog-entries
  (let ((entries '("reversing-watchguard-vpn"
                   "make-object-t-again"
                   "letsencrypt-controller"
                   "kubernetes-presentatio"
                   "servant-presentation"
                   "the-smu-problem"
                   "end-of-dynlangs"
                   "masterless-puppet"
                   "sick-in-sweden"
                   "nsa-zettabytes")))
    (ht<-alist (mapcar #'map-entry entries))))

;; To ensure that legacy links still work, keep this mapping. Forever.
;; TODO: Maybe it is actually possible to do this mapping as a URL rewrite
;; instead.
(defvar tazblog-legacy-links
  (ht
   ("1375310627" "nsa-zettabytes.org")
   ("1423995834" "sick-in-sweden.org")
   ("1448288135" "masterless-puppet.org")
   ("1448375823" "end-of-dynlangs.org")
   ("1450354078" "the-smu-problem.org")
   ("1455623225" "servant-presentation.org")
   ("1467448422" "kubernetes-presentation.org")
   ("1474235830" "letsencrypt-controller.org")
   ("1476807384" "make-object-t-again.org")
   ("1486830338" "reversing-watchguard-vpn.org")))

(setq elblog-articles
      (ht-merge tazblog-entries tazblog-legacy-links))
(setq elblog-article-directory default-directory)

;; By default elblog will include all entries in the elblog-entries list in the
;; index.
;; Due to the legacy-alias setup above this would be a bit silly, so this
;; returns tazblog-entries directly.
(setq elblog-index-filter-func (lambda (_) tazblog-entries))

