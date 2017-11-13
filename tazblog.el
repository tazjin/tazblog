;; tazjin's blog.
;; -*- lexical-binding: t; -*-

(require 'elblog)
(require 'ht)

;; Blog entry configuration

(defun map-entry (entry)
  "Automatically map an entry to its corresponding org-mode file."
  `(,entry . ,(concat entry ".org")))

(setq elblog-articles
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

(setq elblog-article-directory default-directory)

;; Legacy link setup

;; To ensure that legacy links still work, keep this mapping. Forever.
(defvar tazblog-legacy-ids
  (ht
   ("1375310627" "nsa-zettabytes")
   ("1423995834" "sick-in-sweden")
   ("1448288135" "masterless-puppet")
   ("1448375823" "end-of-dynlangs")
   ("1450354078" "the-smu-problem")
   ("1455623225" "servant-presentation")
   ("1467448422" "kubernetes-presentation")
   ("1474235830" "letsencrypt-controller")
   ("1476807384" "make-object-t-again")
   ("1486830338" "reversing-watchguard-vpn")))

(defun legacy-link-handler (httpcon)
  "Redirect legacy links (/en/{article-id}) to new format (/{article-name})."
  (letrec ((legacy-id (elnode-http-mapping httpcon 1))
           (article-name (ht-get tazblog-legacy-ids legacy-id)))
    (message "legacy handler called for %s" article-name)
    (elnode-send-redirect
     httpcon (concat "/" (or article-name "not-found")))))

(setq elblog-additional-routes
      '(("^.*//en/\\(.*\\)$" . legacy-link-handler)))
