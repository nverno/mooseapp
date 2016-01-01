;;; org-vals.el --- 
;; Filename: org-vals.el
;; Description: Org publish options
;; Author: Noah Peart
;; Created: Mon Oct 26 21:13:26 2015 (-0400)
;; Last-Updated: Thu Dec 31 18:25:24 2015 (-0500)
;;           By: Noah Peart
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ox-publish)

(setq projdir
      (cond
       ((string-equal system-type "windows-nt")
	"C:/home/work/mooseapp/inst/app/org/")
       (t "~/work/mooseapp/inst/app/org")))
(setq htmldir
      (cond
       ((string-equal system-type "windows-nt")
	"C:/home/work/mooseapp/inst/app/www/org/html/")
       (t "~/work/mooseapp/inst/app/www/org/html/")))
(setq theme-file "theme-bigblow.setup")
(setq preamble (prep-org (get-string-from-file theme-file)))

(setq org-publish-project-alist
      `(
	("orgfiles"
	 :auto-sitemap t
	 :html-head ,preamble
	 :sitemap-title "Sitemap"
	 :base-directory ,projdir
	 :base-extenstion "org"
	 :publishing-directory ,htmldir
	 :publishing-function org-html-publish-to-html
	 :recursive t
	 :html-link-home "sitemap.html"
	 :auto-preamble t)

	("mooseapp" :components ("orgfiles"))))




