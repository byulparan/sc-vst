(asdf/defsystem:defsystem #:sc-vst
  :name "sc-vst"
  :author "Park Sungmin. byulparan@gmail.com"
  :description "additional library collection for cl-collider"
  :licence "Public Domain / 0-clause MIT"
  :serial t
  :depends-on (#:cl-collider
	       #:temporary-file)
  :components ((:file "package")
	       (:file "sc-vst")))
