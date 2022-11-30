program main
use M_manifest, only : manifest_name, manifest_version, manifest_license 
use M_manifest, only : manifest_maintainer, manifest_author, manifest_copyright
use M_manifest, only : manifest_description 
use M_manifest, only : manifest_categories, manifest_keywords 
use M_manifest, only : manifest_homepage, manifest_targets 
use M_manifest, only : manifest_edition
implicit none

   print *, "name:",manifest_name
   print *, "version:",manifest_version
   print *, "license:",manifest_license
   print *, "maintainer:",manifest_maintainer
   print *, "author:",manifest_author
   print *, "copyright:",manifest_copyright
   print *, "description:",manifest_description
   print *, "categories:",manifest_categories
   print *, "keywords:",manifest_keywords
   print *, "homepage:",manifest_homepage
   print *, "targets:",manifest_targets
   call manifest_edition()
end program main
