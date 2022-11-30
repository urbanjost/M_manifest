module M_manifest
implicit none
private
character(len=*),parameter,public :: manifest_name ='&
&NAME&
&'
character(len=*),parameter,public :: manifest_version ='&
&VERSION&
&'
character(len=*),parameter,public :: manifest_license ='&
&LICENSE&
&'
character(len=*),parameter,public :: manifest_maintainer ='&
&MAINTAINER&
&'
character(len=*),parameter,public :: manifest_author ='&
&AUTHOR&
&'
character(len=*),parameter,public :: manifest_copyright ='&
&COPYRIGHT&
&'
character(len=*),parameter,public :: manifest_description ='&
&DESCRIPTION&
&'
character(len=*),parameter,public :: manifest_categories ='&
&CATEGORIES&
&'
character(len=*),parameter,public :: manifest_keywords ='&
&KEYWORDS&
&'
character(len=*),parameter,public :: manifest_homepage ='&
&HOMEPAGE&
&'
character(len=*),parameter,public :: manifest_targets ='&
&TARGETS&
&'

public :: get_name
public :: get_now
public :: manifest_edition
contains
subroutine manifest_edition()
character(len=*),parameter :: g='(*(g0,1x))'

   print g, "project      :",manifest_name
   print g, "name         :",get_name()
   print g, "version      :",manifest_version
   print g, "license      :",manifest_license
   print g, "maintainer   :",manifest_maintainer
   print g, "author       :",manifest_author
   print g, "copyright    :",manifest_copyright
   print g, "description  :",manifest_description
   print g, "categories   :",manifest_categories
   print g, "keywords     :",manifest_keywords
   print g, "homepage     :",manifest_homepage
   print g, "targets      :",manifest_targets
   print g, "current time :",get_now()
end subroutine manifest_edition

function get_name() result(name)
!>   get_name(3f) - get name of the current executable (LICENSE:PD)
implicit none
character(len=:),allocatable :: arg0
integer                      :: arg0_length
integer                      :: ios
character(len=4096)          :: long_name
character(len=:),allocatable :: name
   arg0_length=0
   name=''
   long_name=''
   call get_command_argument(0,length=arg0_length,status=ios)
   if(ios.eq.0)then
      if(allocated(arg0))deallocate(arg0)
      allocate(character(len=arg0_length) :: arg0)
      call get_command_argument(0,arg0,status=ios)
      if(ios.eq.0)then
         inquire(file=arg0,iostat=ios,name=long_name)
         if(ios == 0)then
            name=trim(long_name)
         else
            name=arg0
         endif
      else
         arg0=''
      endif
   else
      arg0=''
   endif
end function get_name

function get_now() result(datetime)
!>   get_now(3f) - datetime in form !YYYY-MM-DD HH:MM:SS UTC-ZZZZ (LICENSE:PD)
integer           :: values(8)
character(len=50) :: datetime
call date_and_time(values=values)
write(datetime,&
   & '(i4.4,"-",i2.2,"-",i2.2," ",i2.2,":",i2.2,":",i2.2," UTC",SP,i5.4)') &
   & values(1:3),values(5:7),values(4)

end function get_now

end module M_manifest
