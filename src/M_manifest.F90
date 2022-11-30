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
character(len=*),parameter,public :: manifest_name1 ='&
&NAME1&
&'
character(len=*),parameter,public :: manifest_value1 ='&
&VALUE1&
&'
character(len=*),parameter,public :: manifest_name2 ='&
&NAME2&
&'
character(len=*),parameter,public :: manifest_value2 ='&
&VALUE2&
&'

public :: get_name
public :: get_now
public :: manifest_edition
contains
subroutine manifest_edition()
character(len=*),parameter :: g='(*(g0,1x))'

if(manifest_name.ne.'')        print g, "project      :",manifest_name
if(get_name().ne.'')           print g, "name         :",get_name()
if(manifest_version.ne.'')     print g, "version      :",manifest_version
if(manifest_license.ne.'')     print g, "license      :",manifest_license
if(manifest_maintainer.ne.'')  print g, "maintainer   :",manifest_maintainer
if(manifest_author.ne.'')      print g, "author       :",manifest_author
if(manifest_copyright.ne.'')   print g, "copyright    :",manifest_copyright
if(manifest_description.ne.'') print g, "description  :",manifest_description
if(manifest_categories.ne.'')  print g, "categories   :",manifest_categories
if(manifest_keywords.ne.'')    print g, "keywords     :",manifest_keywords
if(manifest_homepage.ne.'')    print g, "homepage     :",manifest_homepage
if(manifest_value1.ne.'')      print g, "value1       :",manifest_value1
if(manifest_value2.ne.'')      print g, "value2       :",manifest_value2
if(get_now().ne.'')            print g, "current time :",get_now()
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
