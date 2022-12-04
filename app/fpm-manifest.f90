program main
! use toml_key to get keynames?
! can give io unit instead of data in gulp
use M_io,        only : gulp
use fpm_strings, only : join, string_t
use tomlf,       only : toml_parse
use fpm_toml,    only : toml_table, toml_serializer, get_value, get_list
use fpm_manifest_package
use fpm_error, only : error_t
implicit none
integer,parameter                       :: tfc = selected_char_kind('DEFAULT')
integer                                 :: i
character(len=1), parameter             :: nl = new_line('a')
character(len=*), parameter             :: g0='(g0)'
character(kind=tfc,len=:),allocatable   :: input(:)
type(toml_table), allocatable           :: table
character(kind=tfc, len=:), allocatable :: joined_string
character(len=:),allocatable            :: name, version(:), license(:), author(:), maintainer(:), copyright(:)
character(len=:),allocatable            :: description(:), categories(:), keywords(:), homepage(:)
character(len=:),allocatable            :: textblock(:)
character(len=:),allocatable            :: arg

   call gulp("fpm.toml",input)
   if(.not.allocated(input))then
       write(*,*)'*gulp* failed to load file '
       stop
   endif
!type(package_config_t)                           :: package
   !call package%info(unit, verbosity)

   ! you have to add a newline character by using the intrinsic
   ! function `new_line("a")` to get the lines processed correctly.
   joined_string = join(input,right=nl)

   if (allocated(table)) deallocate(table)
   call toml_parse(table, joined_string)
   if (allocated(table)) then
      ! If the TOML file is successfully parsed the table will be allocated
   
      ! ASSUME EXPANDED STRING FITS ON LINE AND IS SCALAR, NOT ARRAY
      call get_value(table, "name",  name )
      if(.not.allocated(name)) name=''
      write(*,g0)'module '//trim(name)//'_manifest','implicit none','private'
      write(*,g0) 'character(len=*),parameter,public :: manifest_name(*) = [character(len=128) :: "'//name//'"]'
   
      LOOPOVER: do i=1,huge(0)
       select case(i)
       case(1);cycle LOOPOVER
       case(2);arg="version"
       case(3);arg="license"
       case(4);arg="author"
       case(5);arg="maintainer"
       case(6);arg="copyright"
       case(7);arg="description"
       case(8);arg="categories"
       case(9);arg="keywords"
       case(10);arg="homepage"
       case default
          exit LOOPOVER
       end select
       call printarg()
      enddo LOOPOVER
   endif
   write(*,g0) 'character(len=*),parameter,public :: manifest_date(*) = [character(len=128) :: "'//get_now()//'"]'

textblock=[ CHARACTER(LEN=128) :: &
'public :: manifest_edition',&
'contains',&
'subroutine manifest_edition()',&
'character(len=*),parameter :: g=''(*(g0,1x))''',&
'integer :: i',&
'',&
'if(size(manifest_name).ne.0)        print g, "project      :",(trim(manifest_name(i)),i=1,size(manifest_name))',&
'if(get_name().ne.'''')              print g, "name         :",get_name()',&
'if(size(manifest_version).ne.0)     print g, "version      :",(trim(manifest_version(i)),i=1,size(manifest_version))',&
'if(size(manifest_license).ne.0)     print g, "license      :",(trim(manifest_license(i)),i=1,size(manifest_license))',&
'if(size(manifest_maintainer).ne.0)  print g, "maintainer   :",(trim(manifest_maintainer(i)),i=1,size(manifest_maintainer))',&
'if(size(manifest_author).ne.0)      print g, "author       :",(trim(manifest_author(i)),i=1,size(manifest_author))',&
'if(size(manifest_copyright).ne.0)   print g, "copyright    :",(trim(manifest_copyright(i)),i=1,size(manifest_copyright))',&
'if(size(manifest_description).ne.0) print g, "description  :",(trim(manifest_description(i)),i=1,size(manifest_description))',&
'if(size(manifest_categories).ne.0)  print g, "categories   :",(trim(manifest_categories(i)),i=1,size(manifest_categories))',&
'if(size(manifest_keywords).ne.0)    print g, "keywords     :",(trim(manifest_keywords(i)),i=1,size(manifest_keywords))',&
'if(size(manifest_homepage).ne.0)    print g, "homepage     :",(trim(manifest_homepage(i)),i=1,size(manifest_homepage))',&
'if(size(manifest_date).ne.0)        print g, "version date :",(trim(manifest_date(i)),i=1,size(manifest_date))',&
'if(get_now().ne.'''')               print g, "current date :",get_now()',&
'end subroutine manifest_edition',&
'',&
'function get_name() result(name)',&
'!>   get_name(3f) - get name of the current executable (LICENSE:PD)',&
'implicit none',&
'character(len=:),allocatable :: arg0',&
'integer                      :: arg0_length',&
'integer                      :: ios',&
'character(len=4096)          :: long_name',&
'character(len=:),allocatable :: name',&
'   arg0_length=0',&
'   name=''''',&
'   long_name=''''',&
'   call get_command_argument(0,length=arg0_length,status=ios)',&
'   if(ios.eq.0)then',&
'      if(allocated(arg0))deallocate(arg0)',&
'      allocate(character(len=arg0_length) :: arg0)',&
'      call get_command_argument(0,arg0,status=ios)',&
'      if(ios.eq.0)then',&
'         inquire(file=arg0,iostat=ios,name=long_name)',&
'         if(ios == 0)then',&
'            name=trim(long_name)',&
'         else',&
'            name=arg0',&
'         endif',&
'      else',&
'         arg0=''''',&
'      endif',&
'   else',&
'      arg0=''''',&
'   endif',&
'end function get_name',&
'',&
'function get_now() result(datetime)',&
'!>   get_now(3f) - datetime in form !YYYY-MM-DD HH:MM:SS UTC-ZZZZ (LICENSE:PD)',&
'integer           :: values(8)',&
'character(len=50) :: datetime',&
'call date_and_time(values=values)',&
'write(datetime,&',&
'   & ''(i4.4,"-",i2.2,"-",i2.2," ",i2.2,":",i2.2,":",i2.2," UTC",SP,i5.4)'') &',&
'   & values(1:3),values(5:7),values(4)',&
'',&
'end function get_now',&
'']
   write(*,g0)textblock
   write(*,g0)'end module '//trim(name)//'_manifest'
   write(*,g0)'!program testit'
   write(*,g0)'!use '//trim(name)//'_manifest, only : manifest_edition'
   write(*,g0)'!call manifest_edition()'
   write(*,g0)'!end program testit'
   call table%destroy

contains

function get_now() result(datetime)
!>   get_now(3f) - datetime in form !YYYY-MM-DD HH:MM:SS UTC-ZZZZ (LICENSE:PD)
integer           :: values(8)
character(len=28) :: datetime
call date_and_time(values=values)
write(datetime,&
   & '(i4.4,"-",i2.2,"-",i2.2," ",i2.2,":",i2.2,":",i2.2," UTC",SP,i5.4)') &
   & values(1:3),values(5:7),values(4)

end function get_now

subroutine printarg()
type(error_t), allocatable    :: error
type(string_t), allocatable   :: values(:)
integer                       :: j
   if (allocated(error))deallocate(error)
   call get_list(table, arg, values, error)
   if (allocated(error))then
      write(*,'(*(g0))')'*error* getting ',arg
   elseif(allocated(values))then
      write(*,g0) 'character(len=*),parameter,public :: manifest_'//arg//'(*) = [character(len=128) :: &'
      write(*,'(*(""""a,"""":,",",1x))',advance='no')( values(j)%s,j=1,size(values) )
      write(*,'("]")')
   else
      write(*,'(a,"(*) = [character(len=128)::]")')arg
   endif
end subroutine printarg

end program main
