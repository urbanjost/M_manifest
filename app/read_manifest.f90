program main
use M_io,      only : gulp
use fpm_manifest_package
use fpm_strings, only : join
implicit none
character(len=:),allocatable :: pageout(:) ! array to hold file in memory

   call gulp("fpm.toml",pageout)
   if(.not.allocated(pageout))then
       write(*,*)'*demo_gulp* failed to load file '
   else
       call interrogate_toml_data(pageout)
   endif
   deallocate(pageout)  ! release memory
contains
subroutine interrogate_toml_data(input)
!> verify a string array is a valid fpm.toml file
use tomlf, only : toml_parse
use fpm_toml, only : toml_table, toml_serializer, get_value
implicit none
integer,parameter        :: tfc = selected_char_kind('DEFAULT')
character(len=1), parameter                      :: nl = new_line('a')
character(kind=tfc,len=:),intent(in),allocatable :: input(:)
type(toml_table), allocatable                    :: table
character(kind=tfc, len=:), allocatable          :: joined_string
type(toml_serializer)                            :: ser
type(package_config_t)                           :: package
character(kind=tfc,len=:),allocatable            :: strings(:)
character(kind=tfc,len=20),allocatable           :: query(:)
character(kind=tfc,len=:),allocatable            :: string
integer                                          :: i
!call package%info(unit, verbosity)

! you have to add a newline character by using the intrinsic
! function `new_line("a")` to get the lines processed correctly.
joined_string = join(input,right=nl)

if (allocated(table)) deallocate(table)
call toml_parse(table, joined_string)
if (allocated(table)) then
   ! If the TOML file is successfully parsed the table will be allocated 
   query=[character(len=20) :: &
      "name" , &
      "version" , &
      "license" , &
      "author" , &
      "maintainer" , &
      "copyright" , &
      "description" , &
      "categories" , &
      "keywords" , &
      "homepage" , &
      "notthere"]
   do i=1,size(query)
      call get_value(table, query(i), string)
      if(allocated(string)) write(*,*)query(i)//':'//string
   enddo
   ! can be written to the standard output by passing the `toml_serializer`
   ! as visitor to the table.
   call table%accept(ser)
   call table%destroy
endif

end subroutine interrogate_toml_data

end program main
