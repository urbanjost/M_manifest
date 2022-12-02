program main
use M_io,        only : gulp
use fpm_strings, only : join
use tomlf,       only : toml_parse
use fpm_toml,    only : toml_table, toml_serializer
use fpm_manifest_package
implicit none
integer,parameter                       :: tfc = selected_char_kind('DEFAULT')
character(len=1), parameter             :: nl = new_line('a')
character(kind=tfc,len=:),allocatable   :: input(:)
type(toml_table), allocatable           :: table
character(kind=tfc, len=:), allocatable :: joined_string
type(toml_serializer)                   :: ser

   call gulp("fpm.toml",input)
   if(.not.allocated(input))then
       write(*,*)'*gulp* failed to load file '
       stop
   endif
   ! you have to add a newline character by using the intrinsic
   ! function `new_line("a")` to get the lines processed correctly.
   joined_string = join(input,right=nl)
   if (allocated(table)) deallocate(table)
   call toml_parse(table, joined_string)
   if (allocated(table)) then
      ! If the TOML file is successfully parsed the table will be allocated
      call table%accept(ser)
   endif
   call table%destroy

end program main
