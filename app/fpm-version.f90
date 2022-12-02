program array
use tomlf, only : toml_table, toml_array, get_value, toml_parse, toml_error, len
use fpm_error, only : error_t, syntax_error
use fpm_strings, only : string_t
use fpm_toml, only : toml_table, toml_key, toml_stat, get_value, get_list
implicit none
character(len=128), dimension(:), allocatable  :: arr_data
integer                       :: data_len, io, i, j
integer                       :: argument_count, argument_length, istat
type(toml_table), allocatable :: table
type(toml_array), pointer     :: arr
type(toml_error), allocatable :: parse_error
type(error_t), allocatable    :: error
type(string_t), allocatable   :: values(:)
character(len=:),allocatable  :: string, arg
character(len=256)            :: iomsg
! add call to find fpm.toml file?
  open(newunit=io, file="fpm.toml",iostat=istat,iomsg=iomsg,status='old')
  if(istat.ne.0)then
     stop trim(iomsg)
  endif
  call toml_parse(table, io, parse_error)
  close(unit=io)
  if (allocated(parse_error)) then
    print*, "Error parsing table:"//parse_error%message
  endif
  argument_count = command_argument_count()
  if(argument_count.eq.0)then
    LOOPOVER: do i=1,huge(0)-1
       select case(i)
       case(1);arg="name"
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
contains

subroutine printarg()
   if (allocated(error))deallocate(error)
   call get_list(table, arg, values, error)
   if (allocated(error))then
      write(*,'(*(g0))')'*error* getting ',arg
   elseif(allocated(values))then
      write(*,'(a,t14,": ")',advance='no')arg
      write(*,'(*(a:,",",1x))')( values(j)%s,j=1,size(values) )
   endif
end subroutine printarg

end program
