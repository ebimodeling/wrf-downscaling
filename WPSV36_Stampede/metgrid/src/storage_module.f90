module storage_module

   use datatype_module
   use minheap_module
   use misc_definitions_module
   use module_debug
   use parallel_module

   ! Maximum umber of words to keep in memory at a time
   ! THIS MUST BE AT LEAST AS LARGE AS THE SIZE OF THE LARGEST ARRAY TO BE STORED
   integer, parameter :: MEMSIZE_MAX = 1E9

   ! Name (when formatted as i9.9) of next file to be used as array storage
   integer :: next_filenumber = 1

   ! Time counter used by policy for evicting arrays to Fortran units
   integer :: global_time = 0

   ! Current memory usage of module
   integer :: memsize = 0

   ! Primary head and tail pointers
   type (head_node), pointer :: head => null()
   type (head_node), pointer :: tail => null()

   ! Pointer for get_next_output_fieldname
   type (head_node), pointer :: next_output_field  => null()

   contains

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: storage_init
   !
   ! Purpose: Initialize the storage module.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine storage_init()

      implicit none

      call init_heap()

   end subroutine storage_init


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: reset_next_field
   !
   ! Purpose: Sets the next field to the first available field
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine reset_next_field()

      implicit none

      next_output_field => head

   end subroutine reset_next_field


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: storage_put_field
   !
   ! Purpose: Stores an fg_input type. Upon return, IT MUST NOT BE ASSUMED that 
   !      store_me contains valid data, since all such data may have been written 
   !      to a Fortran unit
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine storage_put_field(store_me)

      implicit none

      ! Arguments
      type (fg_input), intent(in) :: store_me

      ! Local variables
      integer :: funit
      logical :: is_used
      character (len=64) :: fname
      type (head_node), pointer :: name_cursor
      type (data_node), pointer :: data_cursor
      type (data_node), pointer :: newnode
      type (data_node), pointer :: evictnode

      !CWH Initialize local pointer variables
      nullify(evictnode)       !MGD initialization for evictnode should not be necessary

      ! We'll first see if there is already a list for this fieldname
      name_cursor => head
      do while (associated(name_cursor))
         if (primary_cmp(name_cursor%fg_data, store_me) == EQUAL) exit 
         name_cursor => name_cursor%next
      end do

      ! If not, create a new node in the primary list
      if (.not. associated(name_cursor)) then
         allocate(name_cursor)
         call dup(store_me, name_cursor%fg_data)
         nullify(name_cursor%fg_data%r_arr)
         nullify(name_cursor%fg_data%valid_mask)
         nullify(name_cursor%fg_data%modified_mask)
         nullify(name_cursor%fieldlist_head)
         nullify(name_cursor%fieldlist_tail)
         nullify(name_cursor%prev)
         name_cursor%next => head
         if (.not. associated(head)) tail => name_cursor
         head => name_cursor
      else
         if ((name_cursor%fg_data%header%time_dependent .and. .not. store_me%header%time_dependent) .or. &
             (.not. name_cursor%fg_data%header%time_dependent .and. store_me%header%time_dependent)) then
            call mprintf(.true.,ERROR,'Cannot combine time-independent data with '// &
                         'time-dependent data for field %s',s1=store_me%header%field)
         end if
      end if

      ! At this point, name_cursor points to a valid head node for fieldname
      data_cursor => name_cursor%fieldlist_head
      do while ( associated(data_cursor) )
         if ((secondary_cmp(store_me, data_cursor%fg_data) == LESS) .or. &
             (secondary_cmp(store_me, data_cursor%fg_data) == EQUAL)) exit 
         data_cursor => data_cursor%next
      end do

      if (associated(data_cursor)) then
         if (secondary_cmp(store_me, data_cursor%fg_data) == EQUAL) then 
            if (data_cursor%filenumber > 0) then
! BUG: Might need to deal with freeing up a file
call mprintf(.true.,WARN,'WE NEED TO FREE THE FILE ASSOCIATED WITH DATA_CURSOR')
call mprintf(.true.,WARN,'PLEASE REPORT THIS BUG TO THE DEVELOPER!')
            end if
            data_cursor%fg_data%r_arr => store_me%r_arr 
            data_cursor%fg_data%valid_mask => store_me%valid_mask 
            data_cursor%fg_data%modified_mask => store_me%modified_mask 
            return
         end if
      end if

      allocate(newnode)
      call dup(store_me, newnode%fg_data)

      newnode%field_shape = shape(newnode%fg_data%r_arr)
      memsize = memsize + size(newnode%fg_data%r_arr)
      newnode%last_used = global_time
      global_time = global_time + 1
      newnode%filenumber = 0
      call add_to_heap(newnode)

      do while (memsize > MEMSIZE_MAX)
         call get_min(evictnode)
         evictnode%filenumber = next_filenumber
         next_filenumber = next_filenumber + 1
         do funit=10,100
            inquire(unit=funit, opened=is_used)
            if (.not. is_used) exit
         end do
         memsize = memsize - size(evictnode%fg_data%r_arr)
         write(fname,'(i9.9,a2,i3.3)') evictnode%filenumber,'.p',my_proc_id
         open(funit,file=trim(fname),form='unformatted',status='unknown')
         write(funit) evictnode%fg_data%r_arr  
         close(funit)
         deallocate(evictnode%fg_data%r_arr)
      end do

      ! Inserting node at the tail of list
      if (.not. associated(data_cursor)) then
         newnode%prev => name_cursor%fieldlist_tail
         nullify(newnode%next)

         ! List is actually empty
         if (.not. associated(name_cursor%fieldlist_head)) then
            name_cursor%fieldlist_head => newnode
            name_cursor%fieldlist_tail => newnode
         else
            name_cursor%fieldlist_tail%next => newnode
            name_cursor%fieldlist_tail => newnode
         end if

      ! Inserting node at the head of list
      else if ((secondary_cmp(name_cursor%fieldlist_head%fg_data, newnode%fg_data) == GREATER) .or. &
               (secondary_cmp(name_cursor%fieldlist_head%fg_data, newnode%fg_data) == EQUAL)) then
         nullify(newnode%prev)
         newnode%next => name_cursor%fieldlist_head
         name_cursor%fieldlist_head%prev => newnode
         name_cursor%fieldlist_head => newnode

      ! Inserting somewhere in the middle of the list
      else 
         newnode%prev => data_cursor%prev 
         newnode%next => data_cursor    
         data_cursor%prev%next => newnode
         data_cursor%prev => newnode
      end if

   end subroutine storage_put_field


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: storage_get_field
   !
   ! Purpose: Retrieves an fg_input type from storage; if the fg_input type whose
   !    header matches the header of get_me does not exist, istatus = 1 upon 
   !    return; if the requested fg_input type is found, istatus = 0
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine storage_get_field(get_me, istatus)

      implicit none

      ! Arguments
      type (fg_input), intent(inout) :: get_me
      integer, intent(out) :: istatus

      ! Local variables
      integer :: funit
      logical :: is_used
      character (len=64) :: fname
      type (head_node), pointer :: name_cursor
      type (data_node), pointer :: data_cursor
      type (data_node), pointer :: evictnode

      !CWH Initialize local pointer variables
      nullify(evictnode)     !MGD initialization for evictnodeshould not be necessary

      global_time = global_time + 1

      istatus = 1

      ! We'll first see if there is already a list for this fieldname
      name_cursor => head
      do while (associated(name_cursor))
         if (primary_cmp(name_cursor%fg_data, get_me) == EQUAL) exit 
         name_cursor => name_cursor%next
      end do

      if (.not. associated(name_cursor)) return 

      ! At this point, name_cursor points to a valid head node for fieldname
      data_cursor => name_cursor%fieldlist_head
      do while ( associated(data_cursor) )
         if (secondary_cmp(get_me, data_cursor%fg_data) == EQUAL) then
            call dup(data_cursor%fg_data, get_me)

            ! Before deciding whether we need to write an array to disk, first consider 
            !   that reading the requested array will use memory
            if (data_cursor%filenumber > 0) then
               memsize = memsize + data_cursor%field_shape(1)*data_cursor%field_shape(2) 
            end if

            ! If we exceed our memory limit, we need to evict
            do while (memsize > MEMSIZE_MAX)
               call get_min(evictnode)
               evictnode%filenumber = next_filenumber
               next_filenumber = next_filenumber + 1
               do funit=10,100
                  inquire(unit=funit, opened=is_used)
                  if (.not. is_used) exit
               end do
               memsize = memsize - size(evictnode%fg_data%r_arr)
               write(fname,'(i9.9,a2,i3.3)') evictnode%filenumber,'.p',my_proc_id
               open(funit,file=trim(fname),form='unformatted',status='unknown')
               write(funit) evictnode%fg_data%r_arr  
               close(funit)
               deallocate(evictnode%fg_data%r_arr)
            end do

            ! Get requested array
            if (data_cursor%filenumber > 0) then
               data_cursor%last_used = global_time 
               global_time = global_time + 1
               call add_to_heap(data_cursor)
               write(fname,'(i9.9,a2,i3.3)') data_cursor%filenumber,'.p',my_proc_id
               do funit=10,100
                  inquire(unit=funit, opened=is_used)
                  if (.not. is_used) exit
               end do
               open(funit,file=trim(fname),form='unformatted',status='old')
               allocate(data_cursor%fg_data%r_arr(data_cursor%field_shape(1),data_cursor%field_shape(2)))
               read(funit) data_cursor%fg_data%r_arr 
               get_me%r_arr => data_cursor%fg_data%r_arr
               close(funit,status='delete')
               data_cursor%filenumber = 0
            else
               get_me%r_arr => data_cursor%fg_data%r_arr

               call remove_index(data_cursor%heap_index)
               data_cursor%last_used = global_time 
               global_time = global_time + 1
               call add_to_heap(data_cursor)
            end if

            istatus = 0
            return 
         end if
         data_cursor => data_cursor%next
      end do

   end subroutine storage_get_field 


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: storage_query_field
   !
   ! Purpose: 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine storage_query_field(get_me, istatus)

      implicit none

      ! Arguments
      type (fg_input), intent(inout) :: get_me
      integer, intent(out) :: istatus

      ! Local variables
      type (head_node), pointer :: name_cursor
      type (data_node), pointer :: data_cursor

      istatus = 1

      ! We'll first see if there is already a list for this fieldname
      name_cursor => head
      do while (associated(name_cursor))
         if (primary_cmp(name_cursor%fg_data, get_me) == EQUAL) exit 
         name_cursor => name_cursor%next
      end do

      if (.not. associated(name_cursor)) return 

      ! At this point, name_cursor points to a valid head node for fieldname
      data_cursor => name_cursor%fieldlist_head
      do while ( associated(data_cursor) )
         if (secondary_cmp(get_me, data_cursor%fg_data) == EQUAL) then
            get_me%r_arr => data_cursor%fg_data%r_arr
            get_me%valid_mask => data_cursor%fg_data%valid_mask
            get_me%modified_mask => data_cursor%fg_data%modified_mask
            istatus = 0
            return
         end if
         data_cursor => data_cursor%next
      end do

   end subroutine storage_query_field 


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_next_output_fieldname
   !
   ! Purpose: 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_next_output_fieldname(nest_num, field_name, ndims, &
                                        min_level, max_level, &
                                        istagger, mem_order, dim_names, units, description, &
                                        sr_x, sr_y, &
                                        istatus)

      implicit none

      ! Arguments
      integer, intent(in) :: nest_num
      integer, intent(out) :: ndims, min_level, max_level, istagger, istatus
      integer, intent(out) :: sr_x, sr_y
      character (len=128), intent(out) :: field_name, mem_order, units, description
      character (len=128), dimension(3), intent(out) :: dim_names

      integer, parameter  :: WRF_FILE_NOT_OPENED                  = 100
      integer, parameter  :: WRF_FILE_OPENED_NOT_COMMITTED        = 101
      integer, parameter  :: WRF_FILE_OPENED_FOR_WRITE            = 102
      integer, parameter  :: WRF_FILE_OPENED_FOR_READ             = 103
      integer, parameter  :: WRF_REAL                             = 104
      integer, parameter  :: WRF_DOUBLE                           = 105
      integer, parameter  :: WRF_FLOAT=WRF_REAL
      integer, parameter  :: WRF_INTEGER                          = 106
      integer, parameter  :: WRF_LOGICAL                          = 107
      integer, parameter  :: WRF_COMPLEX                          = 108
      integer, parameter  :: WRF_DOUBLE_COMPLEX                   = 109
      integer, parameter  :: WRF_FILE_OPENED_FOR_UPDATE           = 110
  
!WRF Error and Warning messages (1-999)
!All i/o package-specific status codes you may want to add must be handled by your package (see below)
! WRF handles these and netCDF messages only
  integer, parameter  :: WRF_NO_ERR                  =  0       !no error
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       !file not found, or incomplete
  integer, parameter  :: WRF_WARN_MD_NF              = -2       !metadata not found
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       !timestamp not found
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       !no more timestamps
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       !variable not found
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       !no more variables for the current time
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       !too many open files
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       !data type mismatch
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       !attempt to write readonly file
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      !attempt to read writeonly file
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      !attempt to access unopened file
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      !attempt to do 2 trainings for 1 variable
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      !attempt to read past EOF
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      !bad data handle
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      !write length not equal to training length
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      !more dimensions requested than training
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      !attempt to read more data than exists
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      !input dimension inconsistent
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      !input MemoryOrder not recognized
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      !a dimension name with 2 different lengths
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      !string longer than provided storage
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      !function not supportable
  integer, parameter  :: WRF_WARN_NOOP               = -23      !package implements this routine as NOOP

!Fatal errors 
  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 !allocation error
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 !dealloc error
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 !bad file status


!Package specific errors (1000+)        
!Netcdf status codes
!WRF will accept status codes of 1000+, but it is up to the package to handle
! and return the status to the user.

  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    !this code not in either spec?
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    !this code not in either spec?
  integer, parameter  :: WRF_WARN_FILE_OPEN_FOR_READ = -1009
  integer, parameter  :: WRF_IO_NOT_INITIALIZED      = -1010
  integer, parameter  :: WRF_WARN_MD_AFTER_OPEN      = -1011
  integer, parameter  :: WRF_WARN_TOO_MANY_VARIABLES = -1012
  integer, parameter  :: WRF_WARN_DRYRUN_CLOSE       = -1013
  integer, parameter  :: WRF_WARN_DATESTR_BAD_LENGTH = -1014
  integer, parameter  :: WRF_WARN_ZERO_LENGTH_READ   = -1015
  integer, parameter  :: WRF_WARN_DATA_TYPE_NOT_FOUND = -1016
  integer, parameter  :: WRF_WARN_DATESTR_ERROR      = -1017
  integer, parameter  :: WRF_WARN_DRYRUN_READ        = -1018
  integer, parameter  :: WRF_WARN_ZERO_LENGTH_GET    = -1019
  integer, parameter  :: WRF_WARN_ZERO_LENGTH_PUT    = -1020
  integer, parameter  :: WRF_WARN_NETCDF             = -1021    
  integer, parameter  :: WRF_WARN_LENGTH_LESS_THAN_1 = -1022    
  integer, parameter  :: WRF_WARN_MORE_DATA_IN_FILE  = -1023    
  integer, parameter  :: WRF_WARN_DATE_LT_LAST_DATE  = -1024

! For HDF5 only
  integer, parameter  :: WRF_HDF5_ERR_FILE                 = -200
  integer, parameter  :: WRF_HDF5_ERR_MD                   = -201
  integer, parameter  :: WRF_HDF5_ERR_TIME                 = -202
  integer, parameter  :: WRF_HDF5_ERR_TIME_EOF             = -203
  integer, parameter  :: WRF_HDF5_ERR_MORE_DATA_IN_FILE    = -204
  integer, parameter  :: WRF_HDF5_ERR_DATE_LT_LAST_DATE    = -205
  integer, parameter  :: WRF_HDF5_ERR_TOO_MANY_FILES       = -206
  integer, parameter  :: WRF_HDF5_ERR_TYPE_MISMATCH        = -207
  integer, parameter  :: WRF_HDF5_ERR_LENGTH_LESS_THAN_1   = -208
  integer, parameter  :: WRF_HDF5_ERR_WRITE_RONLY_FILE     = -209
  integer, parameter  :: WRF_HDF5_ERR_READ_WONLY_FILE      = -210
  integer, parameter  :: WRF_HDF5_ERR_FILE_NOT_OPENED      = -211
  integer, parameter  :: WRF_HDF5_ERR_DATESTR_ERROR        = -212
  integer, parameter  :: WRF_HDF5_ERR_DRYRUN_READ          = -213
  integer, parameter  :: WRF_HDF5_ERR_ZERO_LENGTH_GET      = -214
  integer, parameter  :: WRF_HDF5_ERR_ZERO_LENGTH_PUT      = -215
  integer, parameter  :: WRF_HDF5_ERR_2DRYRUNS_1VARIABLE   = -216
  integer, parameter  :: WRF_HDF5_ERR_DATA_TYPE_NOTFOUND   = -217
  integer, parameter  :: WRF_HDF5_ERR_READ_PAST_EOF        = -218
  integer, parameter  :: WRF_HDF5_ERR_BAD_DATA_HANDLE      = -219
  integer, parameter  :: WRF_HDF5_ERR_WRTLEN_NE_DRRUNLEN   = -220
  integer, parameter  :: WRF_HDF5_ERR_DRYRUN_CLOSE         = -221
  integer, parameter  :: WRF_HDF5_ERR_DATESTR_BAD_LENGTH   = -222
  integer, parameter  :: WRF_HDF5_ERR_ZERO_LENGTH_READ     = -223
  integer, parameter  :: WRF_HDF5_ERR_TOO_MANY_DIMS        = -224
  integer, parameter  :: WRF_HDF5_ERR_TOO_MANY_VARIABLES   = -225
  integer, parameter  :: WRF_HDF5_ERR_COUNT_TOO_LONG       = -226
  integer, parameter  :: WRF_HDF5_ERR_DIMENSION_ERROR      = -227
  integer, parameter  :: WRF_HDF5_ERR_BAD_MEMORYORDER      = -228
  integer, parameter  :: WRF_HDF5_ERR_DIMNAME_REDEFINED    = -229
  integer, parameter  :: WRF_HDF5_ERR_MD_AFTER_OPEN        = -230
  integer, parameter  :: WRF_HDF5_ERR_CHARSTR_GT_LENDATA   = -231
  integer, parameter  :: WRF_HDF5_ERR_BAD_DATA_TYPE        = -232
  integer, parameter  :: WRF_HDF5_ERR_FILE_NOT_COMMITTED   = -233

  integer, parameter  :: WRF_HDF5_ERR_ALLOCATION        = -2001
  integer, parameter  :: WRF_HDF5_ERR_DEALLOCATION      = -2002
  integer, parameter  :: WRF_HDF5_ERR_BAD_FILE_STATUS   = -2003
  integer, parameter  :: WRF_HDF5_ERR_BAD_VARIABLE_DIM  = -2004
  integer, parameter  :: WRF_HDF5_ERR_MDVAR_DIM_NOT_1D  = -2005
  integer, parameter  :: WRF_HDF5_ERR_TOO_MANY_TIMES    = -2006
  integer, parameter ::  WRF_HDF5_ERR_DATA_ID_NOTFOUND  = -2007

  integer, parameter ::  WRF_HDF5_ERR_DATASPACE         = -300
  integer, parameter ::  WRF_HDF5_ERR_DATATYPE          = -301
  integer, parameter :: WRF_HDF5_ERR_PROPERTY_LIST      = -302

  integer, parameter :: WRF_HDF5_ERR_DATASET_CREATE     = -303
  integer, parameter :: WRF_HDF5_ERR_DATASET_READ       = -304
  integer, parameter :: WRF_HDF5_ERR_DATASET_WRITE      = -305
  integer, parameter :: WRF_HDF5_ERR_DATASET_OPEN       = -306
  integer, parameter :: WRF_HDF5_ERR_DATASET_GENERAL    = -307
  integer, parameter :: WRF_HDF5_ERR_GROUP              = -308

  integer, parameter :: WRF_HDF5_ERR_FILE_OPEN          = -309
  integer, parameter :: WRF_HDF5_ERR_FILE_CREATE        = -310
  integer, parameter :: WRF_HDF5_ERR_DATASET_CLOSE      = -311
  integer, parameter :: WRF_HDF5_ERR_FILE_CLOSE         = -312
  integer, parameter :: WRF_HDF5_ERR_CLOSE_GENERAL      = -313

  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_CREATE   = -314
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_READ     = -315
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_WRITE    = -316
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_OPEN     = -317
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_GENERAL  = -318
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_CLOSE    = -319

  integer, parameter :: WRF_HDF5_ERR_OTHERS             = -320
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_OTHERS   = -321

  integer, parameter :: WRF_GRIB2_ERR_GRIBCREATE        = -401
  integer, parameter :: WRF_GRIB2_ERR_ADDLOCAL          = -402
  integer, parameter :: WRF_GRIB2_ERR_ADDGRIB           = -403
  integer, parameter :: WRF_GRIB2_ERR_ADDFIELD          = -404
  integer, parameter :: WRF_GRIB2_ERR_GRIBEND           = -405
  integer, parameter :: WRF_GRIB2_ERR_WRITE             = -406
  integer, parameter :: WRF_GRIB2_ERR_GRIB2MAP          = -407
  integer, parameter :: WRF_GRIB2_ERR_GETGB2            = -408
  integer, parameter :: WRF_GRIB2_ERR_READ              = -409

      ! Local variables
      type (data_node), pointer :: data_cursor

      istatus = 1
 
      if (.not. associated(next_output_field)) return

      min_level = 1
      max_level = 0
      ndims = 2

      do while (max_level == 0 .and. associated(next_output_field))

         data_cursor => next_output_field%fieldlist_head
         if (associated(data_cursor)) then
            if (.not. is_mask_field(data_cursor%fg_data)) then
               do while ( associated(data_cursor) )
                  istatus = 0
                  max_level = max_level + 1
                  data_cursor => data_cursor%next
               end do
            end if
         end if

         if (max_level == 0) next_output_field => next_output_field%next
      end do

      if (max_level > 0 .and. associated(next_output_field)) then

         if (max_level > 1) ndims = 3
         if (ndims == 2) then
            mem_order = 'XY ' 
            dim_names(3) = ' '
         else
            mem_order = 'XYZ' 
            if (is_time_dependent(next_output_field%fg_data)) then
               dim_names(3) = ' '
               dim_names(3)(1:32) = next_output_field%fg_data%header%vertical_coord
            else
               write(dim_names(3),'(a11,i4.4)') 'z-dimension', max_level
            end if
         end if
         field_name = get_fieldname(next_output_field%fg_data)
         istagger = get_staggering(next_output_field%fg_data)
         if (istagger == M .or. istagger == HH .or. istagger == VV) then
            dim_names(1) = 'west_east'
            dim_names(2) = 'south_north'
         else if (istagger == U) then
            dim_names(1) = 'west_east_stag'
            dim_names(2) = 'south_north'
         else if (istagger == V) then
            dim_names(1) = 'west_east'
            dim_names(2) = 'south_north_stag'
         else
            dim_names(1) = 'i-dimension'
            dim_names(2) = 'j-dimension'
         end if
         units = get_units(next_output_field%fg_data)
         description = get_description(next_output_field%fg_data) 
         call get_subgrid_dim_name(nest_num, field_name, dim_names(1:2), &
                                   sr_x, sr_y, istatus)

         next_output_field => next_output_field%next
      end if

   end subroutine get_next_output_fieldname 


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_subgrid_dim_name
   !
   ! Purpose: 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_subgrid_dim_name(nest_num, field_name, dimnames, &
                                   sub_x, sub_y, istatus)

      use gridinfo_module

      implicit none

      ! Arguments
      integer, intent(in) :: nest_num
      integer, intent(out) :: sub_x, sub_y, istatus
      character(len=128), intent(in) :: field_name
      character(len=128), dimension(2), intent(inout) :: dimnames

      ! Local variables
      integer :: idx, nlen

      sub_x = next_output_field%fg_data%header%sr_x
      sub_y = next_output_field%fg_data%header%sr_y

      if (sub_x > 1) then
        dimnames(1) = trim(dimnames(1))//"_subgrid"
      end if
      if (sub_y > 1) then
        dimnames(2) = trim(dimnames(2))//"_subgrid"
      end if

      istatus = 0

   end subroutine get_subgrid_dim_name


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_next_output_field
   !
   ! Purpose: 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_next_output_field(field_name, r_array, &
                                    start_i, end_i, start_j, end_j, min_level, max_level, istatus)

      implicit none

      ! Arguments
      integer, intent(out) :: start_i, end_i, start_j, end_j, min_level, max_level, istatus
      real, pointer, dimension(:,:,:) :: r_array
      character (len=128), intent(out) :: field_name

      integer, parameter  :: WRF_FILE_NOT_OPENED                  = 100
      integer, parameter  :: WRF_FILE_OPENED_NOT_COMMITTED        = 101
      integer, parameter  :: WRF_FILE_OPENED_FOR_WRITE            = 102
      integer, parameter  :: WRF_FILE_OPENED_FOR_READ             = 103
      integer, parameter  :: WRF_REAL                             = 104
      integer, parameter  :: WRF_DOUBLE                           = 105
      integer, parameter  :: WRF_FLOAT=WRF_REAL
      integer, parameter  :: WRF_INTEGER                          = 106
      integer, parameter  :: WRF_LOGICAL                          = 107
      integer, parameter  :: WRF_COMPLEX                          = 108
      integer, parameter  :: WRF_DOUBLE_COMPLEX                   = 109
      integer, parameter  :: WRF_FILE_OPENED_FOR_UPDATE           = 110
  
!WRF Error and Warning messages (1-999)
!All i/o package-specific status codes you may want to add must be handled by your package (see below)
! WRF handles these and netCDF messages only
  integer, parameter  :: WRF_NO_ERR                  =  0       !no error
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       !file not found, or incomplete
  integer, parameter  :: WRF_WARN_MD_NF              = -2       !metadata not found
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       !timestamp not found
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       !no more timestamps
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       !variable not found
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       !no more variables for the current time
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       !too many open files
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       !data type mismatch
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       !attempt to write readonly file
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      !attempt to read writeonly file
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      !attempt to access unopened file
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      !attempt to do 2 trainings for 1 variable
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      !attempt to read past EOF
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      !bad data handle
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      !write length not equal to training length
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      !more dimensions requested than training
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      !attempt to read more data than exists
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      !input dimension inconsistent
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      !input MemoryOrder not recognized
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      !a dimension name with 2 different lengths
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      !string longer than provided storage
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      !function not supportable
  integer, parameter  :: WRF_WARN_NOOP               = -23      !package implements this routine as NOOP

!Fatal errors 
  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 !allocation error
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 !dealloc error
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 !bad file status


!Package specific errors (1000+)        
!Netcdf status codes
!WRF will accept status codes of 1000+, but it is up to the package to handle
! and return the status to the user.

  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    !this code not in either spec?
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    !this code not in either spec?
  integer, parameter  :: WRF_WARN_FILE_OPEN_FOR_READ = -1009
  integer, parameter  :: WRF_IO_NOT_INITIALIZED      = -1010
  integer, parameter  :: WRF_WARN_MD_AFTER_OPEN      = -1011
  integer, parameter  :: WRF_WARN_TOO_MANY_VARIABLES = -1012
  integer, parameter  :: WRF_WARN_DRYRUN_CLOSE       = -1013
  integer, parameter  :: WRF_WARN_DATESTR_BAD_LENGTH = -1014
  integer, parameter  :: WRF_WARN_ZERO_LENGTH_READ   = -1015
  integer, parameter  :: WRF_WARN_DATA_TYPE_NOT_FOUND = -1016
  integer, parameter  :: WRF_WARN_DATESTR_ERROR      = -1017
  integer, parameter  :: WRF_WARN_DRYRUN_READ        = -1018
  integer, parameter  :: WRF_WARN_ZERO_LENGTH_GET    = -1019
  integer, parameter  :: WRF_WARN_ZERO_LENGTH_PUT    = -1020
  integer, parameter  :: WRF_WARN_NETCDF             = -1021    
  integer, parameter  :: WRF_WARN_LENGTH_LESS_THAN_1 = -1022    
  integer, parameter  :: WRF_WARN_MORE_DATA_IN_FILE  = -1023    
  integer, parameter  :: WRF_WARN_DATE_LT_LAST_DATE  = -1024

! For HDF5 only
  integer, parameter  :: WRF_HDF5_ERR_FILE                 = -200
  integer, parameter  :: WRF_HDF5_ERR_MD                   = -201
  integer, parameter  :: WRF_HDF5_ERR_TIME                 = -202
  integer, parameter  :: WRF_HDF5_ERR_TIME_EOF             = -203
  integer, parameter  :: WRF_HDF5_ERR_MORE_DATA_IN_FILE    = -204
  integer, parameter  :: WRF_HDF5_ERR_DATE_LT_LAST_DATE    = -205
  integer, parameter  :: WRF_HDF5_ERR_TOO_MANY_FILES       = -206
  integer, parameter  :: WRF_HDF5_ERR_TYPE_MISMATCH        = -207
  integer, parameter  :: WRF_HDF5_ERR_LENGTH_LESS_THAN_1   = -208
  integer, parameter  :: WRF_HDF5_ERR_WRITE_RONLY_FILE     = -209
  integer, parameter  :: WRF_HDF5_ERR_READ_WONLY_FILE      = -210
  integer, parameter  :: WRF_HDF5_ERR_FILE_NOT_OPENED      = -211
  integer, parameter  :: WRF_HDF5_ERR_DATESTR_ERROR        = -212
  integer, parameter  :: WRF_HDF5_ERR_DRYRUN_READ          = -213
  integer, parameter  :: WRF_HDF5_ERR_ZERO_LENGTH_GET      = -214
  integer, parameter  :: WRF_HDF5_ERR_ZERO_LENGTH_PUT      = -215
  integer, parameter  :: WRF_HDF5_ERR_2DRYRUNS_1VARIABLE   = -216
  integer, parameter  :: WRF_HDF5_ERR_DATA_TYPE_NOTFOUND   = -217
  integer, parameter  :: WRF_HDF5_ERR_READ_PAST_EOF        = -218
  integer, parameter  :: WRF_HDF5_ERR_BAD_DATA_HANDLE      = -219
  integer, parameter  :: WRF_HDF5_ERR_WRTLEN_NE_DRRUNLEN   = -220
  integer, parameter  :: WRF_HDF5_ERR_DRYRUN_CLOSE         = -221
  integer, parameter  :: WRF_HDF5_ERR_DATESTR_BAD_LENGTH   = -222
  integer, parameter  :: WRF_HDF5_ERR_ZERO_LENGTH_READ     = -223
  integer, parameter  :: WRF_HDF5_ERR_TOO_MANY_DIMS        = -224
  integer, parameter  :: WRF_HDF5_ERR_TOO_MANY_VARIABLES   = -225
  integer, parameter  :: WRF_HDF5_ERR_COUNT_TOO_LONG       = -226
  integer, parameter  :: WRF_HDF5_ERR_DIMENSION_ERROR      = -227
  integer, parameter  :: WRF_HDF5_ERR_BAD_MEMORYORDER      = -228
  integer, parameter  :: WRF_HDF5_ERR_DIMNAME_REDEFINED    = -229
  integer, parameter  :: WRF_HDF5_ERR_MD_AFTER_OPEN        = -230
  integer, parameter  :: WRF_HDF5_ERR_CHARSTR_GT_LENDATA   = -231
  integer, parameter  :: WRF_HDF5_ERR_BAD_DATA_TYPE        = -232
  integer, parameter  :: WRF_HDF5_ERR_FILE_NOT_COMMITTED   = -233

  integer, parameter  :: WRF_HDF5_ERR_ALLOCATION        = -2001
  integer, parameter  :: WRF_HDF5_ERR_DEALLOCATION      = -2002
  integer, parameter  :: WRF_HDF5_ERR_BAD_FILE_STATUS   = -2003
  integer, parameter  :: WRF_HDF5_ERR_BAD_VARIABLE_DIM  = -2004
  integer, parameter  :: WRF_HDF5_ERR_MDVAR_DIM_NOT_1D  = -2005
  integer, parameter  :: WRF_HDF5_ERR_TOO_MANY_TIMES    = -2006
  integer, parameter ::  WRF_HDF5_ERR_DATA_ID_NOTFOUND  = -2007

  integer, parameter ::  WRF_HDF5_ERR_DATASPACE         = -300
  integer, parameter ::  WRF_HDF5_ERR_DATATYPE          = -301
  integer, parameter :: WRF_HDF5_ERR_PROPERTY_LIST      = -302

  integer, parameter :: WRF_HDF5_ERR_DATASET_CREATE     = -303
  integer, parameter :: WRF_HDF5_ERR_DATASET_READ       = -304
  integer, parameter :: WRF_HDF5_ERR_DATASET_WRITE      = -305
  integer, parameter :: WRF_HDF5_ERR_DATASET_OPEN       = -306
  integer, parameter :: WRF_HDF5_ERR_DATASET_GENERAL    = -307
  integer, parameter :: WRF_HDF5_ERR_GROUP              = -308

  integer, parameter :: WRF_HDF5_ERR_FILE_OPEN          = -309
  integer, parameter :: WRF_HDF5_ERR_FILE_CREATE        = -310
  integer, parameter :: WRF_HDF5_ERR_DATASET_CLOSE      = -311
  integer, parameter :: WRF_HDF5_ERR_FILE_CLOSE         = -312
  integer, parameter :: WRF_HDF5_ERR_CLOSE_GENERAL      = -313

  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_CREATE   = -314
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_READ     = -315
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_WRITE    = -316
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_OPEN     = -317
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_GENERAL  = -318
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_CLOSE    = -319

  integer, parameter :: WRF_HDF5_ERR_OTHERS             = -320
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_OTHERS   = -321

  integer, parameter :: WRF_GRIB2_ERR_GRIBCREATE        = -401
  integer, parameter :: WRF_GRIB2_ERR_ADDLOCAL          = -402
  integer, parameter :: WRF_GRIB2_ERR_ADDGRIB           = -403
  integer, parameter :: WRF_GRIB2_ERR_ADDFIELD          = -404
  integer, parameter :: WRF_GRIB2_ERR_GRIBEND           = -405
  integer, parameter :: WRF_GRIB2_ERR_WRITE             = -406
  integer, parameter :: WRF_GRIB2_ERR_GRIB2MAP          = -407
  integer, parameter :: WRF_GRIB2_ERR_GETGB2            = -408
  integer, parameter :: WRF_GRIB2_ERR_READ              = -409

      ! Local variables
      integer :: k
      type (data_node), pointer :: data_cursor
      type (fg_input) :: temp_field

      istatus = 1
 
      if (.not. associated(next_output_field)) return

      min_level = 1
      max_level = 0

      do while (max_level == 0 .and. associated(next_output_field))

         data_cursor => next_output_field%fieldlist_head
         if (associated(data_cursor)) then
            if (.not. is_mask_field(data_cursor%fg_data)) then
               do while ( associated(data_cursor) )
                  istatus = 0
                  max_level = max_level + 1
                  data_cursor => data_cursor%next
               end do
            end if
         end if

         if (max_level == 0) next_output_field => next_output_field%next
      end do

      if (max_level > 0 .and. associated(next_output_field)) then

         start_i = 1
         end_i = next_output_field%fieldlist_head%field_shape(1)
         start_j = 1
         end_j = next_output_field%fieldlist_head%field_shape(2)

         allocate(r_array(next_output_field%fieldlist_head%field_shape(1), &
                          next_output_field%fieldlist_head%field_shape(2), &
                          max_level) )

         k = 1
         data_cursor => next_output_field%fieldlist_head
         do while ( associated(data_cursor) )
            call dup(data_cursor%fg_data, temp_field)
            call storage_get_field(temp_field, istatus)
            r_array(:,:,k) = temp_field%r_arr
            k = k + 1 
            data_cursor => data_cursor%next
         end do

         field_name = get_fieldname(next_output_field%fg_data)

         next_output_field => next_output_field%next
      end if

   end subroutine get_next_output_field


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: storage_delete_field
   !
   ! Purpose: Deletes the stored fg_input type whose header matches delete_me
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine storage_delete_field(delete_me)

      implicit none

      ! Arguments
      type (fg_input), intent(in) :: delete_me

      ! Local variables
      integer :: funit
      logical :: is_used
      character (len=64) :: fname
      type (head_node), pointer :: name_cursor
      type (data_node), pointer :: data_cursor

      ! We'll first see if there is a list for this fieldname
      name_cursor => head
      do while (associated(name_cursor))
         if (primary_cmp(name_cursor%fg_data, delete_me) == EQUAL) exit 
         name_cursor => name_cursor%next
      end do

      if (.not. associated(name_cursor)) return

      ! At this point, name_cursor points to a valid head node for fieldname
      data_cursor => name_cursor%fieldlist_head
      do while ( associated(data_cursor) )
         if (secondary_cmp(delete_me, data_cursor%fg_data) == EQUAL) then

            if (data_cursor%filenumber > 0) then
               do funit=10,100
                  inquire(unit=funit, opened=is_used)
                  if (.not. is_used) exit
               end do
               write(fname,'(i9.9,a2,i3.3)') data_cursor%filenumber,'.p',my_proc_id
               open(funit,file=trim(fname),form='unformatted',status='old')
               close(funit,status='delete')
            else
               call remove_index(data_cursor%heap_index)
               memsize = memsize - size(data_cursor%fg_data%r_arr)
               deallocate(data_cursor%fg_data%r_arr)
            end if
            if (associated(data_cursor%fg_data%valid_mask)) call bitarray_destroy(data_cursor%fg_data%valid_mask)
            nullify(data_cursor%fg_data%valid_mask)
            if (associated(data_cursor%fg_data%modified_mask)) call bitarray_destroy(data_cursor%fg_data%modified_mask)
            nullify(data_cursor%fg_data%modified_mask)

            ! Only item in the list
            if (.not. associated(data_cursor%next) .and. &
                .not. associated(data_cursor%prev)) then
               nullify(name_cursor%fieldlist_head)          
               nullify(name_cursor%fieldlist_tail)          
               deallocate(data_cursor)
! DO WE REMOVE THIS HEADER NODE AT THIS POINT?
               return

            ! Head of the list
            else if (.not. associated(data_cursor%prev)) then
               name_cursor%fieldlist_head => data_cursor%next
               nullify(data_cursor%next%prev)
               deallocate(data_cursor)
               return

            ! Tail of the list
            else if (.not. associated(data_cursor%next)) then
               name_cursor%fieldlist_tail => data_cursor%prev
               nullify(data_cursor%prev%next)
               deallocate(data_cursor)
               return

            ! Middle of the list
            else
               data_cursor%prev%next => data_cursor%next
               data_cursor%next%prev => data_cursor%prev
               deallocate(data_cursor)
               return

            end if 
           
         end if
         data_cursor => data_cursor%next
      end do

   end subroutine storage_delete_field


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: storage_delete_all_td
   !
   ! Purpose: Deletes the stored time-dependent data
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine storage_delete_all_td()

      implicit none

      ! Local variables
      integer :: funit
      logical :: is_used
      character (len=64) :: fname
      type (head_node), pointer :: name_cursor
      type (data_node), pointer :: data_cursor, next_cursor

      ! We'll first see if there is a list for this fieldname
      name_cursor => head
      do while (associated(name_cursor))

         data_cursor => name_cursor%fieldlist_head
         do while ( associated(data_cursor) )
            if ( is_time_dependent(data_cursor%fg_data) ) then
   
               if (data_cursor%filenumber > 0) then
                  do funit=10,100
                     inquire(unit=funit, opened=is_used)
                     if (.not. is_used) exit
                  end do
                  write(fname,'(i9.9,a2,i3.3)') data_cursor%filenumber,'.p',my_proc_id
                  open(funit,file=trim(fname),form='unformatted',status='old')
                  close(funit,status='delete')
               else
                  call remove_index(data_cursor%heap_index)
                  memsize = memsize - size(data_cursor%fg_data%r_arr)
                  deallocate(data_cursor%fg_data%r_arr)
               end if
               if (associated(data_cursor%fg_data%valid_mask)) call bitarray_destroy(data_cursor%fg_data%valid_mask)
               nullify(data_cursor%fg_data%valid_mask)
               if (associated(data_cursor%fg_data%modified_mask)) call bitarray_destroy(data_cursor%fg_data%modified_mask)
               nullify(data_cursor%fg_data%modified_mask)

               ! We should handle individual cases, that way we can deal with a list 
               !   that has both time independent and time dependent nodes in it. 
   
               ! Only item in the list
               if (.not. associated(data_cursor%next) .and. &
                   .not. associated(data_cursor%prev)) then
                  next_cursor => null()
                  nullify(name_cursor%fieldlist_head)          
                  nullify(name_cursor%fieldlist_tail)          
                  deallocate(data_cursor)
! DO WE REMOVE THIS HEADER NODE AT THIS POINT?
   
               ! Head of the list
               else if (.not. associated(data_cursor%prev)) then
                  name_cursor%fieldlist_head => data_cursor%next
                  next_cursor => data_cursor%next
                  nullify(data_cursor%next%prev)
                  deallocate(data_cursor)
   
               ! Tail of the list
               else if (.not. associated(data_cursor%next)) then
! THIS CASE SHOULD PROBABLY NOT OCCUR
                  name_cursor%fieldlist_tail => data_cursor%prev
                  next_cursor => null()
                  nullify(data_cursor%prev%next)
                  deallocate(data_cursor)
   
               ! Middle of the list
               else
! THIS CASE SHOULD PROBABLY NOT OCCUR
                  next_cursor => data_cursor%next
                  data_cursor%prev%next => data_cursor%next
                  data_cursor%next%prev => data_cursor%prev
                  deallocate(data_cursor)
   
               end if 
              
            end if
            data_cursor => next_cursor
         end do

         name_cursor => name_cursor%next
      end do

   end subroutine storage_delete_all_td


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: storage_get_levels
   !
   ! Purpose: Returns a list of all levels for the field indicated in the_header. 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine storage_get_levels(the_header, list)
      
      implicit none

      ! Arguments
      integer, pointer, dimension(:) :: list
      type (fg_input), intent(in) :: the_header

      ! Local variables
      integer :: n
      type (head_node), pointer :: name_cursor
      type (data_node), pointer :: data_cursor

      if (associated(list)) deallocate(list)
      nullify(list)

      ! We'll first see if there is a list for this header 
      name_cursor => head
      do while (associated(name_cursor))
         if (primary_cmp(name_cursor%fg_data, the_header) == EQUAL) exit 
         name_cursor => name_cursor%next
      end do

      if (.not. associated(name_cursor)) return 

      n = 0
      ! At this point, name_cursor points to a valid head node for fieldname
      data_cursor => name_cursor%fieldlist_head
      do while ( associated(data_cursor) )
         n = n + 1
         if (.not. associated(data_cursor%next)) exit
         data_cursor => data_cursor%next
      end do

      if (n > 0) allocate(list(n)) 

      n = 1
      do while ( associated(data_cursor) )
         list(n) = get_level(data_cursor%fg_data)
         n = n + 1
         data_cursor => data_cursor%prev
      end do

   end subroutine storage_get_levels


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: storage_delete_all
   !
   ! Purpose: Deletes all data, both time-independent and time-dependent. 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine storage_delete_all()

      implicit none

      ! Local variables
      integer :: funit
      logical :: is_used
      character (len=64) :: fname
      type (head_node), pointer :: name_cursor
      type (data_node), pointer :: data_cursor

      ! We'll first see if there is already a list for this fieldname
      name_cursor => head
      do while (associated(name_cursor))

         if (associated(name_cursor%fieldlist_head)) then
            data_cursor => name_cursor%fieldlist_head
            do while ( associated(data_cursor) )
               name_cursor%fieldlist_head => data_cursor%next

               if (data_cursor%filenumber > 0) then
                  do funit=10,100
                     inquire(unit=funit, opened=is_used)
                     if (.not. is_used) exit
                  end do
                  write(fname,'(i9.9,a2,i3.3)') data_cursor%filenumber,'.p',my_proc_id
                  open(funit,file=trim(fname),form='unformatted',status='old')
                  close(funit,status='delete')
               else
                  call remove_index(data_cursor%heap_index)
                  memsize = memsize - size(data_cursor%fg_data%r_arr)
                  deallocate(data_cursor%fg_data%r_arr)
               end if
               if (associated(data_cursor%fg_data%valid_mask)) call bitarray_destroy(data_cursor%fg_data%valid_mask)
               nullify(data_cursor%fg_data%valid_mask)
               if (associated(data_cursor%fg_data%modified_mask)) call bitarray_destroy(data_cursor%fg_data%modified_mask)
               nullify(data_cursor%fg_data%modified_mask)

               deallocate(data_cursor)
               data_cursor => name_cursor%fieldlist_head
            end do
         end if

         head => name_cursor%next
         deallocate(name_cursor)
         name_cursor => head
      end do

      nullify(head)
      nullify(tail)

      call heap_destroy()

   end subroutine storage_delete_all


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: storage_get_all_headers
   !
   ! Purpose: 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine storage_get_all_headers(header_list)

      implicit none

      ! Arguments
      type (fg_input), pointer, dimension(:) :: header_list

      ! Local variables
      integer :: nheaders
      type (head_node), pointer :: name_cursor

      nullify(header_list)

      ! First find out how many time-dependent headers there are
      name_cursor => head
      nheaders = 0
      do while (associated(name_cursor))
         if (associated(name_cursor%fieldlist_head)) then
            if (.not. is_mask_field(name_cursor%fieldlist_head%fg_data)) then
               nheaders = nheaders + 1 
            end if
         end if
         name_cursor => name_cursor%next
      end do

      allocate(header_list(nheaders))

      name_cursor => head
      nheaders = 0
      do while (associated(name_cursor))
         if (associated(name_cursor%fieldlist_head)) then
            if (.not. is_mask_field(name_cursor%fieldlist_head%fg_data)) then
               nheaders = nheaders + 1
               call dup(name_cursor%fieldlist_head%fg_data, header_list(nheaders))
            end if
         end if
         name_cursor => name_cursor%next
      end do

   end subroutine storage_get_all_headers


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: storage_get_all_td_headers
   !
   ! Purpose: 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine storage_get_td_headers(header_list)

      implicit none

      ! Arguments
      type (fg_input), pointer, dimension(:) :: header_list

      ! Local variables
      integer :: nheaders
      type (head_node), pointer :: name_cursor

      nullify(header_list)

      ! First find out how many time-dependent headers there are
      name_cursor => head
      nheaders = 0
      do while (associated(name_cursor))
         if (associated(name_cursor%fieldlist_head)) then
            if (is_time_dependent(name_cursor%fieldlist_head%fg_data) .and. &
                .not. is_mask_field(name_cursor%fieldlist_head%fg_data)) then
               nheaders = nheaders + 1 
            end if
         end if
         name_cursor => name_cursor%next
      end do

      allocate(header_list(nheaders))

      name_cursor => head
      nheaders = 0
      do while (associated(name_cursor))
         if (associated(name_cursor%fieldlist_head)) then
            if (is_time_dependent(name_cursor%fieldlist_head%fg_data) .and. &
                .not. is_mask_field(name_cursor%fieldlist_head%fg_data)) then
               nheaders = nheaders + 1
               call dup(name_cursor%fieldlist_head%fg_data, header_list(nheaders))
            end if
         end if
         name_cursor => name_cursor%next
      end do

   end subroutine storage_get_td_headers


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: storage_print_fields
   !
   ! Purpose: 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine storage_print_fields()

      use list_module
      use stringutil

      implicit none

      ! Local variables
      integer :: i, j, k, lmax, n_fields, n_levels, max_levels, itemp
      logical, allocatable, dimension(:,:) :: field_has_level
      integer, allocatable, dimension(:) :: all_levels
      integer, pointer, dimension(:) :: ilevels
      character (len=128), allocatable, dimension(:) :: fieldname_list
      character (len=9) :: ctemp
      type (fg_input), pointer, dimension(:) :: header_list

      type (list) :: all_levs

      !CWH Initialize local pointer variables
      nullify(ilevels)
      nullify(header_list)     !MGD initialization for header_list should not be necessary

      call list_init(all_levs)
      call storage_get_td_headers(header_list)
      n_fields = size(header_list)
      
      allocate(fieldname_list(n_fields))

      max_levels = 0

      do i=1,n_fields
         fieldname_list(i) = header_list(i)%header%field
         call storage_get_levels(header_list(i), ilevels)
         do j=1,size(ilevels)
            if (.not. list_search(all_levs, ikey=ilevels(j), ivalue=itemp)) then
               call list_insert(all_levs, ikey=ilevels(j), ivalue=ilevels(j))
            end if
         end do
         n_levels = size(ilevels)
         if (n_levels > max_levels) max_levels = n_levels
         if (associated(ilevels)) deallocate(ilevels)
      end do 

      max_levels = list_length(all_levs)

      allocate(all_levels(max_levels))
      allocate(field_has_level(n_fields,max_levels))

      field_has_level(:,:) = .false.

      lmax = 0
      do i=1,n_fields
         call storage_get_levels(header_list(i), ilevels)
         n_levels = size(ilevels)
         do j=1,n_levels
            do k=1,lmax 
               if (all_levels(k) == ilevels(j)) exit
            end do 
            if (k > lmax) then
               all_levels(k) = ilevels(j)
               lmax = lmax + 1
            end if
            field_has_level(i,k) = .true.
         end do 
         if (associated(ilevels)) deallocate(ilevels)
      end do 

      call mprintf(.true.,DEBUG,'        .',newline=.false.)
      do i=1,n_fields
         write(ctemp,'(a9)') fieldname_list(i)(1:9)
         call right_justify(ctemp,9)
         call mprintf(.true.,DEBUG,ctemp,newline=.false.)
      end do
      call mprintf(.true.,DEBUG,' ',newline=.true.)
      do j=1,max_levels
         write(ctemp,'(i9)') all_levels(j)
         call mprintf(.true.,DEBUG,'%s ',s1=ctemp,newline=.false.)
         do i=1,n_fields
            if (field_has_level(i,j)) then
               call mprintf(.true.,DEBUG,'        X',newline=.false.)
            else
               call mprintf(.true.,DEBUG,'        -',newline=.false.)
            end if
         end do
         call mprintf(.true.,DEBUG,' ',newline=.true.)
      end do

      deallocate(all_levels)
      deallocate(field_has_level)
      deallocate(fieldname_list)
      deallocate(header_list)

      call list_destroy(all_levs)

   end subroutine storage_print_fields


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: find_missing_values
   !
   ! Purpose: 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine find_missing_values()

      implicit none

      ! Local variables
      integer :: i, j
      logical :: found_missing
      type (head_node), pointer :: name_cursor
      type (data_node), pointer :: data_cursor

      found_missing = .false.

      name_cursor => head
      do while (associated(name_cursor))

         if (associated(name_cursor%fieldlist_head)) then
            data_cursor => name_cursor%fieldlist_head
            do while ( associated(data_cursor) )
               if (.not. associated(data_cursor%fg_data%valid_mask)) then
                  call mprintf(.true.,INFORM, &
                               'Field %s does not have a valid mask and will not be checked for missing values', &
                               s1=data_cursor%fg_data%header%field)
               else
                  ILOOP: do i=1,data_cursor%fg_data%header%dim1(2)-data_cursor%fg_data%header%dim1(1)+1
                  JLOOP: do j=1,data_cursor%fg_data%header%dim2(2)-data_cursor%fg_data%header%dim2(1)+1
                     if (.not. bitarray_test(data_cursor%fg_data%valid_mask,i,j)) then
                        found_missing = .true.
                        call mprintf(.true.,WARN,'Field %s has missing values at level %i at (i,j)=(%i,%i)', &
                                     s1=data_cursor%fg_data%header%field, &
                                     i1=data_cursor%fg_data%header%vertical_level, &
                                     i2=i+data_cursor%fg_data%header%dim1(1)-1, &
                                     i3=j+data_cursor%fg_data%header%dim2(1)-1)
                        exit ILOOP
                     end if
                  end do JLOOP
                  end do ILOOP
               end if
               data_cursor => data_cursor%next
            end do
         end if

         name_cursor => name_cursor%next
      end do

      call mprintf(found_missing,ERROR,'Missing values encountered in interpolated fields. Stopping.')

   end subroutine find_missing_values


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: storage_print_headers
   !
   ! Purpose: 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine storage_print_headers()

      implicit none

      ! Local variables
      type (head_node), pointer :: name_cursor
      type (data_node), pointer :: data_cursor

      call mprintf(.true.,DEBUG,'>>>> STORED FIELDS <<<<')
      call mprintf(.true.,DEBUG,'=======================')

      ! We'll first see if there is already a list for this fieldname
      name_cursor => head
      do while (associated(name_cursor))
         call print_header(name_cursor%fg_data)

         if (associated(name_cursor%fieldlist_head)) then
            data_cursor => name_cursor%fieldlist_head
            do while ( associated(data_cursor) )
               call mprintf(.true.,DEBUG,'  - %i', i1=get_level(data_cursor%fg_data))
               call mprintf(.true.,DEBUG,' ')
               data_cursor => data_cursor%next
            end do
         end if

         name_cursor => name_cursor%next
      end do

   end subroutine storage_print_headers

end module storage_module
