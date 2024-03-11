c-------------------------------------------------------------------------------
c file: mse_lib2.f
c author: mike makowski, michael d. brown
c physicist: Mike Makowski
c-------------------------------------------------------------------------------
c Main MSE library
c-------------------------------------------------------------------------------
c align_data            ! interpolate a waveform onto the mse timebase using a 
c                       !   spline
c average_data          ! average over given window given data & time array
c average_data2         ! average over given window given data, time & mask 
c                       !   arrays
c average_mse           ! returns averaged mse data given a time and averaging
c                       !   interval width
c averaging_intervals   ! determine averaging intervals for pitch angles for 
c                       !   each efit time
c avg_bt                ! averages bt at multiple values of the time
c beam_timing           ! find beam on/off and background times
c calc_mse_baseline     ! calc sine and cosine backround and stdev
c check_msefitfun       ! checks that selected fit function in avaiable for the 
c                       !   current shot
c dlag3p_deriv          ! double precision version of lag3p_deriv
c do_setup_and_beams    ! read the setup files and calc beam timing info
c get_beam_index        ! returns the beam index for a given channel
c get_mse_max_chans     ! returns mse_max_chans for dimensioning by idl,
c                       !   python, and other language routines
c get_mse_spatial_data  ! returns spatial_average_data saved in common as a 
c                       !   passable array
c get_mse_calibration   ! returns calibration saved in common as a 
c                       !   passable array
c get_number_of_beams_and_channels
c lag3pp_deriv          ! returns the derivative of the input tabulated data
c                       !   using a 3 point lagrange polynomial
c linear_interpolation  ! Linearly interpolates data onto a suppled time base
c make_spline           ! compute spline coefficients for a set of data
c mse_geometry          ! returns channel dependent geometric quantities
c open_for_read         ! opens an existing data file in preparation for 
c                       !   reading
c preprocess_mse_data   ! read mse data, calc background, extract when beam ON
c read_phy_data	        ! read in a diiid pointname and convert to volts
c read_syn_data         ! reads synthetic data from mdsplus
c read_getdat_data      ! reads in a diiid pointname with getdat_camac
c read_mse_data	        ! read mse data for a specified channel
c read_msetup           ! read in new mse setup file
c read_msetup5          ! read in new mse setup file msetup5.dat
c read_mcalgain         ! read in new mse calibration file
c read_mcalgain_rows    ! read a column oriented version of the mcalgain file
c read_mrz	        ! read in new rz calibration file and coefficients
c read_mcalgain3        ! read in calibration file for Moller fitting function
c read_mcalgain4        ! read in residual coeficient correction of fitfun 3
c read_mcalgain5        ! read in calibration file for Moller fitting function
c                       ! with bcoil used in place of bt
c read_mse_files        ! read in setup, calib, an rz files
c read_spatial_average  ! read spatial average files
c tangent_slope         ! compute gamma with the tangent slope fitting function
c tangent_offset        ! compute gamma with the tangent offset fitting function
c tangent_resid         ! compute correction to gamma from calibration residual
c
c--- Functions -----------------------------------------------------------------
c
c element               ! parse off next whitespace delimited string
c eval_spline           ! evalate a spline at a point
c get_beam_index        ! returns index of the beam corresponding to a channel
c trimlen               ! returns length of trimmed string.
c
c-------------------------------------------------------------------------------
c History
c
c  12-Nov-05 mam Converted msep_multi -> mse_lib2. 
c            Previous library computed gamma for a particular time and for a 
c            particular channel. This version computes gamma for all channels
c            at multiple times in a single call. 
c            Depricated items:
c              - abmode and lrmode
c              - mse_geometry routine
c              - no support for shots prior to 80540
c              - eliminate need to read sightline and viewport from file
c              - replace msetup.dat with msetup2.dat
c            New features:
c              - calculates Bt as a function of time rather than at a single 
c                time. This is performed in the routine avg_bt
c              - added routine bt_align to align Bt timebase with mse timebase
c              - added routines tangent_slope and tangent_offset
c              - added routine get_number_of_beams_and_channels
c  13-Dec-05 mam Eliminated the subset_valid_data routine
c  14-Dec-05 mam moved averaging_intervals from stark2 to this library.
c            Replaced avg_phy_data with average_data - latter routine chooses
c            beginning and end times based on calculation of indices rather 
c            than comparison with times. Time comparison has difficulties 
c            related to precision and round-off errors
c
c-------------------------------------------------------------------------------
c-------------------------------------------------------------------------------
c-------------------------------------------------------------------------------
! Recursive Fortran 95 quicksort routine
! sorts real numbers into ascending numerical order
! Author: Juli Rew, SCD Consulting (juliana@ucar.edu), 9/03
! Based on algorithm from Cormen et al., Introduction to Algorithms,
! 1997 printing

! Made F conformant by Walt Brainerd

! 9/9/2015 Bill Meyer <meyer8@llnl.gov> Modified to return a sorted index
! and not sort the valued array directory. Allows parallel arrays to be
! easily be sorted based on one array.

	module qsort_c_module
	
	implicit none
	public :: QsortC
	private :: Partition

	contains

	recursive subroutine QsortC(ind,A)
  	real, intent(in), dimension(:) :: A
  	integer, intent(in out), dimension(:) :: ind
  	integer :: iq
	
  	if(size(ind) > 1) then
     	call Partition(ind,A, iq)
     	call QsortC(ind(:iq-1),A)
     	call QsortC(ind(iq:),A)
  	endif
	end subroutine QsortC
	
	subroutine Partition(ind,A, marker)
  	real, intent(in), dimension(:) :: A
  	integer, intent(in out), dimension(:) :: ind
  	integer, intent(out) :: marker
  	integer :: i, j
  	real :: temp
  	real :: x      ! pivot point
  	x = A(ind(1))
  	i= 0
  	j= size(ind) + 1
	
  	do
     	j = j-1
     	do
        	if (A(ind(j)) <= x) exit
        	j = j-1
     	end do
     	i = i+1
     	do
        	if (A(ind(i)) >= x) exit
        	i = i+1
     	end do
     	if (i < j) then
        	! exchange ind(i) and ind(j)
        	temp = ind(i)
        	ind(i) = ind(j)
        	ind(j) = temp
     	elseif (i == j) then
        	marker = i+1
        	return
     	else
        	marker = i
        	return
     	endif
  	end do
	
	end subroutine Partition
	
	end module qsort_c_module
c-------------------------------------------------------------------------------
c routine: avgerage_data
c author:  dw, modification of avg_phy_data by michael d. brown, llnl
c descrpt: given a data array rdata of length np with corresponding time points
c          in tdata, return a boxcar average value over the time window
c          tm1 to tm2 and also return the stardard deviation. a warning message
c          is printed if there were no points found within the given time 
c          window.
c changes:
c
c   20-apr-93 dw  original version.
c   29-apr-97 bwr modified bksub_mode operation using  msesave_bksub
c
c-------------------------------------------------------------------------------
c Inputs:
c
c         tdata = time base array for data
c         rdata = data array
c            np = number of points in input arrays
c            t1 = initial time of averaging interval (seconds)
c            t2 = final time of averaging interval (seconds)
c   dump_string = if non-emppty, dump data and averaging results
c               = empty string for no output
c
c-------------------------------------------------------------------------------
c Output:
c
c      avgval = average of data on the interval (tm1,tm2)
c       sdval = standard deviation of the data on the interval (tm1,tm2)
c   n_samples = number of samples in the average
c
c-------------------------------------------------------------------------------

      subroutine average_data( tdata, rdata, np, t1, t2, dump_string, 
     $  avgval, sdval, n_samples )

      implicit  none
      include   'mse_lib2.inc'

      character*16 dump_string
      integer*4 np, n_samples
      real*4    tdata(1), rdata(1)
      real*4    t1, t2, avgval, sdval

      integer*4 i, i1, i2
      real*4    total
      
      if ( qlevel .ge. 3 ) print *, '(mse_lib2:average_data)'

      ! Find beginning and ending indices

      i1 = 1
      do while ( ( i1 .lt. np ) .and. ( tdata(i1) .le. t1 ) )
        i1 = i1 + 1
      enddo

      i2 = i1
      do while ( ( i2 .lt. np ) .and. ( tdata(i2) .le. t2 ) )
        i2 = i2 + 1
      enddo

      n_samples = i2 - i1 + 1
      
      if ( i1 .le. 0 ) then
        print *, '(mse_lib2:average_data) ERROR: Initial time (',
     $    t1, ') is out of range'
        return
      endif

      if ( i2 .gt. np ) then
        print *, '(mse_lib2:average_data) ERROR: Final time (',
     $    t2, ') is out of range'
        return
      endif

      ! Calculate average

      avgval = sum( rdata(i1:i2) ) / float( n_samples )

      ! Calculate standard deviation

      if ( n_samples .gt. 1 ) 
     $  sdval = sqrt( sum( ( rdata(i1:i2) - avgval )**2 ) / 
     $    float( n_samples - 1) )

      ! Print explicit values if selected

      if ( dump_string .ne. '' ) then
        total = 0.0
        print *, ' **** Data used to average ', dump_string
        print *, '             i       time        value       total'
        print *, '          -------------------------------------------'
        do i = i1, i2
          total = total + rdata(i)
          print '(10x,i6,3f12.6)', i, tdata(i), rdata(i), total
        enddo
        print '(1x,a,f10.6,a,f10.6,a,i5)', 'average =', avgval,
     $    '   std dev =', sdval, '   n_samples =', n_samples
        print *, ' '
      endif

      return
      end

c-------------------------------------------------------------------------------
c routine: avgerage_data2
c author:  dw, modification of avg_phy_data by michael d. brown, llnl
c descrpt: modified version of average_data in which multiple beam pulse are
c          averaged
c changes:
c
c   20-apr-93 dw  original version.
c   29-apr-97 bwr modified bksub_mode operation using  msesave_bksub
c
c-------------------------------------------------------------------------------
c Inputs:
c
c         tdata = time base array for data
c         rdata = data array
c          mask = binary array
c               = 1 for times where data is valid
c               = 0 for times where data is invalid
c            np = number of points in input arrays
c            t1 = initial time of averaging interval (seconds)
c            t2 = final time of averaging interval (seconds)
c   dump_string = if non-emppty, dump data and averaging results
c               = empty string for no output
c
c-------------------------------------------------------------------------------
c Output:
c
c      avgval = average of data on the interval (tm1,tm2)
c       sdval = standard deviation of the data on the interval (tm1,tm2)
c   n_samples = number of valid data points in the average
c
c-------------------------------------------------------------------------------

      subroutine average_data2( tdata, rdata, mask, np, i1, i2,
     $  dump_string, avgval, sdval, n_valid )

      implicit  none
      include   'mse_lib2.inc'

      character*16 dump_string
      integer*4 np, i1, i2, n_valid
      real*4    tdata(1), rdata(1), mask(1)
      real*4    avgval, sdval

      integer*4 i,count,worklen,status
      real*8    total
      integer*4, allocatable:: work(:)
      
      worklen = i2 - i1 + 1
      if(allocated(work)) deallocate(work)
      allocate(work(1:worklen),stat=status)
  
      if ( qlevel .ge. 9 ) print *, '(mse_lib2:average_data2)'

      ! Compute the average and standard deviation

      call rwhere(worklen,(mask(i1:i2) .gt. 0.0),count,work)
      n_valid=count
      if ( count .ge. 2 ) then
        avgval = sum( rdata(i1 + work(1:count) - 1))  / float(count)
        sdval = sqrt( dot_product( (rdata(i1+work(1:count)-1) 
     $    - avgval), ( rdata(i1+work(1:count)-1) - avgval ))
     $     / float( count - 1) )
      else if ( n_valid .ge. 1 ) then
        avgval = sum( rdata(i1+work(1:count)-1)) / float(count)
        sdval = mse_badchannel_std
      else
        avgval = 0.0
        sdval = mse_badchannel_std
      endif

      ! Print explicit values if selected

      if ( dump_string .ne. '' ) then
        total = 0.0
        print *, ' ******** Data used to average ', dump_string
        print *, ' '
        print *, '             i      time    mask    value      total'
        print *, '          -------------------------------------------'
        do i = i1-5, i2+5
          if ( ( i1 .le. i ) .and. ( i .le. i2 ) .and. 
     $              mask(i) .gt. 0.0) then 
            total = total + rdata(i)
            print '(10x,i6,f10.5,f6.1,2f11.5,a)', i, tdata(i), mask(i), 
     $        rdata(i), total, '  **'
          else
            print '(10x,i6,f10.5,f6.1,2f11.5)', i, tdata(i), mask(i), 
     $        rdata(i), total
          endif
        enddo
        print '(1x,a,f10.6,a,f10.6,a,i5)', 'average =', avgval,
     $    '   std dev =', sdval, '   n_samples =', n_valid
        print *, ' '
      endif

      return
      end

c-------------------------------------------------------------------------------
c routine: average_mse (the mother of all routines)
c descrpt: Given the mse-a and -b waveforms and the time and averaging interval
c          returns averaged values of the signals at the array of times 
c          provided together with the corresponding standard deviations
c changes: (dd-mm-yy)
c          13-12-06 started to write routine
c-------------------------------------------------------------------------------
c Inputs:
c
c       times = array of times (in ms)
c         n_t = number of valid times in the times array
c   msefitfun = 1 for tangent slope model
c             = 3 for tangetn offset model
c             = 4 for tangetn offset model with calibration residual correction
c     dt_full = full averaging width (in ms)
c
c-------------------------------------------------------------------------------
c Outputs:
c
c     ms_a_ave = average of msa for each time in times
c     ms_b_ave = average of msbor each time in times
c     ms_a_std = standard deviation of msa for each time in times
c     ms_b_std = standard deviation of msb for each time in times
c   tgamma_ave = average of tangent(gamma) for each time in times
c   tgamma_std = standard deviation of tangent(gamma) for each time in time
c       bt_ave = (50 ms) average of bt for each time in times
c   read_error = 0 if no error occured while reading data
c              = otherwise - error upon reading one or more of the requested
c                data arrays
c
c-------------------------------------------------------------------------------

      subroutine average_mse( shot, times, n_t, msefitfun, dt_full,
     $  ms_a_avg, ms_b_avg, ms_a_std, ms_b_std, tan_gamma_avg,
     $ tan_gamma_std, bt_avg, read_error )

      use qsort_c_module

      implicit none
      include  'mse_lib2.inc'

      integer*4 shot, n_t, msefitfun, bksub, read_error(max_mse_chans)
      integer*4 i_t,k
      real*4    dt_full
      real*4    times(n_t), bt_avg(n_t)
      real*4    ms_a_avg( n_t, max_mse_chans )
      real*4    ms_b_avg( n_t, max_mse_chans )
      real*4    ms_a_std( n_t, max_mse_chans )
      real*4    ms_b_std( n_t, max_mse_chans )
      real*4    tan_gamma_avg( n_t, max_mse_chans )
      real*4    tan_gamma_std( n_t, max_mse_chans )
      real*4    raxis( n_t )
      real*4    eval_linear


      character*10 bt_name /'BT        '/
      character*10 bc_name /'BCOIL     '/
      character*16 dump_sin, dump_cos, dump_gamma
      character*8  daq_sys
      character*80 rcstag 
     $      / '@(#) MSE Library CVS Tag: $Name: V5_02 $'/
      integer*4 kerror, n_bt, ierr, i_ch 
      real*4    bt_time(ptdata_max_pts), bt_data(ptdata_max_pts)
      real*4    time1
      integer*4 chan_mask(max_mse_chans),time_mask(n_t)
      integer*4 good_chans, good_ind(max_mse_chans)
      integer*4 ind(max_mse_chans)
      real*4    good_data(max_mse_chans),good_r(max_mse_chans)
      real*4    cer_correction(n_t,max_mse_chans)
      real*4    tan_gamma_avg_uncor( n_t, max_mse_chans )
      integer*4 status
      integer*4 ep /0/

      ep = 1


      entry average_mse_cer( shot, times, n_t, msefitfun, dt_full,
     $  ms_a_avg, ms_b_avg, ms_a_std, ms_b_std, tan_gamma_avg,
     $ tan_gamma_std, bt_avg, read_error, tan_gamma_avg_uncor)

      if(ep .ne. 1) then
	 ep = 2
      endif

      if ( qlevel .ge. 3 ) print *, '(mse_lib2:average_mse)'

      !-------------------------------------------------------------------------
      ! Initializations

      pi = 4.0 * atan(1.0)
      r2d = 180.0 / pi
      d2r = pi / 180.0

      ! Set flags for dumping data depending on qlevel

      if ( qlevel .ge. 6 ) then
        dump_sin = 'sin'
        dump_cos = 'cos'
        dump_gamma = 'gamma'
      else
        dump_sin = ''
        dump_cos = ''
        dump_gamma = ''
      endif

      ! Determine if the selected fitting function is valid for the specified
      ! shot

      call check_msefitfun( shot, msefitfun )


      ! Read the Bt data and calculate the average Bt at the supplied list of 
      ! times.

      time1 = -0.2
      if (shot .ge. first_bcoil_shot) then
        call read_phy_data( shot, bc_name, time1, ptdata_max_pts,
     $     bt_time, bt_data, n_bt, ierr )
      else
        call read_phy_data( shot, bt_name, time1, ptdata_max_pts,
     $     bt_time, bt_data, n_bt, ierr )
      endif
      if ( ierr .ne. 0 ) then
        print *, '(mse_lib2:average_mse) No Bt data for shot', shot
        ep = 0
        return
      endif

      ! Compute bt_avg at each time in times

      call avg_bt( times, n_t, 0.050, bt_data, bt_time, n_bt, bt_avg )

      ! Check on the Bt direction 

      call check_bt_direction(bt_data,bt_time,n_bt)

      ! Read the setup file, get the geometry info, and find the beam on/off 
      ! times (on_beg, on_end, and npulses are in common).

      call do_setup_and_beams( shot, kerror )
      if ( ( kerror .eq. 2 ) .or. ( kerror .eq. 8 ) ) then
        if ( qlevel .ge. 3 ) print '(1x,a,i2,a)',
     $    '(mse_lib2:average_mse) Error', kerror,
     $    ' from do_setup_and_beams'
        ep = 0
        return
      endif


      !-------------------------------------------------------------------------

      do i_ch = 1, n_mse_chans

      call average_mse_ch( shot, times, n_t, i_ch,msefitfun, dt_full,
     $  ms_a_avg(:,i_ch), ms_b_avg(:,i_ch), ms_a_std(:,i_ch), 
     $  ms_b_std(:,i_ch), tan_gamma_avg(:,i_ch), tan_gamma_std(:,i_ch), 
     $  bt_data, bt_time, n_bt,bt_avg,read_error(i_ch) )

      enddo ! End loop on channels

      if(ep .eq. 2)then
         tan_gamma_avg_uncor = tan_gamma_avg
      endif

      if ( use_cer .ne. 0) then
         do i_t = 1, n_t
            call rwhere(11,
     $ 	     (tan_gamma_std(i_t,1:11) .lt. mse_badchannel_std),
     $        good_chans,good_ind)
            if ( good_chans .gt. 0) then
	       good_data(1:good_chans) = 
     $   	    tan_gamma_avg(i_t,good_ind(1:good_chans))
               good_r(1:good_chans) = r(good_ind(1:good_chans)) 
	    ind(1:good_chans) = (/ (k,k=1,good_chans) /)
	    call QsortC(ind(1:good_chans),good_data(1:good_chans))
               raxis(i_t) = eval_linear( good_chans, 0.0,
     $          good_data(ind(1:good_chans)),good_r(ind(1:good_chans)))
	    endif

         enddo

       
         call mse_fastEr_correction(shot,n_t,raxis,
     $	     times,dt_full,cer_correction,status,tan_gamma_std)

         if ( qlevel .ge. 1 ) then
         print *,'(mse_lib2:average_data) Cer correction done'
         print *,'                       use_cer',use_cer
         print *,'                       certree',certree
         print *,'                       use_cer330',use_cer330
         print *,'                       use_cer210',use_cer210
         endif


         tan_gamma_avg = tan_gamma_avg * cer_correction


      endif



      ep = 0
      return
      end
c-------------------------------------------------------------------------------
c routine: average_mse_ch 
c descrpt: Given the mse-a and -b waveforms and the time and averaging interval
c          returns averaged values of the signals at the array of times 
c          provided together with the corresponding standard deviations
c changes: (dd-mm-yy)
c          13-12-06 started to write routine
c-------------------------------------------------------------------------------
c Inputs:
c
c       times = array of times (in ms)
c         n_t = number of valid times in the times array
c   msefitfun = 1 for tangent slope model
c             = 3 for tangetn offset model
c             = 4 for tangetn offset model with calibration residual correction
c     dt_full = full averaging width (in ms)
c
c-------------------------------------------------------------------------------
c Outputs:
c
c     ms_a_ave = average of msa for each time in times
c     ms_b_ave = average of msbor each time in times
c     ms_a_std = standard deviation of msa for each time in times
c     ms_b_std = standard deviation of msb for each time in times
c   tgamma_ave = average of tangent(gamma) for each time in times
c   tgamma_std = standard deviation of tangent(gamma) for each time in time
c       bt_ave = (50 ms) average of bt for each time in times
c   read_error = 0 if no error occured while reading data
c              = otherwise - error upon reading one or more of the requested
c                data arrays
c
c-------------------------------------------------------------------------------

      subroutine average_mse_ch( shot, times, n_t, i_ch,msefitfun, 
     $  dt_full,ms_a_avg, ms_b_avg, ms_a_std, ms_b_std, tan_gamma_avg,
     $ tan_gamma_std, bt_data,bt_time, n_bt,bt_avg,read_error )

      implicit none
      include  'mse_lib2.inc'


      integer*4 shot, n_t, msefitfun, bksub, read_error, i_ch
      real*4    dt_full
      real*4    times(n_t), bt_avg(n_t)
      real*4    ms_a_avg( n_t )
      real*4    ms_b_avg( n_t )
      real*4    ms_a_std( n_t )
      real*4    ms_b_std( n_t )
      real*4    tan_gamma_avg( n_t )
      real*4    gamma_avg( n_t )
      real*4    tan_gamma_std( n_t )

      character*10 bt_name /'BT        '/
      character*10 bc_name /'BCOIL     '/
      character*16 dump_sin, dump_cos, dump_gamma
      character*8  daq_sys
      character*80 rcstag 
     $     / '@(#) MSE Library CVS Tag: $Name: V5_02 $'/
      integer*4 kerror, n_bt, ierr, beam_index, get_beam_index
      real*4 max_signal_level, min_signal_level
      integer*4 mse_npts, npulses
      integer*4 i_avg_window( max_times, 5 ), i_t, i_r, i_slot, ii
      integer*4 n_samps, n_samps1, n_samps2, i_beg, i_end, n_avg
      integer*4 s1, s2, i_max(1), i
      logical*1 sin_saturation, cos_saturation, sin_low, cos_low
      logical*1 beam_off_too_long
      logical*1 tsin_saturation, tcos_saturation, tsin_low, tcos_low
      logical*1 tbeam_off_too_long
      real*4    time1, pre_beam_max_time, post_beam_min_time
      real*4    bt_time(ptdata_max_pts), bt_data(ptdata_max_pts)
      real*4    bt_aligned(ptdata_max_pts), mse_time(ptdata_max_pts)
      real*4    mse_sindata(ptdata_max_pts), mse_cosdata(ptdata_max_pts)
      real*4  lmse_sindata(ptdata_max_pts), lmse_cosdata(ptdata_max_pts)
      real*4    beam_mask_aligned(ptdata_max_pts),lgamma_avg(n_t)
      real*4    lbeam_mask_aligned(ptdata_max_pts)
      real*4    lbt_aligned(ptdata_max_pts)
      real*4    save_mask(ptdata_max_pts)
      real*4    lraw_gamma(ptdata_max_pts),lmse_time(ptdata_max_pts)
      real*4    raw_gamma(ptdata_max_pts)
      integer*4 lmse_npts
      real*4    on_beg(max_pulses), on_end(max_pulses)
      real*4    off_beg(max_pulses+1), off_end(max_pulses+1)
      real*4    unus_beg(max_pulses+1), unus_end(max_pulses+1)
      real*4    t1_beg, t1_end, t2_beg, t2_end,t1_avg,t2_avg,s
      real*4    avg_sin, avg_sin1, avg_sin2, std_sin, std_sin1, std_sin2
      real*4    avg_cos, avg_cos1, avg_cos2, std_cos, std_cos1, std_cos2
      real*4    avg_gam, avg_gam1, avg_gam2, std_gam, std_gam1, std_gam2
      real*4    std_cov, std_cov1, std_cov2
      real*4    gamma1, avg_gamma, t_avg
      real*4    bksin, bkcos, bksdsin, bksdcos
      real*4    lbksin, lbkcos, lbksdsin, lbksdcos
      real*4    tand
      save beam_index, daq_sys, save_mask
      integer*4 ep /0/

      ep = 1

      entry average_mse_rch( shot, times, n_t, i_ch,msefitfun, 
     $  dt_full,ms_a_avg, ms_b_avg, ms_a_std, ms_b_std, tan_gamma_avg,
     $  tan_gamma_std, bt_data,bt_time, n_bt,bt_avg,read_error, 
     $  lmse_sindata,lmse_cosdata,lraw_gamma,lmse_time,lmse_npts,
     $  lbksin,lbkcos,lbksdsin,lbksdcos )

      if(ep .eq. 0) then
	 ep = 2
      endif

      entry average_mse_rch2( shot, times, n_t, i_ch,msefitfun, 
     $  dt_full,ms_a_avg, ms_b_avg, ms_a_std, ms_b_std, tan_gamma_avg,
     $  tan_gamma_std, bt_data,bt_time, n_bt,bt_avg,read_error, 
     $  lmse_sindata,lmse_cosdata,lraw_gamma,lmse_time,lmse_npts,
     $  lbksin,lbkcos,lbksdsin,lbksdcos,lgamma_avg )

      if(ep .eq. 0) then
	 ep = 3
      endif

      entry average_mse_rch3( shot, times, n_t, i_ch,msefitfun, 
     $  dt_full,ms_a_avg, ms_b_avg, ms_a_std, ms_b_std, tan_gamma_avg,
     $  tan_gamma_std, bt_data,bt_time, n_bt,bt_avg,read_error, 
     $  lmse_sindata,lmse_cosdata,lraw_gamma,lmse_time,lmse_npts,
     $  lbksin,lbkcos,lbksdsin,lbksdcos,lgamma_avg,lbeam_mask_aligned)

      if(ep .eq. 0) then
	 ep = 4
      endif

      entry average_mse_rch4( shot, times, n_t, i_ch,msefitfun, 
     $  dt_full,ms_a_avg, ms_b_avg, ms_a_std, ms_b_std, tan_gamma_avg,
     $  tan_gamma_std, bt_data,bt_time, n_bt,bt_avg,read_error, 
     $  lmse_sindata,lmse_cosdata,lraw_gamma,lmse_time,lmse_npts,
     $  lbksin,lbkcos,lbksdsin,lbksdcos,lgamma_avg,lbeam_mask_aligned,
     $  lbt_aligned)

      if(ep .eq. 0) then
	 ep = 5
      endif

      if ( qlevel .ge. 3 ) then
         if(ep .eq. 1) print *, '(mse_lib2:average_mse_ch)'
         if(ep .eq. 2) print *, '(mse_lib2:average_mse_rch)'
      endif

      !-------------------------------------------------------------------------
      ! Initializations

      pi = 4.0 * atan(1.0)
      r2d = 180.0 / pi
      d2r = pi / 180.0

      ! Set flags for dumping data depending on qlevel

      if ( qlevel .ge. 6 ) then
        dump_sin = 'sin'
        dump_cos = 'cos'
        dump_gamma = 'gamma'
      else
        dump_sin = ''
        dump_cos = ''
        dump_gamma = ''
      endif

        if ( qlevel .ge. 2 ) print *, '(mse_lib2:average_mse_ch)'
     $    // ' Processing channel:', i_ch

        ! Read the mse data

        call read_mse_data( shot, i_ch, -0.050, mse_time, mse_sindata, 
     $    mse_cosdata, mse_npts, read_error )

        if ( qlevel .gt. 3 ) then
          print *, ' Reading mse data for channel', i_ch, 'of shot', 
     $      shot,read_error
        endif



        ! Depending on the channel, the data acquistion channel system 
        ! and/or the beam can change as the channel number increases. When
        ! this happens, the relevant beam mask and time base may change
        ! requiring that all the waveforms be realigned.

        if ( ( data_acq_sys(i_ch) .eq. 'none' ) .or.
     $    ( read_error .ne. 0 ) ) then
          do i_t = 1, n_t
            ms_a_avg( i_t ) = 0.0
            ms_a_std( i_t ) = mse_badchannel_std
            ms_b_avg( i_t ) = 1.0e-20
            ms_b_std( i_t ) = mse_badchannel_std
            tan_gamma_avg( i_t ) = 0.0
            gamma_avg( i_t ) = 0.0
            tan_gamma_std( i_t ) = mse_badchannel_std
          enddo
	  beam_index = 0
          ep = 0
          return
        else if ( ( i_ch .eq. 1 ) .or. 
     $    ( beam_index .ne. get_beam_index(i_ch) ) .or. 
     $    ( daq_sys .ne. data_acq_sys(i_ch) ) ) then

          beam_index = get_beam_index(i_ch)
          daq_sys = data_acq_sys(i_ch)

          call align_data( bt_data, bt_time, n_bt, mse_time, mse_npts,
     $      bt_aligned )

          call beam_timing( beam_index, mse_time, mse_npts, on_beg, 
     $      on_end, off_beg, off_end, unus_beg, unus_end,
     $      npulses, beam_mask_aligned )

        if ( qlevel .gt. 3 ) then
          print *, 'Beam_Timing for beam_index ',beam_index ,npulses
        endif

          ! For each time in times, determine whether it falls during a beam
          ! blip or between beam blips and define an averaging interval
          ! accordingly: ave_windows( i_time, t11, t12, t21, t22 ).
          ! This is used in conjunction with the beam masks to calculate the
          ! average quantities

          call averaging_intervals( i_avg_window, times, n_t, dt_full, 
     $      mse_time, mse_npts, on_beg, on_end, off_beg, off_end, 
     $      unus_beg, unus_end, npulses )
        
	  save_mask = beam_mask_aligned
        else
          beam_mask_aligned = save_mask
        endif
        
        if ( qlevel .ge. 6 ) then
          print *, ' '
          do i_t = 1, n_t
            s1 = sum( beam_mask_aligned( i_avg_window( i_t, 2 ) :
     $        i_avg_window( i_t, 3 )) )
            if ( i_avg_window( i_t, 1 ) .eq. 2 ) then
              s2 = sum( beam_mask_aligned( i_avg_window( i_t, 2 ) :
     $          i_avg_window( i_t, 3 ) ) )
              print *, i_t, s1, s2
            else
              print *, i_t, s1
            endif
          enddo
          print *, ' '
        endif

        if ( qlevel .ge. 66 ) then
          do i_t = 1, mse_npts
            print *, i_t, beam_mask_aligned(i_t), mse_sindata(i_t), 
     $        mse_cosdata(i_t)
          enddo
        endif

        ! Retrieve the mse sin and cos data and compute and subtract the 
        ! baseline from the data

        call preprocess_mse_data2( shot, i_ch, mse_sindata, mse_cosdata,
     $    mse_time, mse_npts, on_beg, on_end, off_beg, off_end, 
     $    unus_beg, unus_end, npulses,
     $    read_error,bksin,bkcos,bksdsin,bksdcos )

        if ( qlevel .ge. 6 ) print *, 'Preprocess out:',daq_sys,
     $    mse_npts,mse_time(1), mse_sindata(1), mse_cosdata(1)

        ! Max & min signal level is dependent on whether acquisition is by
        ! camac or pcs

        if ( daq_sys .eq. 'pcs' ) then
          max_signal_level =  9.90
          min_signal_level = -9.90
        else
          max_signal_level = 4.95
          min_signal_level = 0.05
        endif

        ! Compute the raw tan(gamma). This is used to compute the standard
        ! deviation in gamma

        if ( msefitfun .eq. 3 ) then
          if ( qlevel .ge. 4 ) print *, 'Using tangent_offset'
          do i_r = 1, mse_npts
	    raw_gamma(i_r) = 100.0
            if ( (mse_sindata(i_r) .gt. min_signal_level)  .and.
     $           (mse_cosdata(i_r) .gt. min_signal_level)  .and.
     $           (abs(mse_sindata(i_r))  .lt. max_signal_level) .and.
     $           (abs(mse_cosdata(i_r))  .lt. max_signal_level))then
               call tangent_offset( i_ch, mse_sindata(i_r), 
     $        mse_cosdata(i_r), bt_aligned(i_r), gamma1 )
              raw_gamma(i_r) = gamma1 * d2r 
            else
	      beam_mask_aligned(i_r) = 0.0
            endif
          enddo
        else if ( msefitfun .eq. 4 ) then
          if ( qlevel .ge. 4 ) print *, 
     $	    'Using tangent_offset with calibration residual correction'
          do i_r = 1, mse_npts
	    raw_gamma(i_r) = 100.0
            if ( (mse_sindata(i_r) .gt. min_signal_level)  .and.
     $           (mse_cosdata(i_r) .gt. min_signal_level)  .and.
     $           (abs(mse_sindata(i_r))  .lt. max_signal_level) .and.
     $           (abs(mse_cosdata(i_r))  .lt. max_signal_level))then
               call tangent_offset( i_ch, mse_sindata(i_r), 
     $        mse_cosdata(i_r), bt_aligned(i_r), gamma1 )
              raw_gamma(i_r) = gamma1
               call tangent_resid( i_ch, gamma1)
              raw_gamma(i_r) = gamma1 * d2r
            else
	      beam_mask_aligned(i_r) = 0.0
            endif
          enddo
        else
          if ( qlevel .ge. 4 ) print *, 'Using tangent_slope'
          do i_r = 1, mse_npts
	    raw_gamma(i_r) = 100.0
            if ( (mse_sindata(i_r) .gt. min_signal_level)  .and.
     $           (mse_cosdata(i_r) .gt. min_signal_level)  .and.
     $           (abs(mse_sindata(i_r))  .lt. max_signal_level) .and.
     $           (abs(mse_cosdata(i_r))  .lt. max_signal_level))then
               call tangent_slope( i_ch, mse_sindata(i_r), 
     $        mse_cosdata(i_r), bt_aligned(i_r), gamma1 )
              raw_gamma(i_r) = gamma1 * d2r
            else
	      beam_mask_aligned(i_r) = 0.0
            endif
          enddo
        endif

        tsin_saturation = .false.
        tcos_saturation = .false.
        tsin_low = .false.
        tcos_low = .false.
	tbeam_off_too_long = .false.

        ! Compute average and standard deviation of signals
        do i_t = 1, n_t

          sin_saturation = .false.
          cos_saturation = .false.
          sin_low = .false.
          cos_low = .false.
	  beam_off_too_long = .false.

          if ( ( i_avg_window( i_t, 1 ) .eq. 1 ) .or. !-------------------------
     $      ( i_avg_window( i_t, 1 ) .eq. 3 ) ) then  

            ! Single continuous interval (with masking)

            t1_beg = mse_time( i_avg_window( i_t, 2 ) )
            t1_end = mse_time( i_avg_window( i_t, 3 ) )

            if ( qlevel .ge. 6 ) print *, 'Averaging sin on a single '
     $        // 'interval: (', t1_beg, t1_end, ')'
            call average_data2( mse_time, mse_sindata, 
     $        beam_mask_aligned, mse_npts, i_avg_window( i_t, 2 ), 
     $        i_avg_window( i_t, 3 ), dump_sin, avg_sin, std_sin, 
     $        n_samps )
            if ( abs(avg_sin) + std_sin .ge. max_signal_level )
     $        sin_saturation = .true.
            if ( avg_sin .le. min_signal_level ) sin_low = .true.

            if ( qlevel .ge. 6 ) print *, 'Averaging cos on a single '
     $        // 'interval: (', t1_beg, t1_end, ')'
            call average_data2( mse_time, mse_cosdata, 
     $        beam_mask_aligned, mse_npts, i_avg_window( i_t, 2 ), 
     $        i_avg_window( i_t, 3 ), dump_cos, avg_cos, std_cos, 
     $        n_samps )
            if ( abs(avg_cos) + std_cos .ge. max_signal_level )
     $        cos_saturation = .true.
            if ( avg_cos .le. min_signal_level ) cos_low = .true. 

            ! Compute the standard deviation in gamma

            if ( qlevel .ge. 5 ) print *, 'Averaging gamma on a single'
     $        // ' interval: (', t1_beg, t1_end, ')'

            call average_data2( mse_time, raw_gamma, beam_mask_aligned,
     $        mse_npts, i_avg_window( i_t, 2 ), i_avg_window( i_t, 3 ), 
     $        dump_gamma, avg_gam, std_gam, n_samps )


	    avg_gamma = avg_gam * r2d
 	    std_gam = std_gam * r2d

	    if((times(i_t) .lt. t1_beg) .or. 
     $		    (times(i_t) .gt. t1_end)) then
		std_gam = mse_badchannel_std
            endif


          else if ( i_avg_window( i_t, 1 ) .eq. 2 ) then !----------------------

            ! Two discrete intervals - interpolate between them

            t1_beg = mse_time( i_avg_window( i_t, 2 ) )
            t1_end = mse_time( i_avg_window( i_t, 3 ) )
            t2_beg = mse_time( i_avg_window( i_t, 4 ) )
            t2_end = mse_time( i_avg_window( i_t, 5 ) )

	    ! check that beam hasn't been off too long for
	    ! interpolation
	    if(max_t_beam_off .gt. 0.0 .and. 
     $         max_t_beam_off .le. (t2_beg - t1_end)) then 
	       beam_off_too_long = .true.
               tbeam_off_too_long = .true.
            endif

	    ! mse_strict non-zero is like a max_t_beam_off time of 
	    ! zero seconds
	    if(mse_strict .ne. 0) beam_off_too_long = .true.

            if ( qlevel .ge. 5 ) then
              print *, 'Interpoating gamma based on intervals [ (',
     $          t1_beg, t1_end, ') + (', t2_beg, t2_end, ') ]'
            endif

            if ( qlevel .ge. 6 ) print *, 
     $        'Averaging sin on interval 1: (', t1_beg, t1_end, ')'
            call average_data2( mse_time, mse_sindata, 
     $        beam_mask_aligned, mse_npts, i_avg_window( i_t, 2 ),
     $        i_avg_window( i_t, 3 ), dump_sin, avg_sin1, std_sin1,
     $        n_samps1 )
            if ( abs(avg_sin1) + std_sin1 .ge. max_signal_level ) 
     $        sin_saturation = .true.
            if ( avg_sin1 .le. min_signal_level ) sin_low = .true.

            if ( qlevel .ge. 6 ) print *, 
     $        'Averaging cos on interval 1: (', t1_beg, t1_end, ')'
            call average_data2( mse_time, mse_cosdata, 
     $        beam_mask_aligned, mse_npts, i_avg_window( i_t, 2 ),
     $        i_avg_window( i_t, 3 ), dump_cos, avg_cos1, std_cos1,
     $        n_samps1 )
            if ( abs(avg_cos1) + std_cos1 .ge. max_signal_level )
     $        cos_saturation = .true.
            if ( avg_cos1 .le. min_signal_level ) cos_low = .true. 

            if ( qlevel .ge. 6 ) print *, 
     $        'Averaging sin on interval 2: (', t2_beg, t2_end, ')'
            call average_data2( mse_time, mse_sindata, 
     $        beam_mask_aligned, mse_npts, i_avg_window( i_t, 4 ),
     $        i_avg_window( i_t, 5 ), dump_sin, avg_sin2, std_sin2,
     $        n_samps2 )
            if ( abs(avg_sin2) + std_sin2 .ge. max_signal_level ) 
     $         sin_saturation = .true.
            if ( avg_sin2 .le. min_signal_level ) sin_low = .true.

            if ( qlevel .ge. 6 ) print *,
     $        'Averaging cos on interval 2: (', t2_beg, t2_end, ')'
            call average_data2( mse_time, mse_cosdata, 
     $        beam_mask_aligned, mse_npts, i_avg_window( i_t, 4 ),
     $        i_avg_window( i_t, 5 ), dump_cos, avg_cos2, std_cos2,
     $        n_samps2 )
            if ( abs(avg_cos2) + std_cos2 .ge. max_signal_level )
     $        cos_saturation = .true.
            if ( avg_cos2 .le. min_signal_level ) cos_low = .true. 


            ! Compute the standard deviation in gamma

            if ( qlevel .ge. 6 ) print *, 
     $        'Averaging gamma on interval 1: (', t1_beg, t1_end, ')'
            call average_data2( mse_time, raw_gamma, beam_mask_aligned,
     $        mse_npts, i_avg_window( i_t, 2 ), i_avg_window( i_t, 3 ),
     $        dump_gamma, avg_gam1, std_gam1, n_samps1 )

            if ( qlevel .ge. 6 ) print *, 
     $        'Averaging gamma on interval 2: (', t2_beg, t2_end, ')'
            call average_data2( mse_time, raw_gamma, beam_mask_aligned,
     $        mse_npts, i_avg_window( i_t, 4 ), i_avg_window( i_t, 5 ),
     $        dump_gamma, avg_gam2, std_gam2, n_samps2 )

            ! Linear interpolate between values

            t_avg = 0.5 * ( t2_end + t2_beg - t1_end - t1_beg )
	    t1_avg = 0.5 * (t1_end + t1_beg)
	    t2_avg = 0.5 * (t2_end + t2_beg)
	    s = (avg_gam2 - avg_gam1) / t_avg
            avg_gamma = s * (times(i_t) - t1_avg) + avg_gam1
            std_gam = std_gam1**2 + ( std_gam2**2 - std_gam1**2 )
     $        * (times(i_t) - t1_avg)**2 / t_avg**2 
	    if(std_gam .gt. 0.0) then
		std_gam = sqrt( std_gam)
	    else 
		std_gam = mse_badchannel_std
            endif

	    avg_gamma = avg_gamma * r2d
 	    std_gam = std_gam * r2d

	    if((times(i_t) .lt. t1_beg) .or. 
     $		    (times(i_t) .gt. t2_end)) then
		std_gam = mse_badchannel_std
            endif
	    n_samps = n_samps1 + n_samps2


          endif ! End averaging logic ------------------------------------------

          if ( ( qlevel .ge. 5 ) .and. 
     $      ( i_avg_window( i_t, 1 ) .ne. 0 ) ) then
            print *, '                Channel', i_ch 
            print *, '  Flags: sin_saturation', sin_saturation 
            print *, '                sin_low', sin_low
            print *, '         cos_saturation', cos_saturation
            print *, '                cos_low', cos_low
            print *, '      beam_off_too_long', beam_off_too_long
          else if (( qlevel .ge. 1 ) .and. 
     $      ( sin_saturation .or.  sin_low .or. cos_saturation .or.
     $        cos_low  .or.  beam_off_too_long )) then
            print *, '                Channel', i_ch 
            print *, '  Flags: sin_saturation', sin_saturation 
            print *, '                sin_low', sin_low
            print *, '         cos_saturation', cos_saturation
            print *, '                cos_low', cos_low
            print *, '      beam_off_too_long', beam_off_too_long
	    endif
            if (sin_saturation) tsin_saturation=.true.
            if (cos_saturation) tcos_saturation=.true.
            if (sin_low) tsin_low = .true.
            if (cos_low) tcos_low = .true.


          if ( ( i_avg_window( i_t, 1 ) .ne. 0 ) .and. 
     $      ( .not. sin_saturation ) .and. ( .not. sin_low ) .and.
     $      ( .not. cos_saturation ) .and. ( .not. cos_low ) .and.
     $      ( .not. beam_off_too_long )) then
            ms_a_avg( i_t ) = avg_sin
            ms_a_std( i_t ) = std_sin
            ms_b_avg( i_t ) = avg_cos
            ms_b_std( i_t ) = std_cos
            tan_gamma_avg( i_t ) = tan(avg_gamma*d2r)
	    gamma_avg(i_t) = avg_gamma
            std_gam1 = sqrt(mse_systematic_error**2 + 
     $               	    std_gam**2)*d2r
            tan_gamma_std( i_t ) =  std_gam1 / 
     $                         cos(avg_gamma*d2r)**2
          else
            ms_a_avg( i_t ) = 0.0
            ms_a_std( i_t ) = mse_badchannel_std
            ms_b_avg( i_t ) = 1.0e-20
            ms_b_std( i_t ) = mse_badchannel_std
            tan_gamma_avg( i_t ) = 0.0
            gamma_avg( i_t ) = 0.0
            tan_gamma_std( i_t ) = mse_badchannel_std
          endif

        enddo ! End loop on times


        if (( qlevel .eq. 0 ) .and. 
     $      ( tsin_saturation .or.  tsin_low .or. tcos_saturation .or.
     $        tcos_low  .or.  tbeam_off_too_long )) then
            
            print *, 'Error on one, or more, values for channel:', i_ch
            print *, '  Flags: sin_saturation', tsin_saturation 
            print *, '                sin_low', tsin_low
            print *, '         cos_saturation', tcos_saturation
            print *, '                cos_low', tcos_low
            print *, '      beam_off_too_long', tbeam_off_too_long
        endif

        if ( qlevel .ge. 5 ) then
          print *, ' '
          print *, 'Channel:', i_ch
          print *, ' EFIT   N                                     Ave',
     $      '     Std       Ave     Std        Tan        Std'
          print *, ' TIME Intvl  t11     t12     t21     t22      Cos',
     $      '     Cos       Sin     Sin       Gamma      Gamma'
          print *, '-------------------------------------------------',
     $      '-------------------------------------------------- '
          do i_t = 1, n_t
            print '( 1x, f6.4, i3, 4f8.4, 4f9.4, 2f11.6 )', times(i_t),
     $        i_avg_window( i_t, 1 ),
     $        ( mse_time( i_avg_window( i_t, i_slot ) ), 
     $        i_slot = 2, 5 ), ms_b_avg( i_t ), 
     $        ms_b_std( i_t ), ms_a_avg( i_t ), 
     $        ms_a_std( i_t ), tan_gamma_avg( i_t ), 
     $        tan_gamma_std( i_t )
          enddo
          print *, ' '
        endif

      if(ep .eq. 2)then
	  lmse_sindata(:) = mse_sindata(:)
	  lmse_cosdata(:) = mse_cosdata(:)
	  lraw_gamma(:) = raw_gamma(:)
	  lmse_time(:) = mse_time(:)
	  lmse_npts = mse_npts
	  lbksin = bksin
	  lbkcos = bkcos
	  lbksdsin = bksdsin
	  lbksdcos = bksdcos
      endif
      if(ep .eq. 3)then
	  lmse_sindata(:) = mse_sindata(:)
	  lmse_cosdata(:) = mse_cosdata(:)
	  lraw_gamma(:) = raw_gamma(:)
	  lgamma_avg(:) = gamma_avg(:)
	  lmse_time(:) = mse_time(:)
	  lmse_npts = mse_npts
	  lbksin = bksin
	  lbkcos = bkcos
	  lbksdsin = bksdsin
	  lbksdcos = bksdcos
      endif
      if(ep .eq. 4)then
	  lmse_sindata(:) = mse_sindata(:)
	  lmse_cosdata(:) = mse_cosdata(:)
	  lraw_gamma(:) = raw_gamma(:)
	  lgamma_avg(:) = gamma_avg(:)
	  lmse_time(:) = mse_time(:)
	  lmse_npts = mse_npts
	  lbksin = bksin
	  lbkcos = bkcos
	  lbksdsin = bksdsin
	  lbksdcos = bksdcos
          lbeam_mask_aligned(:) = beam_mask_aligned(:)
      endif
      if(ep .eq. 5)then
	  lmse_sindata(:) = mse_sindata(:)
	  lmse_cosdata(:) = mse_cosdata(:)
	  lraw_gamma(:) = raw_gamma(:)
	  lgamma_avg(:) = gamma_avg(:)
	  lmse_time(:) = mse_time(:)
	  lmse_npts = mse_npts
	  lbksin = bksin
	  lbkcos = bkcos
	  lbksdsin = bksdsin
	  lbksdcos = bksdcos
          lbeam_mask_aligned(:) = beam_mask_aligned(:)
          lbt_aligned(:) = bt_aligned(:)
      endif
      ep = 0

      return
      end

c-------------------------------------------------------------------------------
c routine: beam_timing
c descrpt: Forms a beam mask and determines the beam on/off times
c
c
c-------------------------------------------------------------------------------
c Inputs:
c
c     i_bm = index of beam to generate the timing intervals for
c   mse_time = mse time base
c   mse_npts = number of mse data points
c
c-------------------------------------------------------------------------------
c Outpus:
c
c      on_beg = array containing the first times that a beam blip in fully on
c      on_end = array containing the last times that a beam blip is fully on
c     off_beg = array containing the first times of a beam off interval
c                may be used for baseline
c     off_end = array containing the last times of a beam off interval
c                may be used for baseline
c     unus_beg = array containing the first times of a beam off interval
c                30r may be on or off
c     unus_end = array containing the last times of a beam off interval
c                30r may be on or off
c    n_pulses = number of valid beam pulse found in the beam waveform
c    beam_mask_aligned = an array of 0s (data invalid) and 1s (data valid)
c
c-------------------------------------------------------------------------------

      subroutine beam_timing( i_bm, mse_time, mse_npts, on_beg, on_end, 
     $  off_beg, off_end, unus_beg, unus_end, npulses, 
     $  beam_mask_aligned )

      implicit  none
      include   'mse_lib2.inc'

      integer*4 i_bm, mse_npts, npulses, i
      real*4    mse_time(1)
      real*4    on_beg(max_pulses), on_end(max_pulses)
      real*4    off_beg( max_pulses + 1 ), off_end( max_pulses + 1 )
      real*4    unus_beg(max_pulses+1), unus_end(max_pulses+1)
      real*4    beam_mask_aligned(1),beam_data_aligned(mse_npts,2)
      real*4    begoff, endoff

      integer*4 i_t, beam_min_on_points, i_p, i_q,ii, n_shift, m_shift
      integer*4 beam_rise_points,beam_fall_points, beam_min_off_points
      logical*1 beam_on, beam_off,bound_found
      real*4    beam_mask(ptdata_max_pts)
      real*4    dt_mse

      if ( qlevel .ge. 2 ) print *, '(mse_lib2:beam_timing) Beam ',i_bm

      ! Create the beam mask. For the 210 beam assume that the 210rt and
      ! 210lt beams have the same time base

      ! Minimum time (number of points) the beam must be for the beam to be 
      ! considered "on"

      dt_mse = ( mse_time(mse_npts) - mse_time(1) ) / 
     $  float( mse_npts - 1 )
      beam_min_on_points = ( beam_min_on_time + dt_mse / 2.0 ) / dt_mse
      beam_min_off_points = ( multi_pulse_bksub_min_width + 
     $     dt_mse / 2.0 ) / dt_mse
      beam_rise_points = ( beam_rise_time + dt_mse / 2.0 ) / dt_mse
      beam_fall_points = ( beam_fall_time + dt_mse / 2.0 ) / dt_mse

      last_on_diagbeam = -100.0
      if ( i_bm .eq. 1 ) then
        first_on_diagbeam = beam_time_base(n_beam_pts(1),1)
        call linear_interpolation( beam_data(:,1), beam_time_base(:,1), 
     $    n_beam_pts(1), mse_time, mse_npts, beam_data_aligned(:,1))
        call linear_interpolation( beam_data(:,4), beam_time_base(:,4), 
     $    n_beam_pts(4), mse_time, mse_npts, beam_data_aligned(:,2))

        do i=1,mse_npts
           if ( beam_data_aligned(i,1) .gt. beam_on_level(1) .and.
     $          (ok_30rt .ne. 0 .or.
     $          beam_data_aligned(i,2) .le. beam_on_level(4))) then
             beam_mask_aligned(i) = 3.0 
           elseif ( beam_data_aligned(i,1) .gt. beam_on_level(1) .and.
     $          (ok_30rt .eq. 0 .and.
     $          beam_data_aligned(i,2) .gt. beam_on_level(4))) then
             beam_mask_aligned(i) = 2.0
           elseif ( beam_data_aligned(i,2) .gt. beam_on_level(4)) then
             beam_mask_aligned(i) = 1.0
           else
             beam_mask_aligned(i) = 0.0
           endif
           if (( beam_data(i,1) .gt. beam_on_level(1) .or.
     $          beam_data(i,4) .gt. beam_on_level(4)) .and.
     $	       first_on_diagbeam .gt. beam_time_base(i,1)) then
		first_on_diagbeam = beam_time_base(i,1)
	   endif
	enddo
        do i=n_beam_pts(1),1,-1
           if (( beam_data(i,1) .gt. beam_on_level(1) .or.
     $          beam_data(i,4) .gt. beam_on_level(4)) .and.
     $	       last_on_diagbeam .eq. -100.0) then
		last_on_diagbeam = beam_time_base(i,1)
		diagbeam_inc = 
     $		   beam_time_base(i+1,1)-beam_time_base(i,1)
	   endif
	enddo


      else
        first_on_diagbeam = beam_time_base(n_beam_pts(1),2)
        call linear_interpolation( beam_data(:,2), beam_time_base(:,2), 
     $    n_beam_pts(2), mse_time, mse_npts, beam_data_aligned(:,1))
        call linear_interpolation( beam_data(:,3), beam_time_base(:,3), 
     $    n_beam_pts(3), mse_time, mse_npts, beam_data_aligned(:,2))

        do i=1,mse_npts
           if ( beam_data_aligned(i,1) .gt. beam_on_level(2) .and. 
     $          (ok_210lt .ne. 0 .or.
     $          beam_data_aligned(i,2) .le. beam_on_level(3))) then
             beam_mask_aligned(i) = 3.0
           else if ( beam_data_aligned(i,1) .gt. beam_on_level(2) .and. 
     $          (ok_210lt .eq. 0 .and.
     $          beam_data_aligned(i,2) .gt. beam_on_level(3))) then
             beam_mask_aligned(i) = 2.0
           else if ( beam_data_aligned(i,2) .gt. beam_on_level(3)) then
             beam_mask_aligned(i) = 1.0
           else
             beam_mask_aligned(i) = 0.0
           endif
           if (( beam_data(i,2) .gt. beam_on_level(2) .or.
     $          beam_data(i,3) .gt. beam_on_level(3)) .and.
     $	       first_on_diagbeam .gt. beam_time_base(i,2)) then
		first_on_diagbeam = beam_time_base(i,2)
	   endif
	enddo
        do i=n_beam_pts(2),1,-1
           if (( beam_data(i,2) .gt. beam_on_level(2) .or.
     $          beam_data(i,3) .gt. beam_on_level(3)) .and.
     $	       last_on_diagbeam .eq. -100.0) then
		last_on_diagbeam = beam_time_base(i,2)
		diagbeam_inc = 
     $		   beam_time_base(i+1,2)-beam_time_base(i,2)
	   endif
	enddo

      endif


      ! Dump information on timinig in so desired

      if ( qlevel .ge. 6 ) then
        print *, '                i_bm:', i_bm
        print *, '    beam_min_on_time:', beam_min_on_time
        print *, '            mse_npts:', mse_npts
        print *, '  beam_min_on_points:', beam_min_on_points
        print *, '              dt_mse:', dt_mse
	print *, ' first diagnostic beam on time: ', 
     $	          first_on_diagbeam
	print *, ' last diagnostic beam on time: ', 
     $	          last_on_diagbeam
	print *, ' diagnostic beam time inc: ', 
     $	          diagbeam_inc
      endif

      ! Determine on/off times

      i_t = 1
      npulses = 0
      begoff = 1
      endoff = mse_npts
      beam_off = .true.
      off_beg(npulses:) = begoff
      off_end(npulses:) = endoff
      

      do while( ( i_t .le. mse_npts ) .and. ( npulses .lt. max_pulses )
     $  )

        ! Determine the indices when the beam comes on
         
        beam_on = .false.
       
        do while( i_t .le. mse_npts .and. ( .not. beam_on ) )
          if ( beam_mask_aligned(i_t) .le. 0.5 .and. 
     $          (.not. beam_off)) then
             begoff = i_t
             endoff = i_t
             beam_off = .true.
          else if ( beam_mask_aligned(i_t) .le. 0.5 .and. 
     $          beam_off) then
             endoff = i_t
          else
             beam_off = .false.
          endif 
c          print *,i_t,npulses,mse_time(i_t),beam_mask_aligned(i_t),
c     $           beam_off,begoff,endoff
          if ( beam_mask_aligned(i_t) .ge. 2.5 ) then
            npulses = npulses + 1
            if((endoff - begoff) .ge. beam_min_off_points) then
               off_beg(npulses:) = begoff
               off_end(npulses:) = endoff
            endif
            on_beg(npulses) = i_t
            beam_on = .true.
          endif
          i_t = i_t + 1
        enddo
         
        ! Determine when the beam goes off and check if the beam is on
        ! for the minimum duration
        if(i_t .lt. mse_npts) on_end(npulses) = i_t+1
        do while( i_t .le. mse_npts .and. beam_on )
          if ( beam_mask_aligned(i_t) .ge. 2.5 ) then
            i_t = i_t + 1
          else
            beam_on = .false.
            on_end(npulses) = i_t 
            if ((on_end(npulses) - on_beg(npulses)) 
     $              .lt. beam_min_on_points ) 
     $        npulses = npulses - 1
          endif
        enddo
            
      enddo ! end outer while
      off_beg(npulses+1) = begoff
      off_end(npulses+1) = endoff

      ! Convert indices to times and derive the beam OFF times which are 
      ! used for multi-bksub
      beam_mask_aligned(1:mse_npts) = 0.0
      off_beg(1) = mse_time(1)
      unus_beg(1) = mse_time(1)
      do i_p = 1, npulses
        beam_mask_aligned(on_beg(i_p)+beam_rise_points:on_end(i_p)) = 1.0
        unus_beg(i_p+1) = mse_time(on_end(i_p)+1)
        unus_end(i_p) = mse_time(on_beg(i_p)-1 + beam_rise_points)
        off_end(i_p) = mse_time( off_end(i_p) - beam_rise_points)
        off_beg( i_p) = 
     $            mse_time( off_beg(i_p) + beam_fall_points)
        on_beg(i_p) = mse_time( on_beg(i_p) + beam_rise_points)
        on_end(i_p) = mse_time( on_end(i_p))
        
      enddo
      unus_end(npulses+1) = mse_time(mse_npts)
      off_end(npulses+1) = mse_time( off_end(npulses+1) - 
     $            beam_rise_points)
      off_beg(npulses+1) = 
     $            mse_time( off_beg(npulses+1) + beam_fall_points)
      !off_beg( npulses + 1 ) = last_on_diagbeam + beam_fall_time
      !off_end( npulses + 1 ) = mse_time(off)

    
      ! Dump diagnostic information

      if ( qlevel .ge. 4 ) then
        print *, ' '
        print *, 'Beam: ', i_bm
        print *, 'mse_npts: ', mse_npts
        print *, 'n_pulses: ', npulses
        print *, 'Blip Off-beg   Off-end  On-beg    On-end '
        print *, '    UnUs-beg  Unus-end'
        print *, '------------------------------------------- '
        do i_p = 1, npulses
          print '( i4, 2f10.4 )', i_p, off_beg(i_p) * 1000.0,
     $      off_end(i_p) * 1000.0
          print '( i4, 2f10.4 )', i_p, unus_beg(i_p) * 1000.0,
     $      unus_end(i_p) * 1000.0
          print '( i4, 20x, 2f10.4 )', i_p, on_beg(i_p) * 1000.0,
     $      on_end(i_p) * 1000.0
        enddo
        print '( i4, 2f10.4 )', i_p, off_beg( npulses + 1 ) * 
     $    1000.0, off_end( npulses + 1 ) * 1000.0
        print '( i4, 2f10.4 )', i_p, unus_beg( npulses + 1 ) * 
     $    1000.0, unus_end( npulses + 1 ) * 1000.0
        print *, ' '
     
      endif
        
      ! Dump everything to ensure things work

      if ( qlevel .ge. 12 ) then
        print *, '*****************************', mse_npts, i_p, i_bm,
     $    npulses
        do i_t = 1, mse_npts
          bound_found = .false.
          do i_p = 1, npulses
            if ( on_beg(i_p) .eq. mse_time(i_t) ) then
              print *, i_t, beam_mask_aligned(i_t), mse_time(i_t),
     $          '  On begin', i_p, on_beg(i_p)
              bound_found = .true.
              exit
            else if ( on_end(i_p) .eq. mse_time(i_t) ) then
              print *, i_t, beam_mask_aligned(i_t), mse_time(i_t),
     $          '    On end', i_p, on_end(i_p)
              bound_found = .true.
              exit
            else if ( off_beg(i_p) .eq. mse_time(i_t) ) then
              print *, i_t, beam_mask_aligned(i_t), mse_time(i_t),
     $          ' Off begin', i_p, off_beg(i_p)
              bound_found = .true.
              exit
            else if ( off_end(i_p) .eq. mse_time(i_t) ) then
              print *, i_t, beam_mask_aligned(i_t), mse_time(i_t),
     $          '   Off end', i_p, off_end(i_p)
              bound_found = .true.
              exit
            else if ( unus_beg(i_p) .eq. mse_time(i_t) ) then
              print *, i_t, beam_mask_aligned(i_t), mse_time(i_t),
     $          ' Unusable begin', i_p, unus_beg(i_p)
              bound_found = .true.
              exit
            else if ( unus_end(i_p) .eq. mse_time(i_t) ) then
              print *, i_t, beam_mask_aligned(i_t), mse_time(i_t),
     $          '   Unusuable end', i_p, unus_end(i_p)
              bound_found = .true.
              exit
            endif
          enddo
          if ( .not. bound_found ) print *, i_t, beam_mask_aligned(i_t),
     $      mse_time(i_t)
        enddo

      endif

      return
      end

c-------------------------------------------------------------------------------
c routine: averaging_intervals
c descrpt: Given the beam on/off times, determines segment(s) of the mse 
c          waveform to use in order to obtain the an averaged pitch angle
c          at a particular time. This routines differs slightly from 
c          averaging_intervals_v1 in the values used for time2 and time22
c author:  m. makowski
c
c changes:
c
c   13-dec-05 mam extracted routine from stark2 and made into a subroutine
c   21-dec-06 mam completely reworked the entire routine
c
c-------------------------------------------------------------------------------
c Inputs:
c
c      times = array of times for which an averaged pitch angle is desired
c        n_t = number of times
c    dt_full = desired full width of the averaging interval
c   mse_time = time base for the mse_data
c   mse_npts = number of points in the mse_data array
c     on_beg = array containing the first times that a beam blip in fully on
c     on_end = array containing the last times that a beam blip is fully on
c    off_beg = array containing the first times of a beam off interval
c              may be used for baseline
c    off_end = array containing the last times of a beam off interval
c              may be used for baseline
c   unus_beg = array containing the first times of a beam off interval
c              30r may be on or off
c   unus_end = array containing the last times of a beam off interval
c              30r may be on or off
c    npulses = number of pulses (working dimension of on_beg and on_end; 
c              dimension of off_beg and off_end is npulse+1)
c
c-------------------------------------------------------------------------------
c Output:
c
c   i_ave_widow = 2D array containing the information on the averaging
c                 intervals derived for each averaging time in the array times
c                 - The value of i_ave_window( *, 1 ) is in the range 0 to 3 
C                   where
c                   0 indicates that no valid data was available at that time
c                   1 indicates that a single interval was found. In this case
c                     i_ave_window(i, 1:2) are the indices in mse_time that
c                     define the beginning and end of the valid data range
c                   2 indicates that time of interest falls between two beam
c                     blips and that interpolation is needed to define an 
c                     appropriate average. In this case i_ave_window(i, 1:2) 
c                     and i_ave_window(i, 3:4) are the pairs of indices in
c                     mse_time that are to be used in the interpolation
c                   3 indicates that multiple beam blips are spanned by the
c                     averaging interval and that a beam mask should be used 
c                     to compute the average. In this case i_ave_window(i, 1:2)
c                     are the indices in mse_time that define the beginning and 
c                     end of the valid data range
c
c-------------------------------------------------------------------------------
c Notes:
c
c   All times in seconds
c
c-------------------------------------------------------------------------------

      subroutine averaging_intervals( i_ave_window, times, n_t, dt_full,
     $  mse_time, mse_npts, on_beg, on_end, off_beg, off_end, 
     $  unus_beg, unus_end, npulses )

      implicit none
      include  'mse_lib2.inc'

      integer*4 n_t, npulses, i_ave_window( max_times, 5 ), mse_npts
      real*4    times(n_t), dt_full, mse_time(ptdata_max_pts)
      real*4    on_beg(max_pulses), on_end(max_pulses)
      real*4    off_end(max_pulses+1), off_beg(max_pulses+1)
      real*4    unus_beg(max_pulses+1), unus_end(max_pulses+1)

      integer*4 pulse_num, i_t, i_c, i_t_lhs(1), i_t_rhs(1)
      logical*1 beam_on, between_blips, modify_averaging_interval
      real*4    time1_on, time1_off, time2_on, time2_off
      real*4    t_lhs_want, t_rhs_want

      if ( qlevel .ge. 3 ) print *, '(mse_lib2.averaging_intervals)',
     $  ' Calculating averages for ', n_t, ' times'

      pulse_num = 1
      do i_t = 1, n_t

        modify_averaging_interval = .false.
        beam_on = .false.
        between_blips = .false.

        t_lhs_want = times(i_t) - dt_full / 2.0
        t_rhs_want = times(i_t) + dt_full / 2.0

        if ( qlevel .ge. 6 ) print *, ' Averaging time: ', times(i_t)

        ! Requested time falls before first beam-on interval or after
        ! last beam-on interval

        if ( ( times(i_t) .le. on_beg(1) ) .or. ( times(i_t) .ge. 
     $    on_end(npulses) ) ) then
          i_ave_window( i_t, 1 ) = 0
          if ( qlevel .ge. 3 ) print *, 
     $      '(mse_lib2:averaging_intervals) No data at time', 
     $      times(i_t) * 1000.0
          cycle
        endif

        ! Special case if on last beam-on pulse

        if ( ( ( times(i_t) .ge. on_beg(npulses) ) ) .and. 
     $    ( times(i_t) .lt. on_end(npulses) ) ) then
          pulse_num = npulses
          beam_on = .true.
        endif

        ! Determine if the time is on a blip or between blips

        do while ( ( pulse_num .le. npulses - 1 ) .and. 
     $    ( .not. beam_on ) .and. ( .not. between_blips ) )
          if ( ( on_beg(pulse_num) .le. times(i_t) ) .and. ( times(i_t) 
     $      .le. on_end(pulse_num) ) ) then
            beam_on = .true.
          else if ( ( on_end( pulse_num ) .lt. times(i_t) ) .and. 
     $      ( times(i_t) .lt. on_beg( pulse_num + 1 ) ) ) then
            between_blips = .true.
          else
            pulse_num = pulse_num + 1
          endif
        enddo

        if ( qlevel .ge. 6 ) then
          print *, '   Pulse number = ', pulse_num
          print *, '   Desired interval: (', t_lhs_want, t_rhs_want, ')'
          print *, '   Nearest off: (', t_lhs_want, t_rhs_want, ')'
        endif

        if ( beam_on ) then

          if ( ( on_beg(pulse_num) .le. t_lhs_want ) .and. ( t_rhs_want
     $      .le. on_end(pulse_num) ) ) then

            ! Requested time interval occurs completely within a single period 
            ! when the beam is on

            i_ave_window( i_t, 1 ) = 1
            i_ave_window( i_t:i_t, 2 ) = minloc( abs( 
     $        mse_time(1:mse_npts) - t_lhs_want ) )
            i_ave_window( i_t:i_t, 3 ) = minloc( abs( 
     $        mse_time(1:mse_npts) - t_rhs_want ) )

            if ( qlevel .ge. 6 ) print *, '   Beam on, case 1'

          else if ( ( unus_beg(pulse_num) .le. t_lhs_want ) .or.
     $      ( t_rhs_want .le. unus_end(pulse_num+1) ) ) then

            ! In this case only one blip falls within the desired averaging 
            ! interval AND t_lhs_want and/or t_rhs_want fall in a beam-off 
            ! interval. The averaging interval must be modified in this 
            ! instance

            i_ave_window( i_t, 1 ) = 1
            i_ave_window( i_t:i_t, 2 ) = minloc( abs( 
     $        mse_time(1:mse_npts) - max( t_lhs_want, on_beg(pulse_num) 
     $        ) ) )
            i_ave_window( i_t:i_t, 3 ) = minloc( abs( 
     $        mse_time(1:mse_npts) - min( t_rhs_want, on_end(pulse_num) 
     $        ) ) )

            if ( qlevel .ge. 2 ) print *, 'INFORMATION: '
     $        // 'averaging interval centered at ', times(i_t) * 1000.0,
     $        ' modified to (', mse_time( i_ave_window( i_t, 2 ) ) * 
     $        1000.0, mse_time( i_ave_window( i_t, 3 ) ) * 1000.0, ')'

            if ( qlevel .ge. 6 ) print *, '   Beam on, case 2'

          else 

            ! At this point portions of more than two beam-on intervals are 
            ! caputred by the averaging interval. However, exceptional 
            ! situations arise for the first and last beam-on periods

            if ( t_lhs_want .lt. off_beg(1) ) then
              i_t_lhs = minloc( abs( mse_time(1:mse_npts) - 
     $          on_beg(1) ) )
              modify_averaging_interval = .true.
            else
              i_t_lhs = minloc( abs( mse_time(1:mse_npts) - 
     $          t_lhs_want ) )
            endif

            if ( t_rhs_want .gt. unus_end( npulses + 1 ) ) then
              i_t_rhs = minloc( abs( mse_time(1:mse_npts) - 
     $          on_end(npulses) ) )
              modify_averaging_interval = .true.
            else
              i_t_rhs = minloc( abs( mse_time(1:mse_npts) - 
     $          t_rhs_want ) )
            endif

            i_ave_window( i_t, 1 ) = 3
            i_ave_window( i_t, 2 ) = i_t_lhs(1)
            i_ave_window( i_t, 3 ) = i_t_rhs(1)

            if ( qlevel .ge. 6 ) print *, '   Beam on, case 3'

            if ( modify_averaging_interval .and. ( qlevel .ge. 2 ) ) 
     $        print *, 'INFORMATION: Averaging interval centered at ',
     $        times(i_t) * 1000.0, ' is between beam blips. Using '
     $        // 'weighted average on (', mse_time(i_t_lhs) * 1000.0,
     $        mse_time(i_t_rhs) * 1000.0, ')'
            if ( ( ( mse_time(i_t_rhs(1)) - mse_time(i_t_lhs(1)) ) .lt. 
     $        dt_full ) .and. (  qlevel .ge. 2 ) ) print *, 
     $        '    WARNING: Above interval is less than requested '
     $        // 'averaging value (', dt_full * 1000.0, 'ms)'

          endif

        else if ( between_blips ) then

          if ( qlevel .ge. 6 ) print *, '     Bounds:',
     $      on_beg(pulse_num), on_end( pulse_num + 1 )

          if ( ( t_lhs_want .lt. on_beg(pulse_num) ) .and.
     $      ( on_end( pulse_num + 1 ) .lt. t_rhs_want ) ) then

            ! Capture the entirety of at least two pulses. Use a beam 
            ! mask to compute the average

            i_ave_window( i_t, 1 ) = 3
            i_ave_window( i_t:i_t, 2 ) = minloc( abs( 
     $        mse_time(1:mse_npts) - t_lhs_want ) )
            i_ave_window( i_t:i_t, 3 ) = minloc( abs( 
     $        mse_time(1:mse_npts) - t_rhs_want ) )

            if ( qlevel .ge. 6 ) print *, '   Beam off, Case 1'

          else

            ! The following case are treated here
            ! - averaging interval entirely within a beam-off interval
            ! - averaging interval only captures a portion of either
            !   preceding or trailing beam-on intervals
            ! - averaging interval captures portions of both the preceding
            !   and trailing beam-on intervals, but an insufficient number 
            !   of points are captured to meet the minimum number of point
            !   to form an average
            ! Use an interpolated value to compute the average

            i_ave_window( i_t, 1 ) = 2
            i_ave_window( i_t:i_t, 2 ) = minloc( abs(  
     $        mse_time(1:mse_npts) - max( on_beg(pulse_num), 
     $        on_end(pulse_num) - dt_full ) ) )
            i_ave_window( i_t:i_t, 3 ) = minloc( abs(  
     $        mse_time(1:mse_npts) - on_end(pulse_num) ) )
            i_ave_window( i_t:i_t, 4 ) = minloc( abs(  
     $        mse_time(1:mse_npts) - on_beg( pulse_num + 1 ) ) )
            i_ave_window( i_t:i_t, 5 ) = minloc( abs(  
     $        mse_time(1:mse_npts) - min( on_end( pulse_num + 1 ), 
     $        on_beg( pulse_num + 1 ) + dt_full ) ) )

            if ( qlevel .ge. 2 ) then 
              print *, 'INFORMATION: Averaging interval centered at ',
     $          times(i_t) * 1000.0
              print *, '  modified to [ (', 
     $          mse_time( i_ave_window(i_t,2) ) * 1000.0,
     $          mse_time( i_ave_window(i_t,3) ) * 1000.0, ') + (',
     $          mse_time( i_ave_window(i_t,4) ) * 1000.0,
     $          mse_time( i_ave_window(i_t,5) ) * 1000.0, ') ]'
            endif

            if ( qlevel .ge. 6 ) print *, '   Beam off, Case 2'

            endif

        endif ! end beam-on/between-blips logic

      enddo ! end loop on times

      if ( qlevel .ge.5 ) then
        print *, ' '
        print *, '       N'
        print *, '  i_t Int   i11     i12     i21     i22'
        print *, '----------------------------------------'
        do i_t = 1, n_t
          print '(1x,2i4,4i8)', i_t, ( i_ave_window(i_t,i_c), 
     $      i_c = 1, 5 )
        enddo
        print *, ''
      endif

      return
      end

c-------------------------------------------------------------------------------
c routine: get_number_of_beams_and_channels
c author:  mike makowski
c descrp:  returns the number of beams and mse channels available for a given 
c          shot. Also initializes the sightline_for_chan and viewport arrays.
c changes:
c
c   08-nov-05 mam created routine
c   17-nov-05 mam added coding for sightline_for_chan and viewport array
c             initialization
c
c-------------------------------------------------------------------------------
c Notes:
c
c   After shot 80540 all channels are abmode = A/B and lrmode = L. In addition,
c   for shots between 80540 = first_16chan_shot and 91300 = first_35chan_shot, 
c   the channel setup was identical:
c
c  A/B L 1   A/B L 2   A/B L 3   A/B L 4   A/B L 5   A/B L 6   A/B L 7  A/B L 8 
c  A/B L 10  A/B L 11  A/B L 12  A/B L 13  A/B L 14  A/B L 15  A/B L 16 A/B L 17
c
c-------------------------------------------------------------------------------
c Inputs:
c
c   shot = shot number
c
c-------------------------------------------------------------------------------
c Output:
c
c   Sets the common block variables, n_mse_channels and n_beams
c
c-------------------------------------------------------------------------------

      subroutine get_number_of_beams_and_channels( shot )

      implicit none
      include 'mse_lib2.inc'

      integer*4 shot

      integer*4 i_ch

      if ( qlevel .ge. 3 ) print *, 
     $  '(mse_lib2:get_number_of_beams_and_channels)'

      ! Get number of channels, number of sightlines, and populate viewport

      if ( shot .le. 74150 ) then
        print *, ' No MSE data is available before 74150'
        return
      else if ( shot .lt. first_16chan_shot ) then
        n_mse_chans = 8
        n_sightlines = 12
        print *, ' Shots prior to 80540 are no longer supported'
      else if ( shot .lt. first_35chan_shot ) then
        n_mse_chans = 16
        n_sightlines = 18
        do i_ch = 1, n_mse_chans
          if ( i_ch .le. 8 ) then
            viewport(i_ch) = '315 '
          else
            viewport(i_ch) = '45  '
          endif
        enddo
      else if ( shot .lt. first_36chan_shot ) then
        n_mse_chans = 35
        n_sightlines = 35
        do i_ch = 1, n_mse_chans
          if ( i_ch .le. 16 ) then
            viewport(i_ch) = '315 '
          else if ( i_ch .le. 25 ) then 
            viewport(i_ch) = '45  '
          else
            viewport(i_ch) = '15  '
          endif
        enddo
      else if ( shot .lt. first_40chan_shot ) then
        n_mse_chans = 36
        n_sightlines = 36
        do i_ch = 1, n_mse_chans
          if ( i_ch .le. 11 ) then
            viewport(i_ch) = '315 '
          else if ( i_ch .le. 26 ) then 
            viewport(i_ch) = '45  '
          else
            viewport(i_ch) = '15  '
          endif
        enddo
      else if ( shot .lt. first_45chan_shot ) then
        n_mse_chans = 40
        n_sightlines = 40
        do i_ch = 1, n_mse_chans
          if ( ( i_ch .le. 11 ) .or. ( i_ch .ge. 37 ) ) then
            viewport(i_ch) = '315 '
          else if ( i_ch .le. 26 ) then 
            viewport(i_ch) = '45  '
          else
            viewport(i_ch) = '15  '
          endif
        enddo
      else if ( shot .lt. first_69chan_shot ) then
        n_mse_chans = 45
        n_sightlines = 45
        do i_ch = 1, n_mse_chans
          if ( ( i_ch .le. 11 ) .or. ( i_ch .ge. 37 ) ) then
            viewport(i_ch) = '315 '
          else if ( i_ch .le. 26 ) then 
            viewport(i_ch) = '45  '
          else
            viewport(i_ch) = '15  '
          endif
        enddo
      else
        n_mse_chans = 69
        n_sightlines = 69
        do i_ch = 1, n_mse_chans
          if ( ( i_ch .le. 11 ) .or. 
     $      ( ( i_ch .ge. 37 ) .and. ( i_ch .le. 45 ) ) ) then
            viewport(i_ch) = '315 '
          else if ( i_ch .le. 26 ) then 
            viewport(i_ch) = '45  '
          else if ( i_ch .le. 36 ) then
            viewport(i_ch) = '15  '
          else if ( i_ch .le. 53 ) then
            viewport(i_ch) = '195l'
          else
            viewport(i_ch) = '195u'
          endif
        enddo
      endif

      ! Populate the sightline mapping array. Differs from channel number only
      ! for the 8 and 16 channel configurations

      if ( shot .ge. first_35chan_shot ) then
        do i_ch = 1, n_mse_chans
          sightline_for_chan(i_ch) = i_ch
        enddo
      else if ( shot .ge. first_16chan_shot ) then
        do i_ch = 1, n_mse_chans
          if ( i_ch .le. 8 ) then
            sightline_for_chan(i_ch) = i_ch
          else
            sightline_for_chan(i_ch) = i_ch + 1
          endif
        enddo
      endif

      ! Populate the energy array

      do i_ch = 1, n_mse_chans
        if ( ( 37 .le. i_ch ) .and. ( i_ch .le. 40 ) ) then
          energy(i_ch) = 'half'
        else
          energy(i_ch) = 'full'
        endif
      enddo

      ! Populate the data_acq_sys array

      if ( shot .lt. first_69chan_shot ) then
        do i_ch = 1, n_mse_chans
          data_acq_sys(i_ch) = 'camac' 
        enddo
      else if ( shot .ge. first_allpcs_shot) then	
        do i_ch = 1, n_mse_chans
          data_acq_sys(i_ch) = 'pcs' 
        enddo
      else
        do i_ch = 1, n_mse_chans
          if ( ( 37 .le. i_ch ) .and. ( i_ch .le. 40 ) ) then
            data_acq_sys(i_ch) = 'none'
          else if ( i_ch .eq. 45 ) then
            data_acq_sys(i_ch) = 'none'
          else if ( ( ( 41 .le. i_ch ) .and. ( i_ch .le. 44 ) ) .or.
     $      ( i_ch .ge. 46 ) ) then
            data_acq_sys(i_ch) = 'pcs'
          else
            data_acq_sys(i_ch) = 'camac'
          endif
        enddo
      endif

      ! Set the number of beams

      if ( shot .lt. first_69chan_shot ) then
        n_beams = 1
      else
        n_beams = 4
      endif

      return
      end

c-------------------------------------------------------------------------------
c routine: check_msefitfun
c descrpt: checks that the selected fit function is valid for the current the
c          current shot. If not, it set msefitfun to the default value of 3
c changes:
c
c   11/16/05 mam wrote routine
c
c-------------------------------------------------------------------------------
c Inputs:
c
c   shot = shot number
c
c-------------------------------------------------------------------------------
c Output:
c
c   Valid setting for msefitfun
c
c-------------------------------------------------------------------------------

      subroutine check_msefitfun( shot, msefitfun )

      include   'mse_lib2.inc'

      integer*4 shot, msefitfun
      

      
      if ( (shot .lt. 108800) .and. (msefitfun .ne. 1) ) then
        print *, '(mse_lib2:check_msefitfun) Selected fitting',
     $    msefitfun, ' function is not available for this shot (', shot,
     $    ')'
        print *, ' Reverting to default fitting function ' //
     $    '(msefitfun = 1)'
        msefitfun = 1

      else if ( (shot .ge. 108800) .and. (shot .lt. 130601) .and.
     $           (msefitfun .ne. 1) .and. (msefitfun .ne. 3) ) then
        print *, '(mse_lib2:check_msefitfun) Selected fitting',
     $    msefitfun, ' function is not available for this shot (', shot,
     $    ')'
        print *, ' Reverting to default fitting function ' //
     $    '(msefitfun = 1)'
        msefitfun = 1

      else if ( (shot .ge. 130601) .and. (shot .lt. 143020) .and.
     $           (msefitfun .ne. 3)  ) then
        print *, '(mse_lib2:check_msefitfun) Selected fitting',
     $    msefitfun, ' function is not available for this shot (', shot,
     $    ')'
        print *, ' Reverting to default fitting function ' //
     $    '(msefitfun = 3)'
        msefitfun = 3

      else if (  (shot .ge. 143020) .and. 
     $           (msefitfun .ne. 3) .and. ( msefitfun .ne. 4 ) ) then
        print *, '(mse_lib2:check_msefitfun) Selected fitting',
     $    msefitfun, ' function is not available for this shot (', shot,
     $    ')'
        print *, ' Reverting to default fitting function ' //
     $    '(msefitfun = 3)'
        msefitfun = 3


      endif

      fitfun = msefitfun

      return
      end

c-------------------------------------------------------------------------------
c function: get_beam_index
c descrpt:  returns the beam index corresponding to a given channel
c changes:
c
c   11/15/05 mam wrote routine
c
c-------------------------------------------------------------------------------
c Inputs:
c
c   chan = channel number
c
c-------------------------------------------------------------------------------
c Output:
c
c   Returns the index of the beam corresponding to the channel
c
c-------------------------------------------------------------------------------

      function get_beam_index( chan )

      implicit  none

      integer*4 chan
      integer*4 get_beam_index

      if ( chan .le. 45 ) then
        get_beam_index = 1  !  30lt
      else
        get_beam_index = 2  ! 195rt
      endif

      return
      end

c-------------------------------------------------------------------------------
c routine: check_bt_direction
c author:  bill meyer 
c descr:   set the bt direction, norm vs reverse, from the read data
c changes:
c          07-mar-22 wm - first added
c
c-------------------------------------------------------------------------------
c Inputs:
c
c   bt_data = Bt data array
c   bt_time = Bt time base
c      n_bt = number of points in Bt arrays
c
c-------------------------------------------------------------------------------
c Outputs:
c
c    None, bt_direction is in the include file common block.
c
c-------------------------------------------------------------------------------
c
c-------------------------------------------------------------------------------

      subroutine check_bt_direction( bt_data, bt_time, n_bt )

      implicit  none
      include   'mse_lib2.inc'

      integer*4 n_t, n_bt
      real*4    bt_data(ptdata_max_pts), bt_time(ptdata_max_pts)

      character*16 empty /''/
      integer*4 i_t, i1, i2, n_samples
      real*4    sdval, time1, time2

   
      if( abs(minval(bt_data)) .le. abs(maxval(bt_data))) then
         bt_direction = revBt
         if ( qlevel .ge. 1 ) 
     *        print *, '(mse_lib2:check_bt_direction) reverse Bt'
      else
         bt_direction = normBt
         if ( qlevel .ge. 1 ) 
     *        print *, '(mse_lib2:check_bt_direction) normal Bt'
      endif

      return
      end
c-------------------------------------------------------------------------------
c routine: avg_bt
c author:  mike makowski
c descr:   read in the bt signal for the given shot, and average it for the
c          given time interval (t0-dt_full/2 to t0+dt_full/2). a
c          warning message is printed if the bt data is not available and
c          the routine returns ierr=1.
c changes:
c          08-nov-05 mam - modified version of avg_bt to accomodate an array of
c                    times
c
c-------------------------------------------------------------------------------
c Inputs:
c
c     times = array of times corresponding to the centers of averaging intervals
c             (seconds)
c       n_t = number of time values
c   dt_full = full width of averaging interval (seconds)
c   bt_data = Bt data array
c   bt_time = Bt time base
c      n_bt = number of points in Bt arrays
c
c-------------------------------------------------------------------------------
c Outputs:
c
c   btavg = average value of bt in the interval (t0 - dt_full/2, t0 + dt_full/2)
c    ierr = error return: 0=ok, 1=error
c
c-------------------------------------------------------------------------------
c Note:
c
c   Bt waveform has multiple time domains (i.e. time base is not uniformly
c   incremented)
c
c-------------------------------------------------------------------------------

      subroutine avg_bt( times, n_t, dt_full, bt_data, bt_time, n_bt,
     $  btavg )

      implicit  none
      include   'mse_lib2.inc'

      integer*4 n_t, n_bt
      real*4    bt_data(ptdata_max_pts), bt_time(ptdata_max_pts)
      real*4    times(1), dt_full, btavg(1)

      character*16 empty /''/
      integer*4 i_t, i1, i2, n_samples
      real*4    sdval, time1, time2

      if ( qlevel .ge. 3 ) print *, '(mse_lib2:avg_bt)'

      do i_t = 1, n_t
        time1 = times(i_t) - dt_full / 2.0
        time2 = times(i_t) + dt_full / 2.0
        if ( time1 .lt. bt_time(1) ) time1 = bt_time(1)
        if ( time2 .gt. bt_time(n_bt) ) time2 = bt_time(n_bt)

        i1 = 1
        do while ( ( i1 .le. n_bt ) .and. ( bt_time(i1) .lt. time1 ) )
          i1 = i1 + 1
        enddo

        i2 = i1
        do while ( ( i2 .le. n_bt ) .and. ( bt_time(i2) .le. time2 ) )
          i2 = i2 + 1
        enddo
        if ( bt_time(i2) .gt. time2 ) i2 = i2 - 1

        n_samples = i2 - i1 + 1
      
	if(n_samples .gt. 0) then
	    btavg(i_t) = sum( bt_data(i1:i2) ) / float( n_samples )
	else 
	    btavg(i_t) = 0.0
        endif

        if ( qlevel .ge. 10 ) print *, ' Average Bt at time', 
     $    times(i_t), ' is ', btavg(i_t)
      enddo

      return
      end

c-------------------------------------------------------------------------------
c routine: align_data
c author:  
c descrpt: interpolates a waveform onto the mse timebase
c change:
c          
c   11/16/05 mam wrote routine
c
c-------------------------------------------------------------------------------
c Inputs:
c
c        data = data waveform
c   time_base = time_base of data waveform
c      n_data = number of data points
c    mse_time = mse waveform timebase
c    mse_npts = number of valid points in timebase
c
c-------------------------------------------------------------------------------
c Output:
c
c   aligned_waveform = data waveform interpolated onto the mse timebase
c
c-------------------------------------------------------------------------------

      subroutine align_data( data, time_base, n_data, mse_time,
     $  mse_npts, aligned_waveform )

      implicit  none
      include   'mse_lib2.inc'

      integer*4 n_data, mse_npts
      real*4    data(1), time_base(1), mse_time(1), aligned_waveform(1)

      integer*4 i_t
      real*4    spline_b(ptdata_max_pts), spline_c(ptdata_max_pts)
      real*4    spline_d(ptdata_max_pts), eval_spline

      if ( qlevel .ge. 3 ) print *, '(mse_lib2:align_data)'

      call make_spline( n_data, time_base, data, spline_b, spline_c,
     $  spline_d )

      do i_t = 1, mse_npts
        aligned_waveform(i_t) = eval_spline( n_data, mse_time(i_t),
     $    time_base, data, spline_b, spline_c, spline_d )
      enddo

      return
      end

c-------------------------------------------------------------------------------
c routine: linear_interpolation
c author:  m. makowski
c descrpt: linearly interpolates a waveform onto a supplied time base
c
c Changes (dd/mm/yy):
c
c    04/01/07 wrote routine
c
c-------------------------------------------------------------------------------
c Inputs:
c
c             data = data waveform
c   data_time_base = time_base of data waveform
c           n_data = number of data points
c    new_time_base = new time base
c            n_new = dimension of new time base
c
c-------------------------------------------------------------------------------
c Output:
c
c   aligned_data = data waveform interpolated onto the mse timebase
c
c-------------------------------------------------------------------------------

      subroutine linear_interpolation( waveform, waveform_time_base,
     $  n_waveform, new_time_base, n_new, aligned_waveform )

      implicit  none
      include   'mse_lib2.inc'

      integer*4 n_waveform, n_new
      real*4    waveform_time_base(1), waveform(1)
      real*4    new_time_base(1), aligned_waveform(1)

      integer*4 i_n, i_d, i
      real*4    t_new

      if ( qlevel .ge. 3 ) print *, '(mse_lib2:linear_interpolation)'

      ! Set aligned waveform equal to the first data value for new times prior
      ! to the beginning of the data record

      i_d = 1
      i_n = 1
      do while ( new_time_base(i_n) .le. waveform_time_base(1) )
        aligned_waveform(i_n) = waveform(1)
        i_n = i_n + 1
      enddo

      ! New inch through the new time base interpolating as one progresses

      do while ( ( i_n .le. n_new ) .and. ( i_d .le. n_waveform ) )
        t_new = new_time_base(i_n)
        do while ( t_new .gt. waveform_time_base(i_d) .and. 
     $    ( i_d .le. n_waveform ) )
          i_d = i_d + 1
        enddo
        
        ! Perform the interpolation

        aligned_waveform(i_n) = waveform(i_d-1) + 
     $    ( ( t_new - waveform_time_base(i_d-1) ) / ( 
     $    waveform_time_base(i_d) - waveform_time_base(i_d-1) ) ) * 
     $    ( waveform(i_d) - waveform(i_d-1) )

        if ( qlevel .ge. 16 ) print *, i_n, i_d, 
     $    waveform_time_base(i_d-1), t_new, waveform_time_base(i_d), 
     $    waveform(i_d-1), aligned_waveform(i_n), waveform(i_d)

        ! Increment and continue

        i_n = i_n + 1

      enddo

      ! Set any remaining points to the value of the last point in the 
      ! waveform

      do i = i_n, n_new
        aligned_waveform(i) = waveform(n_waveform)
      enddo

      return
      end

c-------------------------------------------------------------------------------
c routine: calc_mse_baseline
c author:  michael d. brown, llnl
c descrpt: given the cos and sin arrays and corresponding time array (all of
c          length mse_npts), and given the time before any beams came on, and
c          the time when all beams went off, calculate the average cos and sin
c          background and their respective standard deviation. the desire is to
c          average 1 second of data 0.2 seconds before the beams come on, and
c          average 1 second of data 0.2 seconds after the beams turn off. the
c          window will be shrunk if not enough pre/post data points are
c          available.
c changes:
c          01/27/92 mdb original version.
c
c-------------------------------------------------------------------------------
c Inputs:
c
c         mse_npts = number of points in data arrays.
c         mse_time = mse time array
c      mse_sindata = mse sin data array
c      mse_cosdata = mse cos data array
c             i_bm = beam index for mse_data
c    user_prewidth = specific max_prewidth to use (used for bksub_mode = 1 only)
c   user_postwidth = specific max_postwidth to use (used for bksub_mode = 1 
c                    only)
c      user_pre_t1 = specific pre-pulse begin-time (used for bksub_mode = 1 
c                    only)
c     user_post_t2 = specific post-pulse end-time (used for bksub_mode = 1 only)
c          preprox = time between prepulse and data pulse
c         postprox = time between postpulse and data pulse
c
c-------------------------------------------------------------------------------
c Output:
c
c     bksin = average sine background.
c     bkcos = average cosine background.
c   bksdsin = average sine background stdev.
c   bksdcos = average cosine background stdev.
c
c-------------------------------------------------------------------------------
c Notes:
c
c  bksub_mode 
c    = 0 - up to 1 s before beam turn-on and up to 1 sec after beam turn-off
c          with a 0.2 offset
c    = 1 - use between beam blip intervals to form baseline average
c    = 2 - use all mse data prior to t=0 only (no post-beam-off data)
c    = 3 - modified option 0 (eliminates junps in averaging interval)
c    = 4 - use (t0, t0 + up to 1 s) and (t1 - up to 1 s, t1) with a 0.2 s 
c          offset
c    = 5 - up to 1 sec after beam turn-off with a 0.2 offset
c    = 6 - up to 1 s before beam turn-on 
c    = 7 - use between beam blip intervals to form baseline average
c          weighted by temporal proximity
c
c-------------------------------------------------------------------------------

      subroutine calc_mse_baseline( mse_npts, mse_time, mse_sindata, 
     $  mse_cosdata, i_bm, bksin, bkcos, bksdsin, bksdcos,
     $  pre_beam_max_time, post_beam_min_time, user_prewidth, 
     $  user_postwidth, user_pre_t1, user_post_t2, preprox, postprox )

      implicit  none
      include   'mse_lib2.inc'

      integer*4 mse_npts, i_bm
      real*4    mse_time(1)
      real*4    mse_sindata(1), bksin, bksdsin
      real*4    mse_cosdata(1), bkcos, bksdcos
      real*4    user_prewidth, user_postwidth, user_pre_t1, user_post_t2
      real*4    preprox, postprox, inv_preprox, inv_postprox
      real*4    pre_beam_max_time, post_beam_min_time

      character*16 empty /''/
      character*16 db /'debug'/
      integer*4 n_samples
      real*4    max_prewidth,  prewidth,  pre_offset /0.2/
      real*4    max_postwidth, postwidth, post_offset /0.2/
      real*4    pre_t1, post_t1
      real*4    pre_t2, post_t2
      real*4    pre_cos, post_cos, pre_cos_sd, post_cos_sd
      real*4    pre_sin, post_sin, pre_sin_sd, post_sin_sd
      real*4    mse_eps

      if ( qlevel .ge. 3 ) print *, '(mse_lib2:calc_mse_baseline)',
     $ ' bksub_mode =', bksub_mode

      bksin = 0.0
      bkcos = 0.0
      bksdsin = 0.0
      bksdcos = 0.0

      mse_eps = (mse_time(mse_npts) - mse_time(mse_npts-1))/10.0
      max_prewidth  = pre_beam_max_time - mse_time(1) - mse_eps
      max_postwidth = mse_time(mse_npts) - post_beam_min_time 
     $                - mse_eps
c      max_prewidth  = pre_beam_max_time - mse_time(1)
c      max_postwidth = mse_time(mse_npts) - post_beam_min_time 

      if ( ( bksub_mode .eq. 0 ) .or. ( bksub_mode .eq. 3 ) .or.
     $     ( bksub_mode .eq. 5 ) .or. ( bksub_mode .eq. 6 ) ) then

        ! Determine pre beam-turn-on averaging interval

        if ( max_prewidth .gt. 1.2 ) then
          pre_offset = 0.2
          prewidth  = 1.0
        else if ( max_prewidth .gt. 0.4 ) then
          pre_offset = 0.2
          prewidth  = max_prewidth - pre_offset
        else if ( max_prewidth .gt. 0.2 ) then
          if ( bksub_mode .ne. 3 )then
            pre_offset = 0.1
          else
            pre_offset = max_prewidth / 2.0
          endif
          prewidth = max_prewidth - pre_offset
        else
          pre_offset = 0.0
          prewidth = 0.0
        endif
        pre_t1 = pre_beam_max_time - prewidth - pre_offset
        pre_t2 = pre_t1 + prewidth

        ! Determine post beam-turn-on averaging interval

        if ( max_postwidth .gt. 1.2 ) then
          post_offset = 0.2
          postwidth  = 1.0
        else if ( max_postwidth .gt. 0.4 ) then
          post_offset = 0.2
          postwidth  = max_postwidth - post_offset
        else if ( max_postwidth .gt. 0.2 ) then
          if ( bksub_mode .ne. 3 ) then
            post_offset = 0.1
          else
            post_offset = max_postwidth / 2.0
          endif
          postwidth = max_postwidth - post_offset
        else
          post_offset = 0.0
          postwidth = 0.0
        endif
        post_t1 = post_beam_min_time + post_offset
        post_t2 = post_t1 + postwidth 

        if ( bksub_mode .eq. 5 ) then
             pre_t1 = 0.0
             pre_t2 = 0.0
             prewidth = 0.0
        elseif ( bksub_mode .eq. 6 ) then
             post_t1 = 0.0
             post_t2 = 0.0
             postwidth = 0.0
        endif

      else if ( bksub_mode .eq. 1 ) then

        pre_t1 = user_pre_t1
        pre_t2 = user_pre_t1 + user_prewidth
        prewidth = user_prewidth

        post_t1 = user_post_t2 - user_postwidth
        post_t2 = user_post_t2
        postwidth = user_postwidth

      else if ( bksub_mode .eq. 7 ) then

        pre_t1 = user_pre_t1
        pre_t2 = user_pre_t1 + user_prewidth
        prewidth = user_prewidth
        inv_preprox = 1.0 / preprox

        post_t1 = user_post_t2 - user_postwidth
        post_t2 = user_post_t2
        postwidth = user_postwidth
        inv_postprox = 1.0 / postprox

      else if ( bksub_mode .eq. 2 ) then

        pre_t1 = mse_time(1)
        pre_t2 = 0.0
        prewidth = pre_t2 - pre_t1

        post_t1 = mse_time(mse_npts)
        post_t2 = mse_time(mse_npts)
        postwidth = 0.0
        

      else if ( bksub_mode .eq. 4 ) then

        pre_t1 = mse_time(1)
        pre_t2 = mse_time(1) +
     $    max( 0.0, min( 1.0, max_prewidth - pre_offset ) )
        prewidth = pre_t2 - pre_t1

        post_t2 = mse_time(mse_npts)
        post_t1 = mse_time(mse_npts) - 
     $    max( 0.0, min( 1.0, max_postwidth - post_offset ) )
        postwidth = post_t2 - post_t1


      endif

      ! Calculate the background for the cosine signal (pre and post).
      ! Pre-beam window
   

      if ( pre_t1 .lt. pre_t2 ) then
        call average_data( mse_time, mse_cosdata, mse_npts, pre_t1,
     $    pre_t2, empty, pre_cos, pre_cos_sd, n_samples )
        call average_data( mse_time, mse_sindata, mse_npts, pre_t1,
     $    pre_t2, empty, pre_sin, pre_sin_sd, n_samples )
      else
          pre_sin = 0.0
          pre_sin_sd = 0.0
          pre_cos = 0.0
          pre_cos_sd = 0.0
      endif

      ! Post-beam window.

      if ( post_t1 .lt. post_t2 ) then
        call average_data( mse_time, mse_cosdata, mse_npts, post_t1,
     $    post_t2, empty, post_cos, post_cos_sd, n_samples )
        call average_data( mse_time, mse_sindata, mse_npts, post_t1,
     $    post_t2, empty, post_sin, post_sin_sd, n_samples )
      else
          post_sin = 0.0
          post_sin_sd = 0.0
          post_cos = 0.0
          post_cos_sd = 0.0
      endif

      ! Calculate the weighted average (weighted by window width).
      if (bksub_mode  .lt. 7) then
          bksin = ( prewidth * pre_sin + postwidth * 
     $             post_sin ) /  ( prewidth + postwidth )
          bkcos = ( prewidth * pre_cos + postwidth * 
     $             post_cos ) / ( prewidth + postwidth )

      ! Calculate the weighted sd (weighted by window width).

          bksdsin = sqrt( ( prewidth * pre_sin_sd**2 + 
     $       postwidth * post_sin_sd**2 ) / ( prewidth + postwidth ) )
          bksdcos = sqrt( ( prewidth * pre_cos_sd**2 + 
     $       postwidth * post_cos_sd**2 ) / ( prewidth + postwidth ) )
      else
      ! Calculate the weighted average (weighted by temporal proximity).
          bksin = ( inv_preprox * pre_sin + inv_postprox * 
     $             post_sin ) /  ( inv_preprox + inv_postprox )
          bkcos = ( inv_preprox * pre_cos + inv_postprox * 
     $             post_cos ) / ( inv_preprox + inv_postprox )

      ! Calculate the weighted sd (weighted by temporal proximity).

          bksdsin = sqrt( ( inv_preprox * pre_sin_sd**2 + 
     $       inv_postprox * post_sin_sd**2 ) / 
     $      ( inv_preprox + inv_postprox ) )
          bksdcos = sqrt( ( inv_preprox * pre_cos_sd**2 + 
     $       inv_postprox * post_cos_sd**2 ) / 
     $      ( inv_preprox + inv_postprox ) )


      endif

      if ( qlevel .ge. 6 ) then
        print *, '            bksub_mode:', bksub_mode
        print *, '  Pre beam on interval: (',  pre_t1, ',',  pre_t2, ')'
        print *, ' Post beam on interval: (', post_t1, ',', post_t2, ')'
        print *, '        pre_sin =', pre_sin,        ',   post_sin =',
     $    post_sin,    ',        bksin =', bksin
        print *, '        pre_cos =', pre_cos,        ',   post_cos =',
     $    post_cos,    ',        bkcos =', bkcos
        print *, '     pre_sin_sd =', pre_sin_sd,     ' post_sin_sd =',
     $    post_sin_sd, ', bksdsin =', bksdsin
        print *, '     pre_cos_sd =', pre_cos_sd ,    ' post_cos_sd =',
     $    post_cos_sd, ', bksdcos =', bksdcos
        print *, '   max_prewidth =',   max_prewidth, ',   prewidth = ', 
     $    prewidth
        print *, '  max_postwidth = ', max_postwidth, ',  postwidth =', 
     $    postwidth
        print *, '  preprox = ', preprox, ',  postprox =', 
     $    postprox
       endif

      return
      end

c-------------------------------------------------------------------------------
c routine: do_setup_and_beams
c author:  mike brown
c descrpt: preform basic initialization task:
c          - get number of channels
c          - get nubmer of beams
c          - read the calibration files
c          - read beam data and determin on/off times
c
c-------------------------------------------------------------------------------
c Inputs:
c
c   shot = shot number
c
c-------------------------------------------------------------------------------
c Output:
c
c   kerror = error code
c
c-------------------------------------------------------------------------------

      subroutine do_setup_and_beams( shot, kerror )

      implicit  none
      include   'mse_lib2.inc'

      integer*4 shot, kerror

      character*10 beam_ttl_names(4) /'NBVAC30LT', 'NBVAC21RT', 
     $                            'NBVAC21LT', 'NBVAC30RT'/
      character*10 beam_v_names(4) /'PCNBV30LT', 'PCNBV21RT', 
     $                            'PCNBV21LT', 'PCNBV30RT'/
      integer*4 i, j, n_pts

      if ( qlevel .ge. 3 ) print *, '(mse_lib2:do_setup_and_beams)'


      ! Get the number of channels and number of beams for this shot

      call get_number_of_beams_and_channels( shot )

      ! Read the mse setup file and get the set-up parameters for the
      ! given shot.  Also read in the calibration file and the positions
      ! file. All setup info is read into common.

      call read_mse_files( shot, kerror )
      if ( kerror .ne. 0 ) then 
        kerror = 2
        return
      endif

      ! All beams off
      beam_data(:,:) = 0.0
      ! Retrieve the relevant beam data

      ! read beam voltage, set on levels to voltage (kV)
        beam_on_level = beam_v_on_level
        if ( qlevel .ge. 1 ) then
          print *, '(mse_lib2:do_setup_and_beams) Beam on volts '
          print *, '          30left   ',beam_v_on_level(1)
          print *, '          210right ',beam_v_on_level(2)
          print *, '          210left  ',beam_v_on_level(3)
          print *, '          30right  ',beam_v_on_level(4)
        endif

      call read_getdat_data( shot, beam_v_names(1), -1.0, 
     $  ptdata_max_pts, beam_time_base(:,1), beam_data(:,1),
     $  n_beam_pts(1), kerror )
      if ( ( kerror .ne. 0 ) .or. ( n_beam_pts(1) .lt. 3 ) ) then
      ! error so try ttl signal
          if ( qlevel .ge. 1 )
     $     print *, '(mse_lib2:do_setup_and_beams) No data for ',
     $       beam_v_names(1)
          call read_getdat_data( shot, beam_ttl_names(1), -1.0,
     $         ptdata_max_pts,beam_time_base(:,1), beam_data(:,1),
     $          n_beam_pts(1), kerror )
           beam_on_level(1) = beam_ttl_on_volts

         if ( ( kerror .ne. 0 ) .or. ( n_beam_pts(1) .lt. 3 ) ) then
           print *, '(mse_lib2:do_setup_and_beams) No data for ',
     $       beam_ttl_names(1)
	   if ( n_beams .gt. 1) then
               ! Let it continue in case there are 210 beams
               beam_data(:,1) = 0.0
           else
               return
      	   endif
         endif
      endif

      if ( n_beams .gt. 1 ) then

        call read_getdat_data( shot, beam_v_names(2), -1.0, 
     $    ptdata_max_pts, beam_time_base(:,2), beam_data(:,2),
     $    n_beam_pts(2), kerror )
        if ( ( kerror .ne. 0 ) .or. ( n_beam_pts(2) .lt. 3 ) ) then
        ! error so try ttl signal
          if ( qlevel .ge. 1 )
     $     print *, '(mse_lib2:do_setup_and_beams) No data for ',
     $       beam_v_names(2)
          call read_getdat_data( shot, beam_ttl_names(2), -1.0,
     $         ptdata_max_pts,beam_time_base(:,2), beam_data(:,2),
     $          n_beam_pts(2), kerror )
           beam_on_level(2) = beam_ttl_on_volts
           if ( ( kerror .ne. 0 ) .or. ( n_beam_pts(2) .lt. 3 ) ) then
             print *, '(mse_lib2:do_setup_and_beams) No data for ', 
     $         beam_ttl_names(2)
	     beam_data(:,2) = 0.0 
           endif
        endif

        call read_getdat_data( shot, beam_v_names(3), -1.0,
     $    ptdata_max_pts, beam_time_base(:,3), beam_data(:,3),
     $    n_beam_pts(3), kerror )
        if ( ( kerror .ne. 0 ) .or. ( n_beam_pts(3) .lt. 3 ) ) then
        ! error so try ttl signal
          if ( qlevel .ge. 1 )
     $     print *, '(mse_lib2:do_setup_and_beams) No data for ',
     $       beam_v_names(3)
          call read_getdat_data( shot, beam_ttl_names(3), -1.0,
     $         ptdata_max_pts,beam_time_base(:,3), beam_data(:,3),
     $          n_beam_pts(3), kerror )
           beam_on_level(3) = beam_ttl_on_volts
           if ( ( kerror .ne. 0 ) .or. ( n_beam_pts(3) .lt. 3 ) ) then
             print *, '(mse_lib2:do_setup_and_beams) No data for ',
     $         beam_ttl_names(3)
             beam_data(:,3) = 0.0
           endif
        endif

        call read_getdat_data( shot, beam_v_names(4), -1.0,
     $    ptdata_max_pts, beam_time_base(:,4), beam_data(:,4),
     $    n_beam_pts(4), kerror )
        if ( ( kerror .ne. 0 ) .or. ( n_beam_pts(4) .lt. 3 ) ) then
        ! error so try ttl signal
          if ( qlevel .ge. 1 )
     $     print *, '(mse_lib2:do_setup_and_beams) No data for ',
     $       beam_v_names(4)
          call read_getdat_data( shot, beam_ttl_names(4), -1.0,
     $         ptdata_max_pts,beam_time_base(:,4), beam_data(:,4),
     $          n_beam_pts(4), kerror )
           beam_on_level(4) = beam_ttl_on_volts
           if ( ( kerror .ne. 0 ) .or. ( n_beam_pts(4) .lt. 3 ) ) then
             print *, '(mse_lib2:do_setup_and_beams) No data for ',
     $         beam_ttl_names(4)
             beam_data(:,4) = 0.0
           endif
        endif

      endif

      if ( qlevel .ge. 36 ) then
        print *, '    30LT Total on time:', 
     $    sum( beam_data(1:n_beam_pts(1),1) ) * 0.0005 / 5.12
        print *, '   210RT Total on time:', 
     $    sum( beam_data(1:n_beam_pts(2),2) ) * 0.0005 / 5.12
        print *, '   210LT Total on time:', 
     $    sum( beam_data(1:n_beam_pts(3),3) ) * 0.0005 / 5.12
        do j = 1, 3
          print *, beam_v_names(j),beam_ttl_names(j)
          print *, '  Number of points:', n_beam_pts(j)
          print *, '  Sample beam data:', 
     $      ( beam_data(i,j), i = 1933, 1953 )
        enddo
        print *,   'Beam time base:', 
     $    ( beam_time_base(i,1), i = 1933, 1953 )
      endif

      return
      end

c-------------------------------------------------------------------------------
c function: element
c author:  michael d. brown, llnl
c descrpt: given a char string, parse and return the next space or
c          tab separated substring.  the input string is updated with the
c          substring removed. if there is nothing more to parse, ' ' is
c          returned. this function uses function trimlen.
c changes:
c          01/27/92  mdb  original version.
c-------------------------------------------------------------------------------

      character*(*) function element(line)

      implicit  none
      character line*(*)  ! updated: input line to be parsed.

      integer*4 space /32/, tab /9/
      integer*4 trimlen, slen, c1, c2, i

      slen = trimlen(line)
      i = 1
      c1 = 0
      c2 = 0
      do while ( ( c1 .eq. 0 .or. c2 .eq. 0 ) .and. i .le. slen )
        if ( c1 .gt. 0. and. ( ichar( line(i:i) ) .eq. space .or.
     $    ichar( line(i:i) ) .eq. tab ) ) then
          c2 = i
        else if ( c1 .eq. 0 .and. ichar( line(i:i) ) .ne. space
     $    .and. ichar( line(i:i) ) .ne. tab ) then
          c1 = i
        endif
        i = i + 1
      enddo
      if ( c1 .gt. 0 .and. c2 .eq. 0 ) c2 = slen  ! last element on line

      if ( c1 .eq. 0 .and. c2 .eq. 0 ) then
         element = ' '                            ! nothing to parse
      else
        element = line(c1:c2)               ! get element and update input line
        if ( c2 .lt. slen ) then
          line = line(c2+1:slen)
        else
          line = ' '
        endif
      endif

      return
      end

c-------------------------------------------------------------------------------
c routine: tangent_slope
c author:  mike makowski
c descrpt: calculates the pitch angle based on the tangent slope fitting 
c          function
c changes:
c          10/11/05 mam wrote routine
c
c-------------------------------------------------------------------------------
c Inputs:
c
c      chan = channel number of supplied data
c   mse_sin = value of the sin data for the channel
c   mse_cos = value of the cos data for the channel
c        bt = value of B_toroidal
c
c-------------------------------------------------------------------------------
c Output:
c
c   gamma = pitch angle (degrees)
c
c-------------------------------------------------------------------------------

      subroutine tangent_slope( chan, mse_sin, mse_cos, bt, gamma )

      implicit  none
      include   'mse_lib2.inc'

      integer*4 chan
      logical*1 dump_flag
      real*4    mse_sin, mse_cos, bt, gamma
      real*4    offset

      offset = mcal_btscale(chan) * bt + mcal_btoffset(chan)
      if (mse_sin .eq. 0.0 .and. 
     *   (mse_cos * mcal_gain(chan)) .eq. 0.0) then
        gamma = offset / mcal_slope(chan)
      else 
      gamma = ( 0.5 * atan2(mse_sin,mse_cos*mcal_gain(chan))*r2d
     $  - offset ) / mcal_slope(chan)
      endif

      return
      end
      
      subroutine tangent_slope_sig( chan, mse_sin, mse_sin_sig,
     $      mse_cos, mse_cos_sig,mse_cov,bt, sgamma )
      implicit  none
      include   'mse_lib2.inc'

      integer*4 chan
      logical*1 dump_flag
      real*4    mse_sin, mse_cos, bt, sgamma
      real*4    mse_sin_sig, mse_cos_sig
      real*4    mse_cov
      real*4    offset
      real*4    sgamma2
      real*4    covgam

      offset = mcal_btscale(chan) * bt + mcal_btoffset(chan)
      sgamma2 = (mse_sin_sig**2 * mse_cos**2 * mcal_gain(chan)**2 +
     $           mse_cos_sig**2 * mse_sin**2 * mcal_gain(chan)**2) / (
     $  4.0 * mcal_slope(chan)**2 * (mse_cos**2 * mcal_gain(chan)**2 +
     $  mse_sin**2)**2)

      covgam = (-2.0 * mse_cov * mse_cos * mse_sin * 
     $      mcal_gain(chan)**2) / (
     $  4.0 * mcal_slope(chan)**2 * (mse_cos**2 * mcal_gain(chan)**2 +
     $  mse_sin**2)**2)
     $  

      sgamma2 = sgamma2 + covgam

      if(sgamma2 .gt. 0.0) then
	  sgamma = sqrt(sgamma2) * r2d
      else 
	  sgamma = mse_badchannel_std
      endif

      return
      end




c-------------------------------------------------------------------------------
c routine: tangent_resid
c author:  bill meyer
c descrpt: calculates the tangent correction base on 4th order polynomial
c          fit to calibration curve residual.
c changes:
c          03/31/2011 first
c
c-------------------------------------------------------------------------------
c Inputs:
c
c      chan = channel number of supplied data
c   gamma = pitch angle (degrees)
c
c-------------------------------------------------------------------------------
c Output:
c
c   gamma = pitch angle corrected(degrees)
c
c-------------------------------------------------------------------------------

      subroutine tangent_resid( chan, gamma )

      implicit  none
      include   'mse_lib2.inc'

      integer*4 chan
      real*4 gamma, gamma_res
      real*4 c0,c1,c2,c3,c4

      c0 = mcal4_res(chan,1)
      c1 = mcal4_res(chan,2)
      c2 = mcal4_res(chan,3)
      c3 = mcal4_res(chan,4)
      c4 = mcal4_res(chan,5)



      gamma_res = c0+(c1*gamma)+(c2*(gamma**2))+
     .               (c3*(gamma**3))+(c4*(gamma**4))
      gamma = gamma - gamma_res

      return
      end
c-------------------------------------------------------------------------------
c routine: tangent_offset
c author:  mike makowski
c descrpt: calculates the pitch angle based on the tangent offset fitting 
c          function  (Moller's fitting function)
c changes:
c          10/11/05 mam wrote routine
c
c-------------------------------------------------------------------------------
c Inputs:
c
c      chan = channel number of supplied data
c   mse_sin = value of the sin data for the channel
c   mse_cos = value of the cos data for the channel
c        bt = value of B_toroidal
c
c-------------------------------------------------------------------------------
c Output:
c
c   gamma = pitch angle (degrees)
c
c-------------------------------------------------------------------------------

      subroutine tangent_offset( chan, mse_sin, mse_cos, bt, gamma )

      implicit  none
      include   'mse_lib2.inc'

      integer*4 chan
      real*4    mse_sin, mse_cos, bt, gamma, offset,ta,b
      real*4 gamma1, gamma2
      real*4 x,y


c
c setup to use atan2 to avoid mse_cos near zero
c
      y = (mse_sin - (mse_cos*mcal3_dc_offset(chan)))
      x = (mse_cos * mcal3_gain(chan))

      if(x .lt. 0.0) then
         y = -y
         x = -x
       endif

      offset = mcal3_phase(chan) + mcal3_btscale(chan) * bt
c     gamma = 0.5 * atan((mse_sin / mse_cos - mcal3_dc_offset(chan))
c    $  / mcal3_gain(chan) ) * r2d - offset
      gamma = 0.5 * atan2(y,x) * r2d - offset



      return
      end

      subroutine tangent_offset_sig( chan, mse_sin, mse_sin_sig,
     $      mse_cos, mse_cos_sig,mse_cov,bt, sgamma )
      implicit  none
      include   'mse_lib2.inc'

      integer*4 chan
      logical*1 dump_flag
      real*4    mse_sin, mse_cos, bt, sgamma
      real*4    mse_sin_sig, mse_cos_sig
      real*4    mse_cov
      real*4    offset
      real*4    sgamma2
      real*4    covgam


      offset = mcal3_phase(chan) + mcal3_btscale(chan) * bt
      sgamma2 = (mse_sin_sig**2 * mse_cos**2 * mcal3_gain(chan)**2 +
     $           mse_cos_sig**2 * mse_sin**2 * mcal3_gain(chan)**2) / (
     $  4.0 * (mse_cos**2 * mcal3_gain(chan)**2 + (mse_sin - mse_cos *
     $  mcal3_dc_offset(chan))**2)**2)

      covgam = (-2.0 * mse_cov * mse_cos * mse_sin * 
     $      mcal3_gain(chan)**2) / (
     $  4.0 * (mse_cos**2 * mcal3_gain(chan)**2 + (mse_sin - mse_cos *
     $  mcal3_dc_offset(chan))**2)**2)

      sgamma2 = sgamma2 + covgam

      if(sgamma2 .gt. 0.0)  then
	 sgamma = sqrt(sgamma2) * r2d
      else 
	 sgamma = mse_badchannel_std
      endif

      return
      end

c-------------------------------------------------------------------------------
c routine: preprocess_mse_data
c descrpt: read in mse data for this shot/chan, calcaluate the average 
c          background of the sin/cos signal, then extract and return only 
c          those points that occur when the beam was ON.
C
C changes: With multiple beam pulses, calculate new backgrounds to use
C          between pulses.
c
c-------------------------------------------------------------------------------
c Inputs:
c
c    shot = shot number
c   chnum = channel number
c
c-------------------------------------------------------------------------------
c Output:
c
c   mse_sindata = mse sin data array
c   mse_cosdata = mse cos data array
c      mse_time = time base for mse waveforms
c      mse_npts = number of valid points in return arrays
c        on_beg = array containing the first times that a beam blip in fully on
c        on_end = array containing the last times that a beam blip is fully on
c       off_beg = array containing the first times of a beam off interval
c                 may be used for baseline
c       off_end = array containing the last times of a beam off interval
c                 may be used for baseline
c      unus_beg = array containing the first times of a beam off interval
c                 30r may be on or off
c      unus_end = array containing the last times of a beam off interval
c                 30r may be on or off
c       npulses = number of beam blips in the on_beg and on_end arrays. The
c                 dimension of off_beg, and off_end is npulse + 1
c   bksub_error = 0 for successful termination
c               = 1 for invalid bksub_mode
c
c-------------------------------------------------------------------------------
c Notes:
c
c  bksub_mode 
c    = 0 - up to 1 s before beam turn-on and up to 1 sec after beam turn-off
c    = 1 - use between beam blip intervals to form baseline average
c    = 2 - use first 50 ms of data record only (no post-beam-off data)
c    = 3 - modified option 0 (eliminates junps in averaging interval)
c    = 4 - use (t0, t0 + up to 1 s) and (t1 - up to 1 s, t1)
c    = 5 - up to 1 sec after beam turn-off
c    = 6 - up to 1 s before beam turn-on 
c    = 7 - use between beam blip intervals to form baseline average
c          weighted by temporal proximity
c
c-------------------------------------------------------------------------------

      subroutine preprocess_mse_data( shot, chnum, mse_sindata, 
     $  mse_cosdata, mse_time, mse_npts, on_beg, on_end, off_beg, 
     $  off_end, unus_beg, unus_end,  npulses, bksub_error )

      implicit  none
      include   'mse_lib2.inc'

      integer*4 shot, chnum, mse_npts, npulses, bksub_error
      real*4    mse_sindata(ptdata_max_pts), mse_cosdata(ptdata_max_pts)
      real*4    mse_time(ptdata_max_pts)
      real*4    on_beg(max_pulses), on_end(max_pulses)
      real*4    off_beg(max_pulses+1), off_end(max_pulses+1)
      real*4    unus_beg(max_pulses+1), unus_end(max_pulses+1)

      integer*4 get_beam_index, i, i_bm, np, p, q, s, p1
      integer*4 i1, i2, i_mse_beg, i_mse_end
      integer*1 counter(ptdata_max_pts)
      real*4    bksin, bkcos, bksdsin, bksdcos
      real*4    lbksin, lbkcos, lbksdsin, lbksdcos
      real*4    prewidth, postwidth, preprox, postprox
      real*4    pre_pulse_off_width, post_pulse_off_width
      integer*4 ep /0/

      ep = 1

      entry preprocess_mse_data2( shot, chnum, mse_sindata, 
     $  mse_cosdata, mse_time, mse_npts, on_beg, on_end, off_beg, 
     $  off_end, unus_beg, unus_end, npulses, bksub_error,
     $  lbksin,lbkcos,lbksdsin,lbksdcos )

      if(ep .ne. 1) then
	 ep = 2
      endif

      if ( qlevel .ge. 3 ) then
         if (ep .eq. 1 ) print *, '(mse_lib2:preprocess_mse_data)', 
     $  ' Channel = ', chnum
         if (ep .eq. 2 ) print *, '(mse_lib2:preprocess_mse_data2)', 
     $  ' Channel = ', chnum
      endif


      ! Get the beam index for the input channel

      i_bm = get_beam_index( chnum )

      if ( bksub_mode .ne. 1 .and. bksub_mode .ne. 7) then

        ! For all but mode 1, we are averaging up to 1 s of data before the
        ! beam's first pulse and 1 s of data after the beams turn off. See
        ! documentation for calc_mse_baseline for exact details of the
        ! averaging intervals used

	if ( first_on_diagbeam .lt. last_on_diagbeam) then
           call calc_mse_baseline( mse_npts, mse_time, mse_sindata,
     $       mse_cosdata, i_bm, bksin, bkcos, bksdsin, bksdcos, 
     $       first_on_diagbeam-diagbeam_inc,
     $       last_on_diagbeam+diagbeam_inc,0.0,0.0,0.0,0.0,0.0,0.0 )
	else if ( qlevel .ge. 6 ) then
	     print *, 'No usable diagnostic beam on times.'
	endif

        ! Subtract the background.
         
        mse_sindata = mse_sindata - bksin
        mse_cosdata = mse_cosdata - bkcos

      else if ( bksub_mode .eq. 1 .or. bksub_mode .eq. 7) then
         
        ! Alternatively, use the pre- and post-beam-off intervals of EACH PULSE
        ! INDIVIDUALLY to compute a background which is then subtracted from the
        ! captured pulse of interest. The pre/post pulse-off times must be at 
        ! least multi_pulse_bksub_min_width seconds in length.

        p = 1
        q = 1
        s = 1
        i_mse_beg = 1
        i_mse_end = 1 
        do while ( p .le. npulses )

          ! Only consider intervals for which the off-time is greater than the
          ! minimum.
          q = p
          s = p

          do while( (((off_end(q) - off_beg(q)) .lt. 
     $           multi_pulse_bksub_min_width) .or.
     $           (off_end(q) .gt. on_beg(p))) .and. 
     $          (q .gt. 1))
            q = q-1
          enddo
          do while( (((off_end(s) - off_beg(s)) .lt. 
     $           multi_pulse_bksub_min_width) .or.
     $           (off_beg(s) .lt. on_end(p))) .and. 
     $          (s .le. npulses))
            s = s+1
          enddo
          post_pulse_off_width = 
     $      min( off_end(s) - off_beg(s),
     $      multi_pulse_bksub_max_width )

          pre_pulse_off_width = 
     $      min( off_end(q) - off_beg(q),
     $      multi_pulse_bksub_max_width )
          prewidth = pre_pulse_off_width
          postwidth = post_pulse_off_width
          preprox = on_beg(p) - off_end(q)
          postprox = off_beg(s) - on_end(p)

          if ( ( post_pulse_off_width .ge. multi_pulse_bksub_min_width ) 
     $      .and. ( pre_pulse_off_width .ge. multi_pulse_bksub_min_width 
     $      ) ) then

            do while( ( mse_time(i_mse_beg) .lt. off_beg(q))
     $               .and. ( i_mse_beg .le. mse_npts ) )
              i_mse_beg = i_mse_beg + 1
            enddo
            i_mse_end = i_mse_beg + 1

            do while ( ( mse_time(i_mse_end) .lt. off_end(s))
     $                    .and. ( i_mse_end .le. mse_npts ) )
              i_mse_end = i_mse_end + 1
            enddo

            call calc_mse_baseline( mse_npts, mse_time, mse_sindata,
     $        mse_cosdata, i_bm, bksin, bkcos, bksdsin, bksdcos, 
     $        multi_pulse_bksub_max_width,multi_pulse_bksub_min_width,
     $        pre_pulse_off_width, post_pulse_off_width,
     $        mse_time(i_mse_beg), mse_time(i_mse_end),preprox,postprox)
                  
            ! Determine the indices corresponding the interval that the
            ! baseline subtraction is valid for

            i1=1
            i2=1
            do while( ( mse_time(i1) .lt. on_beg(p))
     $               .and. ( i1 .lt. mse_npts ) )
              i1 = i1 + 1
            enddo
            do while( ( mse_time(i2) .lt. on_end(p))
     $               .and. ( i2 .lt. mse_npts ) )
              i2 = i2 + 1
            enddo
            i1 = i1 + 1
            i2 = i2 + 1
            mse_sindata(i1:i2) = mse_sindata(i1:i2) - bksin
            mse_cosdata(i1:i2) = mse_cosdata(i1:i2) - bkcos
                  
            if ( qlevel .gt. 5 ) print '(1x,i4,2i6,4f7.4,2f8.4)', 
     $        p, i_mse_beg, i_mse_end, 
     $        off_beg(p),
     $        off_end(p), off_beg(p+1),
     $        off_end(p+1), bksin, bkcos

          else
            if ( qlevel .gt. 5 ) then
              print *, ' Pre- or Post-width too small'
              print *, ' Prewidth = ', prewidth, '    Postwidth = ', 
     $        postwidth
              print *, ' No background subtraction for pulse: ', p
              print *,off_beg(q),off_end(q)
              print *,off_beg(s),off_end(s)
            endif
          endif

          p = p + 1
c         i_mse_beg = i_mse_end + 1

        enddo

      else

        print *,'ERROR: Illegal bksub_mode(', bksub_mode ,') option'
        bksub_error = 1
        ep = 0
        return

      endif
      
      bksub_error = 0

      if(ep .eq. 2) then
	 lbksin = bksin
	 lbkcos = bkcos
	 lbksdsin = bksdsin
	 lbksdcos = bksdcos
      endif
      ep = 0

      return
      end

c-------------------------------------------------------------------------------
c routine: read_phy_data
c author:  michael d. brown
c desc:    reads in the given shot/pointname starting at the given time t1
c          and returns the time array (seconds) and the data array (physical
c          units).  the max # of points that this routine will read is 81920.
c          this routine was written to handle mse related data.
c notes:   
c          digitizer rate should be smaller then 10 khz (mdb 1/21/92).
c changes:
c          01/27/92  mdb  original version.
c
c-------------------------------------------------------------------------------
c Inputs:
c
c        shot = shot number
c   pointname = point name of data
c          t1 = starting time to read the data
c
c-------------------------------------------------------------------------------
c Output:
c
c         tdata = time base of data (seconds)
c         rdata = data (dim = mxpts)
c            np = number of points in returned array
c    ptdata_err = error code from ptdata
c
c-------------------------------------------------------------------------------

      subroutine read_phy_data( shot, pointname, t1, maxpts, tdata,
     $  rdata, np, ptdata_err )

      implicit  none
      include   'mse_lib2.inc'

      character*10 pointname
      integer*4 shot, maxpts, np, ptdata_err
      real*4 t1, tdata(1), rdata(1)

      ! Variables to read data:
      
      character*10 upointname
      character*4  source /'.pla'/
      integer*4 type /11/, ascii /0/, int16 /0/, int32 /0/
      integer*4 i, ier, iarray(128), rawdata(ptdata_max_pts)
      integer*4 lshot
      integer*2 ipointname(5), isource(2)
      real*4    real32 /0.0/, rarray(ptdata_max_pts+20)

      equivalence (source, isource)
      equivalence (ipointname, upointname)
c      external sigsetmask

      if ( qlevel .ge. 3 ) print *, '(mse_lib2:read_phy_data) ', 
     $  pointname

      lshot = shot

      ! Parameters

      upointname = pointname
      rarray(1) = t1                      ! start time in seconds.
      rarray(2) = 0.0001                  ! digitizer delta time in seconds.
      iarray(1) = min0(maxpts,ptdata_max_pts)         ! max points

      ! Get the data


      call ptdata( type, lshot, isource, ipointname, rawdata, ier,
     $  iarray, rarray, ascii, int16, int32, real32 )
     

      ptdata_err = ier

      if ( ptdata_err .ne. 0 .and. ptdata_err .ne. 4 .and. 

     $  ptdata_err .ne. 2 ) then
        if ( qlevel .ge. 3 ) print *, '(mse_lib2:read_phy_data) ',
     $    ' Shot = ',lshot, ', pointname = ', pointname, 
     $    ' -- Data not found: error = ', ptdata_err

      else 

        ptdata_err = 0
        np = iarray(2)

        do i = 1, np  ! Convert to volts, include offset
          rdata(i) = - ( rawdata(i) - rarray(6) ) * rarray(5) * 
     $      rarray(4)
          tdata(i) = rarray(i+20)
        enddo

      endif

      return
      end

c-------------------------------------------------------------------------------
c routine: read_getdat_data
c author:  michael d. brown, b. meyer
c desc:    based on read_phy_data but uses getalldat_camac instead of ptdata 
c          for reading data . this allows maintanance of tracking dfi changes
c          to be done by getalldat_camac instead of in this code.
c          reads in the given shot/pointname starting at the given time t1
c          and returns the time array (seconds) and the data array (physical
c          units).  the max # of points that this routine will read is 81920.
c          this routine was written to handle mse related data.
c notes:   
c          digitizer rate should be smaller then 10 khz (mdb 1/21/92).
c changes:
c          01/27/92  mdb  original version.
c
c-------------------------------------------------------------------------------
c Inputs:
c
c        shot = shot number
c   pointname = point name of data
c          t1 = starting time to read the data
c      maxpts = maximum number of points to retrieve
c
c-------------------------------------------------------------------------------
c Output:
c
c         tdata = time base of data (seconds)
c         rdata = data (dim = mxpts)
c            np = number of points in returned array
c    ptdata_err = error code from ptdata
c
c-------------------------------------------------------------------------------

      subroutine read_getdat_data( shot, pointname, t1, maxpts, tdata,
     $  rdata, np, ptdata_err )

      implicit  none
      include   'mse_lib2.inc'

      character*(*) pointname
      integer*4 shot, maxpts, np, ptdata_err 
      real*4 t1, tn, tdata(maxpts), rdata(maxpts)

      ! Variables to read data:
      
      character*12 upointname
      character*28 label
      character*15 ulabel
      integer*4 type /11/, ascii /0/, int16 /0/, int32 /0/
      integer*4 i, ier, iarray(128), rawdata(ptdata_max_pts)
      integer*4 lshot, ical
      integer*2 ipointname(6), isource(2), dpointname(6)
      real*4    real32 /0.0/, rarray(ptdata_max_pts+20)
      integer ibranch,icrate,islot
      real*4 tmin,tmax
      integer*4 ep
      data ep/0/

      equivalence (ipointname, upointname)
c      external sigsetmask

      ep = 1

      entry read_getdat_data_i( shot, dpointname, t1, maxpts, tdata,
     $  rdata, np, ptdata_err )

      lshot = shot

      ! Parameters

      if ( ep .eq. 1) then 
             upointname = pointname
      else
             ipointname = dpointname
      endif
      if ( qlevel .ge. 3 ) print *, '(mse_lib2:read_getdat_data) ', 
     $  upointname
      call flush(6)
      iarray(1) = min0(maxpts,ptdata_max_pts)
      ical = 1
      ier = 0

      ! Get the data


      !tmin = -0.05
      tmin = t1
      tmax = 25.0
      call getdat( lshot, upointname, ical,ier, 
     $  tdata, rdata, iarray(1), tmin, tmax, 0,1.0,0)

      np = iarray(1)
      ptdata_err = ier
      if ( ier .lt. 0 ) ptdata_err = 0

      if ( ( ptdata_err .ne. 0 ) .and. ( ptdata_err .ne. 4 ) .and. 
     $  ( ptdata_err .ne. 2 ) ) then
        if ( qlevel .ge. 3 ) print *, '(mse_lib2:read_getdat_data) ',
     $    ' Shot = ',lshot, ', pointname = ', pointname, 
     $    ' -- Data not found: error = ', ptdata_err
      else
        ptdata_err = 0
      endif

      if ( qlevel .ge. 6 ) then
        print *, '   Number of points:', np
        print *, '              Error:', ptdata_err
        print *, '               ical:', ical
      endif

      ep = 0
      return
      end

c-------------------------------------------------------------------------------
c routine: read_mse_data
c author:  michael d. brown, llnl
c desc:    reads in mse data for the given channel, and returns the
c          data in the sin and cos arrays, dependent on mode.
c          if a shot number <= 74150 is given, the old channels names of
c          mse1 and mse2 will be use as a and b.
c changes:
c          01/27/92  mdb  original version.
c
c-------------------------------------------------------------------------------
c Inputs:
c
c     shot = shot number
c    chnum = channel number
c       t1 = initial time (seconds)
c
c-------------------------------------------------------------------------------
c Outputs: 
c
c      mse_time = time base of the mse signal that is read
c   mse_sindata = mse sin data 
c   mse_cosdata = mse cos data
c      mse_npts = number of points in the mse data arrays
c          ierr = ptdata error code, 0 = ok
c
c-------------------------------------------------------------------------------

      subroutine read_mse_data( shot, chnum, t1, mse_time, mse_sindata,
     $  mse_cosdata, mse_npts, ierr )

      implicit  none
      include   'mse_lib2.inc'

      integer*4 shot, chnum, mse_npts, ierr
      real*4    t1, mse_time(ptdata_max_pts) 
      real*4    mse_sindata(ptdata_max_pts), mse_cosdata(ptdata_max_pts)

      character*10 sinname, cosname
      character*6  ch

      integer*4 trimlen, lch, l
      real*4    tstart, tend, dt_ave

      if ( qlevel .ge. 3 ) print *, '(mse_lib2:read_mse_data)'

      lch = chnum

      if ( lch .ge. 10 ) then
        write( ch, '(i2)' ) lch
      else
        write( ch, '(i1)' ) lch
      endif

      l = trimlen(ch)
      write( sinname, '(''MS'',A,''A'')' ) ch(1:l) ! a=sin, b=cos
      write( cosname, '(''MS'',A,''B'')' ) ch(1:l)

      ! Read in the sine data
      ! To read synthetic test data change this to read_syn_data

      call read_getdat_data( shot, sinname, t1, ptdata_max_pts,
     $  mse_time,mse_sindata, mse_npts, ierr )
      if ( ierr .ne. 0 ) return

      tstart = mse_time(1)
      tend = mse_time(mse_npts)

      ! Read in the cosine data
      ! To read synthetic test data change this to read_syn_data

      call read_getdat_data( shot, cosname, t1, ptdata_max_pts,
     $  mse_time,mse_cosdata, mse_npts, ierr )
      if ( ierr .ne. 0 ) return

      if ( mse_time(1) .ne. tstart .or. mse_time(mse_npts) .ne. tend ) 
     $  then
        if ( qlevel .ge. 3 )
     $    print *, ' ERROR: cos and sin waveforms have different '
     $      // 'timebases.'
      endif

      ! Need to shift the time axis by 50 ms in order to properly align data
      ! with time base

      if ( qlevel .ge. 6 ) then
        dt_ave = ( mse_time(mse_npts) - mse_time(1) ) / 
     $    float( mse_npts - 1 )
        print *, '  t0, dt, n_pts, ierr for channel,', chnum, 'are:', 
     $    mse_time(1), dt_ave, mse_npts, ierr,mse_time(mse_npts)
      endif

      return
      end

c-------------------------------------------------------------------------------
c routine: read_msetup
c author:  michael d. brown, llnl
c desc:    scan the setup file for the given shot range and read the setup
c          file data into the common block described in mse_lib2.inc
c changes:
c
c   11/16/05 mam eliminated reading of sightline, lrmode, and viewport from
c            setup file. transitioned to msetup2.dat
c
c-------------------------------------------------------------------------------
c Inputs:
c
c            filename = name of mse setup file containing the shot look-up table
c             shotnum = shot number
c    mcalgain_filenum = index of the mcalgain file from msetup2.dat
c         mrz_filenum = index of the mrz file from msetup2.dat
c   mcalgain3_filenum = index of the mcalgain3 file from msetup2.dat
c                       also index of the mcalgain4 file
c
c-------------------------------------------------------------------------------
c Output:
c
c   ierr = error code 
c        = 0: ok
c        = 1: couldn't open setup file
c        = 2: bad shot range line
c        = 3: reach eof before finding shot in range
c        = 4: bad ab mode
c        = 5: bad lr mode
c        = 6: bad sightline
c   Initializes a large number of common block variables
c
c-------------------------------------------------------------------------------

      subroutine read_msetup( filename, shotnum, mcalgain_filenum, 
     $  mrz_filenum, mcalgain3_filenum, ierr )

      implicit  none
      include  'mse_lib2.inc'

      character*(*) filename
      integer*4 shotnum, ierr 

      character line*96, tmp*96, element*96
      integer*4 mcalgain_filenum, mcalgain3_filenum, mrz_filenum
      integer*4 shot1, shot2, trimlen, unum /14/
      logical*1 done

      if ( qlevel .ge. 1 ) print *, '(mse_lib2:read_msetup)',filename

      ! Open mse log information file

      call open_for_read( unum, filename, ierr )

      if ( ierr .ne. 0 ) then
        if ( qlevel .eq. 3 ) print *, 'Setup file ' // 
     $    filename(1:trimlen(filename)) // ' not found.'
        ierr = 1
        return
      endif

      ! Search file for the lines containing shot intervals and check whether
      ! the supplied shot number falls in the interval

      done = .false.
      ierr = 0
      do while ( .not. done )

        ! Find next blank line or comment

 20     read( unum, '(a)', end = 902 ) line
        if ( line .ne. ' ' .and. line(1:1) .ne. '!' ) goto 20

        ! Read a line -- looking for the shot number range, calib # and comment.

 30     read( unum, '(a)', end = 902 ) line
        if ( line .eq. ' ' .or. line(1:1) .eq. '!' ) goto 30

        tmp = element(line)
        read( tmp, * , err = 901 ) shot1

        tmp = element(line)
        read( tmp, * , err = 901 ) shot2

        if ( qlevel .ge. 6 ) print *, 'shot1 = ', shot1, '   shot2 = ', 
     $    shot2

        ! Are we in the desired shot range?

        if ( shotnum .ge. shot1 .and. shotnum .le. shot2 ) done =.true.

      enddo

      ! Retrieve the file numbers for the various calibration files

      tmp = element(line)
      read( tmp, *, err = 901 ) mcalgain_filenum

      tmp = element(line)
      read( tmp, *, err = 901 ) mrz_filenum

      tmp = element(line)
      read( tmp, *, err = 901 ) mcalgain3_filenum

      tmp = element(line)
      read( tmp, *, err = 987 ) spave_dir_name

      msetup_comment = line

      goto 999

      ! Error exits

 901  continue
      if ( qlevel .eq. 3 ) print *, '  Bad shot range line.'
      ierr = 2
      goto 999

 902  continue
      if ( qlevel .eq. 3 ) print *, '  EOF reached before shot found.'
      ierr = 3
      goto 999

 987  continue
      if ( qlevel .eq. 3 ) print *, '  Unable to parse spave_dir_name.'
      ierr = 3

 999  continue
      close(unum)

      return
      end

c-------------------------------------------------------------------------------
c routine: read_msetup5
c author:  michael d. brown, llnl
c          bill meyer, llnl cloned to make a msetup5 file with 
c             columns cleaned up
c
c desc:    scan the setup file for the given shot range and read the setup
c          file data into the common block described in mse_lib2.inc
c changes:
c
c   09/11/20 wm creation
c
c-------------------------------------------------------------------------------
c Inputs:
c
c            filename = name of mse setup file containing the shot look-up table
c             shotnum = shot number
c         mrz_filenum = index of the mrz file from msetup2.dat
c   mcalgain5_filenum = index of the mcalgain5 file from msetup5.dat
c
c-------------------------------------------------------------------------------
c Output:
c
c   ierr = error code 
c        = 0: ok
c        = 1: couldn't open setup file
c        = 2: bad shot range line
c        = 3: reach eof before finding shot in range
c        = 4: bad ab mode
c        = 5: bad lr mode
c        = 6: bad sightline
c   Initializes a large number of common block variables
c
c-------------------------------------------------------------------------------

      subroutine read_msetup5( filename, shotnum, mrz_filenum,
     $  mcalgain5_filenum, ierr )

      implicit  none
      include  'mse_lib2.inc'

      character*(*) filename
      integer*4 shotnum, ierr 

      character line*96, tmp*96, element*96
      integer*4 mcalgain5_filenum
      integer*4 mcalgain5_normnum, mcalgain5_revnum, mrz_filenum
      integer*4 shot1, shot2, trimlen, unum /14/
      logical*1 done

      if ( qlevel .ge. 1 ) print *, '(mse_lib2:read_msetup5)',filename

      ! Open mse log information file

      call open_for_read( unum, filename, ierr )

      if ( ierr .ne. 0 ) then
        if ( qlevel .eq. 3 ) print *, 'Setup file ' // 
     $    filename(1:trimlen(filename)) // ' not found.'
        ierr = 1
        return
      endif

      ! Search file for the lines containing shot intervals and check whether
      ! the supplied shot number falls in the interval

      done = .false.
      ierr = 0
      do while ( .not. done )

        ! Find next blank line or comment

 20     read( unum, '(a)', end = 902 ) line
        if ( line .ne. ' ' .and. line(1:1) .ne. '!' ) goto 20

        ! Read a line -- looking for the shot number range, calib # and comment.

 30     read( unum, '(a)', end = 902 ) line
        if ( line .eq. ' ' .or. line(1:1) .eq. '!' ) goto 30

        tmp = element(line)
        read( tmp, * , err = 901 ) shot1

        tmp = element(line)
        read( tmp, * , err = 901 ) shot2

        if ( qlevel .ge. 6 ) print *, 'shot1 = ', shot1, '   shot2 = ', 
     $    shot2

        ! Are we in the desired shot range?

        if ( shotnum .ge. shot1 .and. shotnum .le. shot2 ) done =.true.

      enddo

      ! Retrieve the file numbers for the various calibration files

      tmp = element(line)
      read( tmp, *, err = 901 ) mrz_filenum

      tmp = element(line)
      read( tmp, *, err = 901 ) mcalgain5_normnum

      tmp = element(line)
      read( tmp, *, err = 901 ) mcalgain5_revnum

      msetup_comment = line

      if(bt_direction .eq. normBt) then
        mcalgain5_filenum = mcalgain5_normnum
      else
        mcalgain5_filenum = mcalgain5_revnum
      endif

      goto 999

      ! Error exits

 901  continue
      if ( qlevel .eq. 3 ) print *, '  Bad shot range line.'
      ierr = 2
      goto 999

 902  continue
      if ( qlevel .eq. 3 ) print *, '  EOF reached before shot found.'
      ierr = 3
      goto 999

 999  continue
      if ( qlevel .eq. 3 ) then
          print *, '  Normal Bt filnum:',mcalgain5_normnum
          print *, ' Reverse Bt filnum:',mcalgain5_revnum
      endif
      close(unum)

      return
      end

c-------------------------------------------------------------------------------
c routine: read_mcalgain
c author:  michael d. brown, llnl
c descrpt: given a mcalgain file number, read the calibration
c          file data into the common block described in mse_lib2.inc
c changes:
c          01/27/92  mdb  original version.
c          03/23/94  mdb  Modified for 16-channel mse (shots > 80539)
c
c-------------------------------------------------------------------------------
c Inputs:
c
c   filename = calibration file name
c
c-------------------------------------------------------------------------------
c Output:
c
c   ierr = error codes relating to read of calibration file
c        = 0: ok
c        = 1: couldn't open setup file.
c        = 2: illegal data format.
c        = 3: reached eof before all data was found.
c
c-------------------------------------------------------------------------------

      subroutine read_mcalgain( filename, ierr )

      implicit  none
      include   'mse_lib2.inc'

      character*(*) filename
      integer*4 ierr

      character line*80
      integer*4 i, i1, i2, j, unum /45/, trimlen, perline /8/
      real*4    gain(max_mse_chans)
      real*4    slope(max_mse_chans)
      real*4    scale(max_mse_chans)
      real*4    offset(max_mse_chans)

      if ( qlevel .ge. 1 ) print *, '(mse_lib2:read_mcalgain) ', 
     $  filename

      call open_for_read( unum, filename, ierr )
      if ( ierr .ne. 0 ) then
        if ( qlevel .ge. 3 ) print *, ' Calibration file ' // 
     $    filename(1:trimlen(filename)) // ' not found.'
        ierr = 1
        close(unum)
        return
      endif

      do j = 1, n_mse_chans, perline
        i1 = j
        i2 = min0( j + 8 - 1, n_mse_chans )

   80   read( unum, '(a)', end = 902 ) line
        if ( line .eq. ' ' .or. line(1:1) .eq. '!' ) goto 80

        read( line, *, err = 901 ,end = 901) ( gain(i), i = i1, i2 )

   81   read( unum, '(a)', end = 902 ) line 
        if ( line .eq. ' ' .or. line(1:1) .eq. '!' ) goto 81

        read( line, *, err = 901 ) ( slope(i), i = i1, i2 )

   82   read( unum, '(a)', end = 902 ) line
        if ( line .eq. ' ' .or. line(1:1) .eq. '!' ) goto 82

        read( line, *, err = 901 ) ( scale(i), i = i1, i2 )

   83   read( unum, '(a)', end = 902 ) line
        if ( line .eq. ' ' .or. line(1:1) .eq. '!' ) goto 83

        read( line, *, err = 901 ) ( offset(i), i = i1, i2 )
      enddo

      do i = 1, n_mse_chans
        mcal_gain(i)     =  gain(i)
        mcal_slope(i)    =  slope(i)
        mcal_btoffset(i) =  offset(i)
        mcal_btscale(i)  =  scale(i)
        if ( qlevel .ge. 5 ) then 
          print *, i, gain(i), slope(i), offset(i), scale(i)
        endif
      enddo

      do i = n_mse_chans + 1, max_mse_chans
        mcal_gain(i)     =  0.0
        mcal_slope(i)    =  0.0
        mcal_btoffset(i) =  0.0
        mcal_btscale(i)  =  0.0
      enddo
      goto 999
      
      ! error exits

 901  continue
      if ( qlevel .ge. 3 ) print * ,' Illegal data format in file.' //
     $  ' File possibly in new format'
      ierr = 2
      goto 999

 902  continue
      if ( qlevel .ge. 3 ) print *, ' Reached EOF prematurely.'
      ierr = 3

 999  continue
      close(unum)
      return
      end
c-------------------------------------------------------------------------------
c routine: read_mcalgain_rows
c author:  michael d. brown, llnl
c descrpt: given a mcalgain file number, read the calibration
c          file data into the common block described in mse_lib2.inc
c changes:
c          01/27/92  mdb  original version.
c          03/23/94  mdb  Modified for 16-channel mse (shots > 80539)
c          01/05/07  wm   Copied and modified to be transpose of original
c                         mcalgain file (ie the rows are channels numbers)
c
c-------------------------------------------------------------------------------
c Inputs:
c
c   filename = calibration file name
c
c-------------------------------------------------------------------------------
c Output:
c
c   ierr = error codes relating to read of calibration file
c        = 0: ok
c        = 1: couldn't open setup file.
c        = 2: illegal data format.
c        = 3: reached eof before all data was found.
c
c-------------------------------------------------------------------------------

      subroutine read_mcalgain_rows( filename, ierr )

      implicit  none
      include   'mse_lib2.inc'

      character*(*) filename
      integer*4 ierr

      character line*80
      integer*4 i, i1, i2, j, unum /45/, trimlen
      real*4    gain(max_mse_chans)
      real*4    slope(max_mse_chans)
      real*4    scale(max_mse_chans)
      real*4    offset(max_mse_chans)

      if ( qlevel .ge. 1 ) print *, 
     $        '(mse_lib2:read_mcalgain_rows) ',filename

      call open_for_read( unum, filename, ierr )
      if ( ierr .ne. 0 ) then
        if ( qlevel .ge. 3 ) print *, ' Calibration file ' // 
     $    filename(1:trimlen(filename)) // ' not found.'
        ierr = 1
        close(unum)
        return
      endif

      do j = 1, n_mse_chans

   80   read( unum, '(a)', end = 902 ) line
        if ( line .eq. ' ' .or. line(1:1) .eq. '!' ) goto 80

        read( line, *, err = 901 ) i, gain(j), slope(j),
     $	   scale(j), offset(j)

      enddo

      do i = 1, n_mse_chans
        mcal_gain(i)     =  gain(i)
        mcal_slope(i)    =  slope(i)
        mcal_btoffset(i) =  offset(i)
        mcal_btscale(i)  =  scale(i)
        if ( qlevel .ge. 6 ) then 
          print *, i, gain(i), slope(i), offset(i), scale(i)
        endif
      enddo

      do i = n_mse_chans + 1, max_mse_chans
        mcal_gain(i)     =  0.0
        mcal_slope(i)    =  0.0
        mcal_btoffset(i) =  0.0
        mcal_btscale(i)  =  0.0
      enddo
      goto 999
      
      ! error exits

 901  continue
      if ( qlevel .ge. 3 ) print * ,
     $ ' Illegal data format in new format file.' //
     $   char(13) // char(10) // line
      ierr = 2
      goto 999

 902  continue
      if ( qlevel .ge. 3 ) print *, ' Reached EOF prematurely.'
      ierr = 3

 999  continue
      close(unum)
      return
      end

c-------------------------------------------------------------------------------
c routine: read_mrz
c author:  michael d. brown, llnl
c descrpt: given a mrz file name read the r and z location
c          and geometry coefficients a1,a2,a3,a4 and delr and delz.
c          file data into the common block described in mse_lib2.inc.
c changes:
c          01/27/92  mdb  original version.
c          01/30/92  mdb  added delr and delz
c          03/23/94  mdb  modified for 16-channel mse (shots > 80539)
c          04/09/97  bwr  modified for 35 channel mse
c                         removed delr, delz and replaced with a5, a6
C
c-------------------------------------------------------------------------------
c Inputs:
c
c   filename = mrz-file file name
c
c-------------------------------------------------------------------------------
c Output:
c
c   ierr = error code from routine
c        = 0: ok
c        = 1: couldn't open setup file
c        = 2: illegal data format
c        = 3: reached eof before all data was found
c        = 4: sightline number mis-match
c   Routine also populates common the block variables a1, a2, a3, a4, a5, a6,
c     a7, r, z
c
c-------------------------------------------------------------------------------

      subroutine read_mrz( filename, ierr )

      implicit none
      include 'mse_lib2.inc'

      character*(*) filename
      integer*4 ierr

      character line*100, element*2, origline*100
      character lr*2, beam*1, sl*2
      integer*4 unum /46/, trimlen, isl, i_a
      real*4    vals(9)

      if ( qlevel .ge. 1 ) print *, '(mse_lib2:read_mrz) ', filename

      call open_for_read( unum, filename, ierr )
      if ( ierr .ne. 0 ) then
        if ( qlevel .ge. 3 ) print *, ' Calibration file ' // 
     $    filename(1:trimlen(filename)) // ' not found.'
        ierr = 1
        close(unum)
        return
      endif

      do while( .true. )
 80     read( unum, '(a)', end = 999 ) origline
        if ( origline .eq. ' ' .or. origline(1:1) .eq. '!' ) goto 80

        ! Parse off the sightline # and lr mode and qa them.

        line = origline
        sl = element(line)
        read( sl, *, err = 901 ) isl ! sightline number.
        if ( isl .le. 0 .or. isl .gt. n_sightlines ) then
          if ( qlevel .ge. 3 ) print *, ' Sightline number out of range'
          ierr = 4
          close(unum)
          return
        endif
        lr = element(line)
            
        ! Read in (decode) the remaining 8 values and put them in the common

        read( line, *, err = 901,end=900 ) vals
 900    r(isl) = vals(1)
        z(isl) = vals(2)
        a_coefs_mrz(isl,1) = vals(3)
        a_coefs_mrz(isl,2) = vals(4)
        a_coefs_mrz(isl,3) = vals(5)
        a_coefs_mrz(isl,4) = vals(6)
        a_coefs_mrz(isl,5) = vals(7)
        a_coefs_mrz(isl,6) = vals(8)
        a_coefs_mrz(isl,7) = vals(9)
      enddo
      close(unum)
      return

      ! Error exits

 901  continue
      if ( qlevel .ge. 3 ) print *, ' Illegal data format in file.' //
     $  char(13) // char(10) // origline
      ierr = 2

 999  continue
      close(unum)
      return
      end

c-------------------------------------------------------------------------------
c routine: read_mcalgain3
c author:  mike makowski based on m. d. brown's routine read_mrz
c desc:    given a mcalgain3 file name read the calibration constants
c          gain, phase, dc_offset, btscale. Place data into the 
c          appropriate arrays in the common block described in 
c          mse_lib2.inc.
c changes:
c          01/30/02  mam  original version.
c
c-------------------------------------------------------------------------------
c Inputs:
c
c   filename = name of calibration file
c
c-------------------------------------------------------------------------------
c Output:
c
c   ierr = error code
c        = 0: ok
c        = 1: couldn't open setup file.
c        = 2: illegal data format.
c        = 3: reached eof before all data was found.
c        = 4: sightline number mis-match.
c
c-------------------------------------------------------------------------------

      subroutine read_mcalgain3( filename, ierr )

      implicit none
      include 'mse_lib2.inc'

      character*(*) filename
      integer*4 ierr 

      integer*4 unum /47/, trimlen, ichan
      character line*100, element*2, origline*100
      character chan*2
      real*4    vals(4)

      if ( qlevel .ge. 1 ) print *, '(mselib2:read_mcalgain3) ',filename

      call open_for_read( unum, filename, ierr )
      if ( ierr .ne. 0 ) then
        if ( qlevel .ge. 3 ) print *, '  File ' // 
     $    filename(1:trimlen(filename)) // ' not found.'
        ierr = 1
        close(unum)
        return
      endif

      do while( .true. )
 80     read( unum, '(a)', end = 999 ) origline
        if ( origline .eq. ' ' .or. origline(1:1) .eq. '!' ) goto 80

        ! Parse off the channel number

        line = origline
        chan = element(line)
        read( chan, *, err = 901 ) ichan
        if ( ( ichan .le. 0 ) .or. ( ichan .gt. n_mse_chans ) ) then
          if ( qlevel .ge. 3 ) print *,
     $      '  Channel order mis-match in calibration file'
          ierr = 4
          close(unum)
          return
        endif
            
        ! Read in (decode) the remaining 6 values and put them in the common.

        read( line, *, err = 901 ) vals
        mcal3_gain(ichan)      = vals(1)
        mcal3_phase(ichan)     = vals(2)
        mcal3_dc_offset(ichan) = vals(3)
        mcal3_btscale(ichan)   = vals(4)

      enddo
      goto 999

      ! Error exits

 901  continue
      if ( qlevel .ge. 3 ) print *, '  Illegal data format in file.' // 
     $  char(13) // char(10) // origline
      ierr = 2

 999  continue
      close(unum)

      return
      end

c-------------------------------------------------------------------------------
c routine: read_mcalgain4
c author:  Bill Meyer based on makowski's routine read_mcalgain3
c desc:    given a mcalgain4 file name read the calibration constants
c          c0-c4 polynomial coefficients. Place data into the 
c          appropriate arrays in the common block described in 
c          mse_lib2.inc.
c changes:
c          03/10/11  meyer  original version.
c
c-------------------------------------------------------------------------------
c Inputs:
c
c   filename = name of calibration file
c
c-------------------------------------------------------------------------------
c Output:
c
c   ierr = error code
c        = 0: ok
c        = 1: couldn't open setup file.
c        = 2: illegal data format.
c        = 3: reached eof before all data was found.
c        = 4: sightline number mis-match.
c
c-------------------------------------------------------------------------------

      subroutine read_mcalgain4( filename, ierr )

      implicit none
      include 'mse_lib2.inc'

      character*128 filename
      integer*4 ierr 

      integer*4 unum /47/, trimlen, ichan
      character line*100, element*2, origline*100
      character chan*2
      real*4    vals(5)

      if ( qlevel .ge. 1 ) print *, '(mselib2:read_mcalgain4) ',filename

      call open_for_read( unum, filename, ierr )
      if ( ierr .ne. 0 ) then
        if ( qlevel .ge. 3 ) print *, '  File ' // 
     $    filename(1:trimlen(filename)) // ' not found.'
        ierr = 1
        close(unum)
        return
      endif

      do while( .true. )
 80     read( unum, '(a)', end = 999 ) origline
        if ( origline .eq. ' ' .or. origline(1:1) .eq. '!' ) goto 80

        ! Parse off the channel number

        line = origline
        chan = element(line)
        read( chan, *, err = 901 ) ichan
        if ( ( ichan .le. 0 ) .or. ( ichan .gt. n_mse_chans ) ) then
          if ( qlevel .ge. 3 ) print *,
     $      '  Channel order mis-match in calibration file'
          ierr = 4
          close(unum)
          return
        endif
            
        ! Read in (decode) the remaining 6 values and put them in the common.

        read( line, *, err = 901 ) vals
        mcal4_res(ichan,1)      = vals(1)
        mcal4_res(ichan,2)      = vals(2)
        mcal4_res(ichan,3)      = vals(3)
        mcal4_res(ichan,4)      = vals(4)
        mcal4_res(ichan,5)      = vals(5)

      enddo
      goto 999

      ! Error exits

 901  continue
      if ( qlevel .ge. 3 ) print *, '  Illegal data format in file.' // 
     $  char(13) // char(10) // origline
      ierr = 2

 999  continue
      close(unum)

      return
      end

c-------------------------------------------------------------------------------
c routine: read_mcalgain5
c author:  Bill Meyer based on makowski's routine read_mcalgain3
c desc:    given a mcalgain5 file name read the calibration constants
c          gain, phase, dc_offset, bcscale. Place data into the 
c          appropriate arrays in the common block described in 
c          mse_lib2.inc.The scaling is based on BCOIL instead of BT.
c changes:
c          09/02/20  meyer  original version.
c
c-------------------------------------------------------------------------------
c Inputs:
c
c   filename = name of calibration file
c
c-------------------------------------------------------------------------------
c Output:
c
c   ierr = error code
c        = 0: ok
c        = 1: couldn't open setup file.
c        = 2: illegal data format.
c        = 3: reached eof before all data was found.
c        = 4: sightline number mis-match.
c
c-------------------------------------------------------------------------------

      subroutine read_mcalgain5( filename, ierr )

      implicit none
      include 'mse_lib2.inc'

      character*(*) filename
      integer*4 ierr 

      integer*4 unum /47/, trimlen, ichan
      character line*100, element*2, origline*100
      character chan*2
      real*4    vals(4)

      if ( qlevel .ge. 1 ) print *, '(mselib2:read_mcalgain5) ',filename

      call open_for_read( unum, filename, ierr )
      if ( ierr .ne. 0 ) then
        if ( qlevel .ge. 3 ) print *, '  File ' // 
     $    filename(1:trimlen(filename)) // ' not found.'
        ierr = 1
        close(unum)
        return
      endif

      do while( .true. )
 80     read( unum, '(a)', end = 999 ) origline
        if ( origline .eq. ' ' .or. origline(1:1) .eq. '!' ) goto 80

        ! Parse off the channel number

        line = origline
        chan = element(line)
        read( chan, *, err = 901 ) ichan
        if ( ( ichan .le. 0 ) .or. ( ichan .gt. n_mse_chans ) ) then
          if ( qlevel .ge. 3 ) print *,
     $      '  Channel order mis-match in calibration file'
          ierr = 4
          close(unum)
          return
        endif
            
        ! Read in (decode) the remaining 6 values and put them in the common.

        read( line, *, err = 901 ) vals
        mcal3_gain(ichan)      = vals(1)
        mcal3_phase(ichan)     = vals(2)
        mcal3_dc_offset(ichan) = vals(3)
        ! note that if this routine is called then bcoil will be
        ! used instead of bt but the function is the same so bcoil
        ! scaling is just adjusted to work in fitfun=3
        mcal3_btscale(ichan)   = vals(4)*1.0e-5 

      enddo
      goto 999

      ! Error exits

 901  continue
      if ( qlevel .ge. 3 ) print *, '  Illegal data format in file.' // 
     $  char(13) // char(10) // origline
      ierr = 2

 999  continue
      close(unum)

      return
      end

c-------------------------------------------------------------------------------
c routine: read_spatial_average
c author:  m. makowski
c descrpt: reads the spatial average files and saves them in the common block
c          array spatial_average_data
c changes: 23-02-06 mam wrote routine
c
c-------------------------------------------------------------------------------
c Inputs:
c    None
c
c-------------------------------------------------------------------------------
c output:
c    ierr = error code 
c    Populated common block array, spatial_average_data
c
c-------------------------------------------------------------------------------

      subroutine read_spatial_average( ierr )

      implicit none
      include 'mse_lib2.inc'

      integer*4 ierr 
      character env_name*128, env_value*128
      integer*4 trimlen, ilen
      logical*1 i_env_name(128), i_env_value(128)

      character spave_filename*256, a1*256, line*256
      integer*4 i_err, i_ch, i_var, i_u, i_w
      integer*4 spave_lun /14/

      equivalence( env_name, i_env_name )
      equivalence( env_value, i_env_value )

      if ( qlevel .ge. 6 ) print *, '(mselib2:read_spatial_average) ',
     $  spave_dir_name

      env_name = 'MSE_SPATIAL_DIR'
      i_env_name(16) = 0

      call ptgetenv( i_env_name, i_env_value, len(env_value), ilen )

      read( spave_dir_name(2:3), * ) n_u
      read( spave_dir_name(6:7), * ) n_w

      do i_ch = 1, n_mse_chans

        ! Construct file name and read data

      if ( ilen .gt. 0 ) then
        a1 = env_value(1:trimlen(env_value)) // '/' //
     $    spave_dir_name(1:trimlen(spave_dir_name)) // '/mse_sa_ch'
      else
        a1 = '/fusion/projects/codes/mse/spatial_average/' // 
     $    spave_dir_name(1:trimlen(spave_dir_name)) // '/mse_sa_ch'
      endif
        write( spave_filename, '(a,i2.2,''_'',a,''.dat'')' ) 
     $    a1(1:trimlen(a1)), i_ch, 
     $    spave_dir_name(1:trimlen(spave_dir_name))
        
        call open_for_read( spave_lun, spave_filename, ierr )
        if (ierr .eq. 0) then
          do i_var = 1, n_spave_vars
            do i_w = 1, n_w
            read( spave_lun, * ,end=999,err=999) 
     $	    ( spatial_average_data( i_ch, i_var, 
     $        i_u, i_w ), i_u = 1, n_u )
          enddo
          read ( spave_lun, '(a)', end = 14 ,err = 999) line
 14       continue
        enddo
	endif
999     close(spave_lun)
      enddo

      return
      end

c-------------------------------------------------------------------------------
c routine: read_mse_files
c author:  michael d. brown
c descrpt: reads in the mse setup file and parse the msetup2.dat to determine
c          the calibration files corrresponding to the input shot number. Then 
c          reads in the calibration files and geometry file for the shot
c changes:
c          01/27/92 mdb original version.
c          2/92     dw  change the default directory for calibration
c                       files to phys_data:[eqdsk_d3.mse]
c                       (was diii$user:[wroblewsk.msm.cal])
c          2/94     mdb external routine to get calibration file directory.
c          11/01/00 mam modified for alternative calibration function
c          01/30/02 mam modified for moller fitting function (msefitfun=3)
c          07/11/05 mam minor modifications
c
c-------------------------------------------------------------------------------
c Input:
c
c   shot = shot number
c
c-------------------------------------------------------------------------------
c Output:
c
c   Initialization of a a large number of common block variables
c   ierr = error code 
c        = 0: ok
c        = 1: shot not found in setup
c        = 2: can't find calibration file
c        = 3: can't find rz file.
c
c-------------------------------------------------------------------------------

      subroutine read_mse_files( shot, ierr )

      implicit none
      include 'mse_lib2.inc'

      integer*4 shot
      integer*4 ierr

      character env_name*128, env_value*128
      integer*4 trimlen, ilen
      integer*4 mcalgain_filenum, mrz_filenum, mcalgain3_filenum
      integer*4 mcalgain5_filenum
      integer*4 spave_index, len_spave_dir 
      logical*1 i_env_name(128), i_env_value(128)
      integer*4 msefitfun

      equivalence( env_name, i_env_name )
      equivalence( env_value, i_env_value )

      if ( qlevel .ge. 1 ) print *, '(mse_lib2:read_mse_files)', shot

      if(fitfun .eq. 0) then
	 msefitfun = 0
         if ( qlevel .ge. 1 ) print *, '(mse_lib2:read_mse_files)', 
     $	 'Fitfun not set yet, setting default for shot.'
         call check_msefitfun( shot, msefitfun)
      endif

      ierr = 0
      env_name = 'MSE_CALIB_DIR'
      i_env_name(14) = 0

      call ptgetenv( i_env_name, i_env_value, len(env_value), ilen )

      call get_mse_calib_dir( mcaldir )

      if ( qlevel .gt. 1 ) then
        print *, 'For msetup2.dat, ilen = ', ilen
        print *, '  environment variable = ', env_name
        print *, '  env_value = ', env_value
        print *, '    mcaldir = ', mcaldir
      endif

      ! Read the msetup2.dat file.

      if ( ilen .gt. 0 .and. shot .ge. first_bcoil_shot ) then
        msetup_filename = env_value(1:trimlen(env_value)) // 
     $    '/msetup5.dat'
        call read_msetup5( msetup_filename, shot, 
     $    mrz_filenum, mcalgain5_filenum, ierr )
      else if ( ilen .gt. 0 ) then
        msetup_filename = env_value(1:trimlen(env_value)) // 
     $    '/msetup2.dat'
        call read_msetup( msetup_filename, shot, mcalgain_filenum,
     $    mrz_filenum, mcalgain3_filenum, ierr )
      endif

      if ( ( ( ilen .le. 0 ) .or. ( ierr .ne. 0 ) ) .and.
     $     ( shot .ge. first_bcoil_shot ) ) then
        msetup_filename = mcaldir(1:trimlen(mcaldir)) // 'msetup5.dat'
        call read_msetup5( msetup_filename, shot, 
     $    mrz_filenum, mcalgain5_filenum, ierr )
        if ( ierr .ne. 0 ) then
          ierr = 1
          return
        endif
      else if ( ( ilen .le. 0 ) .or. ( ierr .ne. 0 ) ) then
        msetup_filename = mcaldir(1:trimlen(mcaldir)) // 'msetup2.dat'
        
        call read_msetup( msetup_filename, shot, mcalgain_filenum,
     $    mrz_filenum, mcalgain3_filenum, ierr )
        if ( ierr .ne. 0 ) then
          ierr = 1
          return
        endif
      else
        if ( qlevel .ge. 1 ) write(6,*) ' Using users msetup file: ', 
     $    msetup_filename
      endif

      ! Read in the mcalgain file.
      ! This is not done for shots after first_bcoil_shot

      if ( shot .lt. first_bcoil_shot) then
       if ( ilen .gt. 0 ) then
	if ( mcalgain_filenum .lt. 100 ) then
          write( mcalgain_filename, '(a,''/mcalgain_'',i2.2,''.dat'')' )
     $      env_value(1:trimlen(env_value)), mcalgain_filenum
	else if ( mcalgain_filenum .lt. 1000 ) then
          write( mcalgain_filename, '(a,''/mcalgain_'',i3.3,''.dat'')' )
     $      env_value(1:trimlen(env_value)), mcalgain_filenum
	else
          write( mcalgain_filename, '(a,''/mcalgain_'',i4.4,''.dat'')' )
     $      env_value(1:trimlen(env_value)), mcalgain_filenum
        endif
        call read_mcalgain( mcalgain_filename, ierr )
	if (ierr .eq. 2)then
	    ierr = 0
            call read_mcalgain_rows( mcalgain_filename, ierr )
        endif
       endif
       if ( ilen .le. 0 .or. ierr .ne. 0 ) then
	if ( mcalgain_filenum .lt. 100 ) then
          write( mcalgain_filename, '(a,''mcalgain_'',i2.2,''.dat'')' )
     $      mcaldir(1:trimlen(mcaldir)), mcalgain_filenum
	else if ( mcalgain_filenum .lt. 1000 ) then
          write( mcalgain_filename, '(a,''mcalgain_'',i3.3,''.dat'')' )
     $      mcaldir(1:trimlen(mcaldir)), mcalgain_filenum
        else
          write( mcalgain_filename, '(a,''mcalgain_'',i4.4,''.dat'')' )
     $      mcaldir(1:trimlen(mcaldir)), mcalgain_filenum
        endif
        call read_mcalgain( mcalgain_filename, ierr )
	if (ierr .eq. 2)then
	    ierr = 0
            call read_mcalgain_rows( mcalgain_filename, ierr )
        endif
       else
        if ( qlevel .ge. 1 ) write(6,*) ' Using users mcalgain file: ', 
     $    mcalgain_filename
       endif

       if ( ierr .ne. 0 .and. fitfun .eq. 1  ) then
        ierr = 2
        return
       endif
      endif

      ! Read in the position file.
      ierr = 0

      if ( ilen .gt. 0 ) then
	if ( mrz_filenum .lt. 100 ) then
          write( mrz_filename, '(a,''/mrz_'',i2.2,''.dat'')' )
     $      env_value(1:trimlen(env_value)), mrz_filenum
	else if ( mrz_filenum .lt. 1000 ) then
          write( mrz_filename, '(a,''/mrz_'',i3.3,''.dat'')' )
     $      env_value(1:trimlen(env_value)), mrz_filenum
        else
          write( mrz_filename, '(a,''/mrz_'',i4.4,''.dat'')' )
     $      env_value(1:trimlen(env_value)), mrz_filenum
        endif
        call read_mrz( mrz_filename, ierr )
      endif

      if ( ilen .le. 0 .or. ierr .ne. 0 ) then
	if ( mrz_filenum .lt. 100 ) then
          write( mrz_filename, '(a,''mrz_'',i2.2,''.dat'')' )
     $      mcaldir(1:trimlen(mcaldir)), mrz_filenum
	else if ( mrz_filenum .lt. 1000 ) then
          write( mrz_filename, '(a,''mrz_'',i3.3,''.dat'')' )
     $      mcaldir(1:trimlen(mcaldir)), mrz_filenum
        else
          write( mrz_filename, '(a,''mrz_'',i4.4,''.dat'')' )
     $      mcaldir(1:trimlen(mcaldir)), mrz_filenum
        endif
        call read_mrz( mrz_filename, ierr )
      else
        if ( qlevel .ge. 1 ) write(6,*) ' Using users mrz file: ',
     $    mrz_filename
      endif

      if ( ierr .ne. 0 ) then
        ierr = 3
        return
      endif

      ! Read in the Moller calibration file for appropriate shots.

      if ( shot .gt. 100500 .and. shot .lt. first_bcoil_shot) then
        if ( ilen .gt. 0 ) then
	  if ( mcalgain3_filenum .lt. 100 ) then
            write( mcalgain3_filename, 
     $        '(a,''/mcalgain3_'',i2.2,''.dat'')' )
     $        env_value(1:trimlen(env_value)), mcalgain3_filenum
	  else if ( mcalgain3_filenum .lt. 1000 ) then
            write( mcalgain3_filename, 
     $        '(a,''/mcalgain3_'',i3.3,''.dat'')' )
     $        env_value(1:trimlen(env_value)), mcalgain3_filenum
	  else
            write( mcalgain3_filename, 
     $        '(a,''/mcalgain3_'',i4.4,''.dat'')' )
     $        env_value(1:trimlen(env_value)), mcalgain3_filenum
	  endif
          call read_mcalgain3( mcalgain3_filename, ierr )
        endif

        if ( ilen .le. 0 .or. ierr .ne. 0 ) then
	  if ( mcalgain3_filenum .lt. 100 ) then
            write( mcalgain3_filename, 
     $        '(a,''mcalgain3_'',i2.2,''.dat'')' )
     $        mcaldir(1:trimlen(mcaldir)), mcalgain3_filenum
	  else if ( mcalgain3_filenum .lt. 1000 ) then
            write( mcalgain3_filename, 
     $        '(a,''mcalgain3_'',i3.3,''.dat'')' )
     $        mcaldir(1:trimlen(mcaldir)), mcalgain3_filenum
	  else
            write( mcalgain3_filename, 
     $        '(a,''mcalgain3_'',i4.4,''.dat'')' )
     $        mcaldir(1:trimlen(mcaldir)), mcalgain3_filenum
	  endif
          call read_mcalgain3( mcalgain3_filename, ierr )
        else
          if ( qlevel .ge. 1 ) write(6,*) 
     $      ' Using users mcalgain3 file: ', mcalgain3_filename
        endif

        if ( ierr .ne. 0 .and. fitfun .ge. 3) then
          ierr = 5
          return
        endif
      endif

      ! Read in the Holcomb calibration file for appropriate shots.
      ierr = 0

      if ( shot .gt. 143020 .and. shot .lt. first_bcoil_shot) then
        if ( ilen .gt. 0 ) then
	  if ( mcalgain3_filenum .lt. 100 ) then
            write( mcalgain4_filename, 
     $        '(a,''/mcalgain4_'',i2.2,''.dat'')' )
     $        env_value(1:trimlen(env_value)), mcalgain3_filenum
	  else if ( mcalgain3_filenum .lt. 1000 ) then
            write( mcalgain4_filename, 
     $        '(a,''/mcalgain4_'',i3.3,''.dat'')' )
     $        env_value(1:trimlen(env_value)), mcalgain3_filenum
	  else
            write( mcalgain4_filename, 
     $        '(a,''/mcalgain4_'',i4.4,''.dat'')' )
     $        env_value(1:trimlen(env_value)), mcalgain3_filenum
	  endif
          call read_mcalgain4( mcalgain4_filename, ierr )
        endif

        if ( ilen .le. 0 .or. ierr .ne. 0 ) then
	  if ( mcalgain3_filenum .lt. 100 ) then
            write( mcalgain4_filename, 
     $        '(a,''mcalgain4_'',i2.2,''.dat'')' )
     $        mcaldir(1:trimlen(mcaldir)), mcalgain3_filenum
	  else if ( mcalgain3_filenum .lt. 1000 ) then
            write( mcalgain4_filename, 
     $        '(a,''mcalgain4_'',i3.3,''.dat'')' )
     $        mcaldir(1:trimlen(mcaldir)), mcalgain3_filenum
	  else
            write( mcalgain4_filename, 
     $        '(a,''mcalgain4_'',i4.4,''.dat'')' )
     $        mcaldir(1:trimlen(mcaldir)), mcalgain3_filenum
	  endif
          call read_mcalgain4( mcalgain4_filename, ierr )
        else
          if ( qlevel .ge. 1 ) write(6,*) 
     $      ' Using users mcalgain4 file: ', mcalgain4_filename
        endif

        if ( ierr .ne. 0 .and. fitfun .eq. 4) then
          if ( qlevel .ge. 1 ) write(6,*) 
     $      ' Fitfun4 but error reading: ', mcalgain4_filename
          ierr = 5
          return
        endif
      endif

      ! Read spatial average files

      if ( shot .lt. first_bcoil_shot ) then
       len_spave_dir = trimlen(spave_dir_name)
       read( spave_dir_name(len_spave_dir-2:len_spave_dir), * ) 
     $     spave_index

       if ( spave_index .gt. 0 ) then
        call read_spatial_average( ierr )
       endif
      endif

      ! Read in the Moller calibration file for appropriate shots.

      if ( shot .ge. first_bcoil_shot) then
        if ( ilen .gt. 0 ) then
	  if ( mcalgain5_filenum .lt. 100 ) then
            write( mcalgain5_filename, 
     $        '(a,''/mcalgain5_'',i2.2,''.dat'')' )
     $        env_value(1:trimlen(env_value)), mcalgain5_filenum
	  else if ( mcalgain5_filenum .lt. 1000 ) then
            write( mcalgain5_filename, 
     $        '(a,''/mcalgain5_'',i3.3,''.dat'')' )
     $        env_value(1:trimlen(env_value)), mcalgain5_filenum
	  else
            write( mcalgain5_filename, 
     $        '(a,''/mcalgain5_'',i4.4,''.dat'')' )
     $        env_value(1:trimlen(env_value)), mcalgain5_filenum
	  endif
          call read_mcalgain5( mcalgain5_filename, ierr )
        endif

        if ( ilen .le. 0 .or. ierr .ne. 0 ) then
	  if ( mcalgain5_filenum .lt. 100 ) then
            write( mcalgain5_filename, 
     $        '(a,''mcalgain5_'',i2.2,''.dat'')' )
     $        mcaldir(1:trimlen(mcaldir)), mcalgain5_filenum
	  else if ( mcalgain5_filenum .lt. 1000 ) then
            write( mcalgain5_filename, 
     $        '(a,''mcalgain5_'',i3.3,''.dat'')' )
     $        mcaldir(1:trimlen(mcaldir)), mcalgain5_filenum
	  else
            write( mcalgain5_filename, 
     $        '(a,''mcalgain5_'',i4.4,''.dat'')' )
     $        mcaldir(1:trimlen(mcaldir)), mcalgain5_filenum
	  endif
          call read_mcalgain5( mcalgain5_filename, ierr )
        else
          if ( qlevel .ge. 1 ) write(6,*) 
     $      ' Using users mcalgain5 file: ', mcalgain5_filename
        endif

        if ( ierr .ne. 0 .and. fitfun .ge. 3) then
          ierr = 5
          return
        endif
      endif
      return
      end

c-------------------------------------------------------------------------------
c routine: mse_geometry
c author:  michael d. brown; modified by m. makowski
c descrpt: given an r,z (in meters) calculate a1, ..., a7, and the angles 
C          alpha, omega, theta, phi_d, and phi_tor
c changes:
c
c   11/18/05 mam rewrote routine
c   12/08/06 mam added phi_tor
c
c-------------------------------------------------------------------------------
c Inputs:
c
c   shot = shot number
c
c-------------------------------------------------------------------------------
c Output:
c
c              x = array of x-coordinate of intersection of beam centerline and 
c                  sightline
c              y = array of y-coordinate of intersection of beam centerline and 
c                  sightline
c          alpha = array of angles between beam and toroidal field
c          omega = array of pi/2 + angle between t and toroidal field
c          theta = array of view angles relative to horizontal plane
c          phi_d = array of Doppler shift angles
c        phi_tor = array of (machine) toroidal angles for each channel
c        a_coefs = array of the 7 A-coefficients for each channel
c       xyz_lens = array of xyz lens locations for each channel
c     resolution = array of radial resolutions for each channel
c
c-------------------------------------------------------------------------------
c Notes:
c
c   All lengths in meters
c
c-------------------------------------------------------------------------------

      subroutine mse_geometry( x, y, alpha, omega, theta, phi_d,
     $  phi_tor, a_coefs, xyz_lens, resolution )

      implicit  none
      include   'mse_lib2.inc'

      integer*4 i_ch, i_bm, i_a, get_beam_index
      real*4    beam_offset(2), beam_slope(2)
      real*4    xlens_315,  ylens_315,  zlens_315
      real*4    xlens_045,  ylens_045,  zlens_045
      real*4    xlens_015,  ylens_015,  zlens_015
      real*4    xlens_195l, ylens_195l, zlens_195l
      real*4    xlens_195u, ylens_195u, zlens_195u
      real*4        x(max_mse_chans),     y(max_mse_chans)
      real*4      r_x(max_mse_chans),   r_y(max_mse_chans)
      real*4    phi_x(max_mse_chans), phi_y(max_mse_chans)
      real*4      v_x(max_mse_chans),   v_y(max_mse_chans)
      real*4      s_x(max_mse_chans),   s_y(max_mse_chans)
      real*4      s_z(max_mse_chans)
      real*4      t_x(max_mse_chans),   t_y(max_mse_chans)
      real*4    alpha(max_mse_chans), omega(max_mse_chans)
      real*4    theta(max_mse_chans), phi_d(max_mse_chans)
      real*4    phi_tor(max_mse_chans)
      real*4    a_coefs( max_mse_chans, 7 ), resolution(max_mse_chans)
      real*4    xyz_lens( max_mse_chans, 3 )
      real*4    beam_voltage(max_beams) /81000.0, 81000.0,
     $                                   81000.0, 81000.0/ ! Volt
      real*4    cos_bm_slope, sin_bm_slope, du
      real*4    sign, x_lens, y_lens, z_lens, sign_arg, vb_full, vb_half
      real*4    spot_size, beam_waist_full

      pi = 4.0 * atan(1.0)
      r2d = 180.0 / pi
      d2r = pi / 180.0
      if ( qlevel .ge. 3 ) print *, '(mse_lib2:mse_geometry)'

      ! Lens position data

      data xlens_315,  ylens_315,  zlens_315  
     $  /-1.8568,  2.0429, -0.13335/
      data xlens_045,  ylens_045,  zlens_045
     $  / 1.7310,  1.7230, -0.0213/
      data xlens_015,  ylens_015,  zlens_015
     $  / 0.6670,  2.5050,  0.0254/
      data xlens_195l, ylens_195l, zlens_195l 
     $  /-0.6294, -2.3491, -0.1270/
      data xlens_195u, ylens_195u, zlens_195u 
     $  /-0.6294, -2.3491,  0.1270/

      ! Offset and slope of the beam centerline for 030lt and 210rt
      ! 210rt data from March 2006 calibration by Holcomb - see Mathematica
      !   notebook "L210rt_20060309.nb"

      data beam_offset / 1.42305, 10.7242  /
      data beam_slope  / 0.72371,  9.29661 /

      ! For a given major radius and beam (30 right or left) calculate
      !   all the angles.
      ! For a given r (major radius), calculate the position on beam trajectory
      !   (this will be the point of intersection of sightline with the beam).
      !   the beam trajectory is described by: y = bb*x  + ab

      do i_ch = 1, n_mse_chans

        ! Get beam index and set the sign parameter for the beam

        i_bm = get_beam_index(i_ch)
        if ( i_bm .eq. 1 ) then
          sign = -1.0
        else
          sign = 1.0
        endif

        cos_bm_slope = 1.0 / sqrt( 1.0 + beam_slope(i_bm)**2 )
        sin_bm_slope = beam_slope(i_bm) / 
     $    sqrt( 1.0 + beam_slope(i_bm)**2 )

        ! Beam velocities

        vb_full = sqrt( 2.0 * beam_voltage(i_bm) * 1.6022e-19 / 2.0 / 
     $    1.6726e-27 ) 
        vb_half = vb_full / sqrt(2.0)

        ! Get the viewport so that the lens positions for the channel can be
        ! obtained

        if ( viewport(i_ch) .eq. '315' ) then
          x_lens = xlens_315
          y_lens = ylens_315
          z_lens = zlens_315
        else if ( viewport(i_ch) .eq. '15' ) then
          x_lens = xlens_015
          y_lens = ylens_015
          z_lens = zlens_015
        else if ( viewport(i_ch) .eq. '45' ) then
          x_lens = xlens_045
          y_lens = ylens_045
          z_lens = zlens_045
        else if ( viewport(i_ch) .eq. '195l' ) then
          x_lens = xlens_195l
          y_lens = ylens_195l
          z_lens = zlens_195l
        else if ( viewport(i_ch) .eq. '195u' ) then
          x_lens = xlens_195u
          y_lens = ylens_195u
          z_lens = zlens_195u
        endif

        xyz_lens( i_ch, 1 ) = x_lens
        xyz_lens( i_ch, 2 ) = y_lens
        xyz_lens( i_ch, 3 ) = z_lens

        ! Calculate the (x, y) coordinates of the intersection of the sightline
        ! and beam centerline 

        x(i_ch) = beam_offset(i_bm) * cos_bm_slope * ( - sin_bm_slope -
     $    sign * sqrt( - cos_bm_slope**2 + r(i_ch)**2 / 
     $    beam_offset(i_bm)**2 ) )
        if ( i_bm .eq. 1 ) then
           y(i_ch) = sqrt( r(i_ch)**2 - x(i_ch)**2 )
        else
           y(i_ch) = - sqrt( r(i_ch)**2 - x(i_ch)**2 )
        endif

        ! Calculate phi_tor = machine toroidal angle of each emission point

        phi_tor(i_ch) = - atan2( y(i_ch), x(i_ch) ) + pi / 2.0

        ! Calculate unit vectors for the directions of interest
        ! Major radial direction (radial-vector)

        r_x(i_ch) = x(i_ch) / sqrt( x(i_ch)**2 + y(i_ch)**2 )
        r_y(i_ch) = y(i_ch) / sqrt( x(i_ch)**2 + y(i_ch)**2 )

        ! Toroidal direction (phi-vector)

        phi_x(i_ch) = - r_y(i_ch)
        phi_y(i_ch) =   r_x(i_ch)

        ! Beam propagation direction

        v_x(i_ch) = - cos_bm_slope
        v_y(i_ch) = - sin_bm_slope

        ! Observation direction (s-vector)
 
        du = sqrt( ( x(i_ch) - x_lens )**2 + ( y(i_ch) - y_lens )**2 + 
     $    ( z(i_ch) - z_lens )**2 )
        s_x(i_ch) = ( x_lens - x(i_ch) ) / du
        s_y(i_ch) = ( y_lens - y(i_ch) ) / du
        s_z(i_ch) = ( z_lens - z(i_ch) ) / du

        ! t-vector

        t_x(i_ch) = - s_y(i_ch) / sqrt( s_x(i_ch)**2 + s_y(i_ch)**2 )
        t_y(i_ch) =   s_x(i_ch) / sqrt( s_x(i_ch)**2 + s_y(i_ch)**2 )

        ! alpha = angle between beam and toroidal field

        sign_arg = phi_x(i_ch) * v_y(i_ch) - phi_y(i_ch) * v_x(i_ch)
        alpha(i_ch) = acos( phi_x(i_ch) * v_x(i_ch) + phi_y(i_ch) * 
     $    v_y(i_ch) ) * sign_arg / abs( sign_arg )

        ! omega = Pi/2 + angle between t and toroidal field

        sign_arg = t_x(i_ch) * phi_y(i_ch) - t_y(i_ch) * phi_x(i_ch)
        omega(i_ch) = pi / 2.0 + acos( phi_x(i_ch) * t_x(i_ch) +
     $    phi_y(i_ch) * t_y(i_ch) ) * sign_arg / abs( sign_arg )

        ! phi_d = Doppler angle

        sign_arg = v_x(i_ch) * s_y(i_ch) - v_y(i_ch) * s_x(i_ch)
        phi_d(i_ch) = - acos( - ( v_x(i_ch) * s_x(i_ch) + v_y(i_ch) * 
     $    s_y(i_ch) ) ) * sign_arg / abs( sign_arg )

        ! theta = view angle relative to horizontal plane

        theta(i_ch) = atan( ( z_lens - z(i_ch) ) / sqrt( 
     $    ( x_lens - x(i_ch) )**2 + ( y_lens - y(i_ch) )**2 ) )

        ! Compute the A-coefficients

        a_coefs(i_ch,1) = - cos( alpha(i_ch) + omega(i_ch) )
        a_coefs(i_ch,2) = cos( theta(i_ch) ) * sin( alpha(i_ch) )
        a_coefs(i_ch,3) = cos( theta(i_ch) ) * cos( alpha(i_ch) )
        a_coefs(i_ch,4) = sin( theta(i_ch) ) * 
     $    sin( omega(i_ch) + alpha(i_ch) )
        a_coefs(i_ch,5) = - cos( omega(i_ch) ) / vb_full
        a_coefs(i_ch,6) = - cos( theta(i_ch) ) / vb_full
        a_coefs(i_ch,7) = sin( theta(i_ch) ) * sin( omega(i_ch) ) / 
     $    vb_full

        ! Compute the resolution
        
        if ( viewport(i_ch) .eq. '315' ) then
          spot_size = 0.020
        else if ( viewport(i_ch) .eq. '15' ) then
          spot_size = 0.010
        else if ( viewport(i_ch) .eq. '45' ) then
          spot_size = 0.012
        else if ( viewport(i_ch) .eq. '195l' ) then
          spot_size = 0.012
        else if ( viewport(i_ch) .eq. '195u' ) then
          spot_size = 0.005
        endif
        beam_waist_full = 0.14
        resolution(i_ch) = ( spot_size * abs( sin( alpha(i_ch) ) ) + 
     $    beam_waist_full * abs( sin( omega(i_ch) ) ) ) / 
     $    abs( sin( alpha(i_ch) + omega(i_ch) ) )

      enddo

      return
      end

c-------------------------------------------------------------------------------
c routine: get_mse_max_chans
c descrpt: returns mse_max_chans for dimensioning by idl and other languages
c author:  b. meyer
c
c-------------------------------------------------------------------------------
c Output:
c            n_chans = maximum number of mse channels for dimensioning
c
c-------------------------------------------------------------------------------

      subroutine get_mse_max_chans( n_chans )

      implicit  none
      include   'mse_lib2.inc'

      integer*4 n_chans

      n_chans = max_mse_chans
      return
      end
c------------------------------------------------------------------------------- 
c routine: set_mse_beam_logic
c descrpt: sets parameters for handling beam off interpolations
c author:  b. meyer
c
c-------------------------------------------------------------------------------
c Input:
c       mse_strict = integer flag for beam off interpolation
c                  = 0 interpolation between beam blips allowed
c                  = 1 no mse data if beams off
c       max_t_beam_off = if beam off but time between intervals is
c                        less than this, interpolation is allowed
c
c-------------------------------------------------------------------------------

      subroutine set_mse_beam_logic( lmse_strict, dmax_t_beam_off ,
     *                               lok_210lt, lok_30rt  )

      implicit  none
      include   'mse_lib2.inc'

      integer*4 lmse_strict, lok_210lt, lok_30rt
      real*4 dmax_t_beam_off

      mse_strict = lmse_strict
      max_t_beam_off = dmax_t_beam_off
      ok_210lt = lok_210lt
      ok_30rt = lok_30rt
      return
      end

c------------------------------------------------------------------------------- 
c routine: set_mse_beam_on_vlevel
c descrpt: sets voltage thresholds for beam on
c author:  b. meyer
c
c-------------------------------------------------------------------------------
c Input:
c       v30lt      = Voltage threshold to consider 30left beam on (79kV default)
c       v210rt     = Voltage threshold to consider 210right beam on (79kV default)
c       v210lt     = Voltage threshold to consider 210lt beam on (61kV default)
c       v30rt      = Voltage threshold to consider 30right beam on (61kV default)
c
c-------------------------------------------------------------------------------

      subroutine set_mse_beam_on_vlevel(v30lt,v210rt,v210lt,v30rt)

      implicit  none
      include   'mse_lib2.inc'

      real*4 v30lt,v210rt,v210lt,v30rt

      if(v30lt .gt. 0.0)  beam_v_on_level(1) = v30lt
      if(v210rt .gt. 0.0) beam_v_on_level(2) = v210rt
      if(v210lt .gt. 0.0) beam_v_on_level(3) = v210lt
      if(v30rt .gt. 0.0)  beam_v_on_level(4) = v30rt

      return
      end

c------------------------------------------------------------------------------- 
c routine: set_cer_correction(use_cer,certree,use_cer330,use_cer210)
c descrpt: sets parameters for handling cer correction
c author:  b. meyer
c
c-------------------------------------------------------------------------------
c Input:
c       use_cer = 1 turn on the cer correction
c                 0 turn off the cer correction (default)
c      certree  = mdsplus tree to read cer data from
c                 0 - mdsplus tree CERQUICK (default)
c                 1 - CERAUTO
c                 2 - CERFIT
c                 3 - CERNEUR
c     use_cer330 = use cer channals with viewing angles between 310-330 degrees
c     use_cer210 = use cer channals with viewing angles between 210-230 degrees
c
c     Channels with viewing angles < 25 are used by default.
c-------------------------------------------------------------------------------
      subroutine set_cer_correction( luse_cer,lcertree,
     $      luse_cer330,luse_cer210)

      implicit none
      include 'mse_lib2.inc'

      integer *4 luse_cer,lcertree,luse_cer330,luse_cer210


      use_cer = luse_cer
      certree = lcertree
      use_cer330 = luse_cer330
      use_cer210 = luse_cer210
      return
      end

c-------------------------------------------------------------------------------
c routine: set_mse_quiet
c descrpt: sets debug level
c author:  b. meyer
c
c------------------------------------------------------------------------------- c Input:
c       quiet = flag for printing messages
c             = 0 for minimum output
c             = 1 for basic information
c             = 3 for a trace of the calling sequence of routines
c             = 6 for extensive output from this routine and those in mse_lib2
c
c-------------------------------------------------------------------------------

      subroutine set_mse_quiet( quiet )

      implicit  none
      include   'mse_lib2.inc'

      integer*4 quiet

      qlevel = quiet
      return
      end

c-------------------------------------------------------------------------------
c routine: set_mse_bksub
c descrpt: sets debug level
c author:  b. meyer
c
c-------------------------------------------------------------------------------
c Input:
c  bksub 
c    = 0 - up to 1 s before beam turn-on and up to 1 sec after beam turn-off
c          with a 0.2 offset
c    = 1 - use between beam blip intervals to form baseline average
c    = 2 - use all mse data prior to t=0 only (no post-beam-off data)
c    = 3 - modified option 0 (eliminates junps in averaging interval)
c    = 4 - use (t0, t0 + up to 1 s) and (t1 - up to 1 s, t1) with a 0.2 s 
c          offset
c
c-------------------------------------------------------------------------------

      subroutine set_mse_bksub( bksub )

      implicit  none
      include   'mse_lib2.inc'

      integer*4 bksub

      bksub_mode = bksub
      return
      end


c-------------------------------------------------------------------------------
c routine: get_mse_spatial_data
c descrpt: returns spatial_average_data saved in common as a passable array
c author:  b. meyer
c
c-------------------------------------------------------------------------------
c Ouptut:
c         spatial_array = spatial averaging data read from calibration file
c
c-------------------------------------------------------------------------------

      subroutine get_mse_spatial_data( spatial_array )

      implicit  none
      include   'mse_lib2.inc'

      integer*4 i_var, i_w, i_ch, i_u
      real*4    spatial_array( max_mse_chans, n_spave_vars, n_u, n_w )

      do i_ch = 1, n_mse_chans
        do i_var = 1, n_spave_vars
          do i_w = 1, n_w
	    do i_u = 1, n_u
            spatial_array( i_ch, i_var, i_u, i_w ) = 
     $        spatial_average_data( i_ch, i_var, i_u, i_w )
            enddo
          enddo
        enddo
      enddo

      return
      end

c-------------------------------------------------------------------------------
c routine: get_mse_calibration
c descrpt: returns calibration data saved in common as a passable array
c author:  b. meyer
c
c-------------------------------------------------------------------------------
c Inputs:
c
c   msefitfun = 1 for tangent slope model
c             = 3 for tangetn offset model
c             = 4 for tangetn offset residual model
c Output:
c         gain,slope,scale,offset = data read from calibration file
c                                   depends on mse function
c
c-------------------------------------------------------------------------------

      subroutine get_mse_calibration( msefitfun,
     .                    gain,slope,scale,offset )


      implicit  none
      include   'mse_lib2.inc'
      integer*4 msefitfun,i_ch
      real*4    gain(max_mse_chans)
      real*4    slope(max_mse_chans)
      real*4    scale(max_mse_chans)
      real*4    offset(max_mse_chans)

      if(msefitfun .eq. 1) then
         do i_ch = 1, max_mse_chans
		gain(i_ch) = mcal_gain(i_ch)
		slope(i_ch) = mcal_slope(i_ch)
		scale(i_ch) = mcal_btscale(i_ch)
		offset(i_ch) = mcal_btoffset(i_ch)
         enddo
      else if (msefitfun .eq. 3) then
         do i_ch = 1, max_mse_chans
		gain(i_ch) = mcal3_gain(i_ch)
		slope(i_ch) = mcal3_phase(i_ch)
		scale(i_ch) = mcal3_btscale(i_ch)
		offset(i_ch) = mcal3_dc_offset(i_ch)
         enddo
      else if (msefitfun .eq. 4) then
         do i_ch = 1, max_mse_chans
		gain(i_ch) = mcal3_gain(i_ch)
		slope(i_ch) = mcal3_phase(i_ch)
		scale(i_ch) = mcal3_btscale(i_ch)
		offset(i_ch) = mcal3_dc_offset(i_ch)
         enddo
      else
	write(6,*) "Msefitfun of ",msefitfun," not implemented."
      endif		

      return
      end

c-------------------------------------------------------------------------------
c function: trimlen
c author: michael d. brown, llnl
c descrpt: returns the length of a string with trailing spaces/tabs removed.
c          note that an all-blank string has trimmed length of 0.
c changes:
c
c   08/07/89 mdb original version.
c-------------------------------------------------------------------------------

      integer*4 function trimlen( string )

      implicit none
      character*(*) string

      integer*4 i

      trimlen = 0
      do i = len(string), 1, -1
        if ( string(i:i) .ne. ' ' .and. string(i:i) .ne. char(9) ) then
          trimlen = i
          return
        endif
      enddo

      return
      end

c-------------------------------------------------------------------------------
c routine: get_mse_calib_dir
c descprt: provides default directroy for obtaining the mse calibration files
c
c-------------------------------------------------------------------------------
c Inputs: None
c 
c-------------------------------------------------------------------------------
c Output:
c
c   calib_dir = default mse calibration directory
c
c-------------------------------------------------------------------------------
   
      subroutine get_mse_calib_dir( calib_dir )

      implicit none
      character*96 calib_dir

      calib_dir = '/fusion/projects/codes/mse/calib/'

      return
      end

c-------------------------------------------------------------------------------
c routine: open_for_read
c descrpt: opens an existing data file in preparation for reading 
c
c-------------------------------------------------------------------------------
c Inputs:
c
c       unit = unit number for open
c   filename = file name to open for read only
c       ierr = error return (0=OK)
c
c-------------------------------------------------------------------------------
c Output:
c
c   open file
c
c-------------------------------------------------------------------------------

      subroutine open_for_read( unit, filename, ierr )

      implicit  none

      integer*4 unit, ierr
      character*(*) filename

      open( unit, file = filename, form = 'formatted', status = 'old', 
     $  iostat = ierr, err = 900 )
 900  continue

      return
      end

c-------------------------------------------------------------------------------
c routine: make_spline
c descrpt: compute cubic spline coeefficients
c          the coefficients b(i), c(i), and d(i), i=1,2,...,n are computed
c          for a cubic interpolating spline
c
c            s(x) = y(i) + b(i)*(x-x(i)) + c(i)*(x-x(i))**2 + d(i)*(x-x(i))**3
c
c          for  x(i) .le. x .le. x(i+1)
c          the accompanying function subprogram  seval  can be used to evaluate
c          the spline.
c changes:
c
c   11/15/05 mam - added routine (provided by bill meyer) to library
c
c-------------------------------------------------------------------------------
c Inputs:
c
c    n = the number of data points or knots (n.ge.2)
c    x = the abscissas of the knots in strictly increasing order
c    y = the ordinates of the knots
c
c-------------------------------------------------------------------------------
c Output:
c
c    b, c, d  = arrays of spline coefficients as defined above.
c    y(i) = s(x(i))
c    b(i) = sp(x(i))
c    c(i) = spp(x(i))/2
c    d(i) = sppp(x(i))/6  (derivative from the right)
c
c-------------------------------------------------------------------------------

      subroutine make_spline( n, x, y, b, c, d )

      integer n
      real x(*), y(*), b(*), c(*), d(*)

      integer nm1, ib, i
      real    t

      nm1 = n - 1
      if ( n .lt. 2 ) return
      if ( n .lt. 3 ) go to 50

      !  Set up tridiagonal system
      !  b = diagonal, d = offdiagonal, c = right hand side.

      d(1) = x(2) - x(1)
      c(2) = ( y(2) - y(1) ) / d(1)
      do i = 2, nm1
        d(i) = x(i+1) - x(i)
        b(i) = 2.0 * ( d(i-1) + d(i) )
        c(i+1) = ( y(i+1) - y(i) ) / d(i)
        c(i) = c(i+1) - c(i)
      enddo

      ! End conditions. Third derivatives at  x(1)  and  x(n)
      ! obtained from divided differences

      b(1) = -d(1)
      b(n) = -d(n-1)
      c(1) = 0.0
      c(n) = 0.0
      if ( n .ne. 3 ) then
        c(1) = c(3) / ( x(4) - x(2) ) - c(2) / ( x(3) - x(1) )
        c(n) = c(n-1) / ( x(n) - x(n-2) ) - c(n-2) / ( x(n-1) - x(n-3) )
        c(1) = c(1) * d(1)**2 / ( x(4) - x(1) )
        c(n) = -c(n) * d(n-1)**2 / ( x(n) - x(n-3) )
      endif

      ! Forward elimination

      do i = 2, n
        t = d(i-1) / b(i-1)
        b(i) = b(i) - t * d(i-1)
        c(i) = c(i) - t * c(i-1)
      enddo

      ! back substitution

      c(n) = c(n) / b(n)
      do ib = 1, nm1
        i = n - ib
        c(i) = ( c(i) - d(i) * c(i+1) ) / b(i)
      enddo

      ! c(i) is now the sigma(i) of the text
      ! compute polynomial coefficients

      b(n) = ( y(n) - y(nm1) ) / d(nm1) + d(nm1) * 
     $  ( c(nm1) + 2.0 * c(n) )
      do i = 1, nm1
        b(i) = ( y(i+1) - y(i) ) / d(i) - d(i) * ( c(i+1) + 2.0 * c(i) )
        d(i) = ( c(i+1) - c(i) ) / d(i)
        c(i) = 3.0 * c(i)
      enddo
      c(n) = 3.0 * c(n)
      d(n) = d(n-1)
      return

   50 b(1) = ( y(2) - y(1) ) / ( x(2) - x(1) )
      c(1) = 0.0
      d(1) = 0.0
      b(2) = b(1)
      c(2) = 0.0
      d(2) = 0.0

      return
      end

c-------------------------------------------------------------------------------
c routine: eval_spline (function)
c descrpt: this subroutine evaluates the cubic spline function
c
c            seval = y(i) + b(i)*(u-x(i)) + c(i)*(u-x(i))**2 + d(i)*(u-x(i))**3
c
c          where  x(i) .lt. u .lt. x(i+1), using horner's rule
c
c          if  u .lt. x(1) then  i = 1  is used.
c          if  u .ge. x(n) then  i = n  is used.
c          if  u  is not in the same interval as the previous call, then a
c          binary search is performed to determine the proper interval
c
c changes:
c
c   11/15/05 added routine (provided by bill meyer) to the library
c
c-------------------------------------------------------------------------------
c Inputs:
c
c   n = the number of data points
c   u = the abscissa at which the spline is to be evaluated
c   x,y = the arrays of data abscissas and ordinates
c   b,c,d = arrays of spline coefficients computed by spline
c
c-------------------------------------------------------------------------------
c Output:
c
c   Returns value of spline evaluated at u
c
c-------------------------------------------------------------------------------

      real*4 function eval_spline( n, u, x, y, b, c, d )

      integer i, j, k, n
      real    u, x(*), y(*), b(*), c(*), d(*)
      real    dx

      data    i /1/

      ! Uncomment this next line to force a binary search every call
      ! i = 1

      if ( i .ge. n ) i = 1
      if ( u .lt. x(i) ) go to 10
      if ( u .le. x(i+1) ) go to 30
      if ( u .le. x(1) ) then
        eval_spline = y(1)
        return
      else if ( u .ge. x(n) ) then
        eval_spline = y(n)
        return
      endif

      ! binary search

   10 i = 1
      j = n + 1
   20 k = ( i + j ) / 2
      if ( u .lt. x(k) ) j = k
      if ( u .ge. x(k) ) i = k
      if ( .not. ( u .lt. x(k) ) .and. .not. ( u .ge. x(k) ) ) then
        eval_spline = 0
        return
      endif
      if ( j .gt. i + 1 ) go to 20

      ! evaluate spline

   30 dx = u - x(i)
      eval_spline = y(i) + dx * ( b(i) + dx * ( c(i) + dx * d(i) ) )

      return
      end

      real function eval_linear(n, u, x, y)
      integer n
      real  u, x(n), y(n)
c
c  this subroutine does linear interpolation
c
c    seval = y(i) + b*(u-x(i))
c
c    where  x(i) .lt. u .lt. x(i+1), using horner's rule
c
c  if  u .lt. x(1) then  i = 1  is used.
c  if  u .ge. x(n) then  i = n  is used.
c
c  input..
c
c    n = the number of data points
c    u = the abscissa at which the spline is to be evaluated
c    x,y = the arrays of data abscissas and ordinates
c
c  if  u  is not in the same interval as the previous call, then a
c  binary search is performed to determine the proper interval.
c
      integer i, j, k
      real dx,b
      data i/1/
      ! Uncomment this next line to force a binary search every call
      i = 1
      if ( i .ge. n ) i = 1
      if ( u .lt. x(i) ) go to 10
      if ( u .le. x(i+1) ) go to 30
      if ( u .le. x(1) ) then
        eval_linear = y(1)
        return
      else if ( u .ge. x(n) ) then
        eval_linear = y(n)
        return
      endif
c
c  binary search
c
   10 i = 1
      j = n+1
   20 k = (i+j)/2
      if ( u .lt. x(k) ) j = k
      if ( u .ge. x(k) ) i = k
      if ( .not. ( u .lt. x(k) ) .and. .not. ( u .ge. x(k) ) ) then
        eval_linear = 0
        return
      endif
      if ( j .gt. i+1 ) go to 20
c
c  evaluate spline
c
   30 dx = u - x(i)
      if (i .lt. n) b = (y(i+1) - y(i))/(x(i+1)-x(i))
      else b = (y(i) - y(i-1))/(x(i)-x(i-1))
      eval_linear = y(i) + dx*b
      return
      end
c-------------------------------------------------------------------------------
c routine: lag3p_deriv
c descrpt: this subroutine returns the derivative of the input tabulated data
c          using a 3 point lagrange polynomial
c
c changes:
c
c   4/4/06 added routine to the library - wm
c
c-------------------------------------------------------------------------------
c inputs: 
c
c   n = number of points in tabulated data
c   x = x-axis of tabulated data
c   y = input data
c
c-------------------------------------------------------------------------------
c outputs: 
c
c   d = calculated dy/dx by differentiating a 3-rd order lagrange 
c       polynomial
c
c-------------------------------------------------------------------------------

      subroutine lag3p_deriv( n, x, y, d )

      implicit none
      real*4 x(*), y(*), d(*)
      integer n

      integer*4 i
      real*4    x1, x2, x3, y1, y2, y3
      real*4    x12, x13, x23, x21, x31, x32

      ! do interior points

      do i = 1, n
	if ( i .eq. 1 ) then
	  x1 = x(i)
          x2 = x(i+1)
          x3 = x(i+2)
          y1 = y(i)
          y2 = y(i+1)
          y3 = y(i+2)
          x12 =  x1 - x2
          x21 = -x12
          x13 =  x1 - x3
          x31 = -x13
          x23 =  x2 - x3
          x32 = -x23
          d(1) = ( x12 + x13 ) * y1 / x12 / x13 + x13 * y2 / x21 / x23
     $      + x12 * y3 / x31 / x32
	else if ( i .eq. n ) then
	  x1 = x(i-2)
          x2 = x(i-1)
          x3 = x(i)
          y1 = y(i-2)
          y2 = y(i-1)
          y3 = y(i)
          x12 =  x1 - x2
          x21 = -x12
          x13 =  x1 - x3
          x31 = -x13
          x23 =  x2 - x3
          x32 = -x23
          d(n) = x32 * y1 / x12 / x13 + x31 * y2 / x21 / x23 + 
     $      ( x31 + x32 ) * y3 / x31 / x32
	else
	  x1 = x(i-1)
          x2 = x(i)
          x3 = x(i+1)
          y1 = y(i-1)
          y2 = y(i)
          y3 = y(i+1)
          x12 =  x1 - x2
          x21 = -x12
          x13 =  x1 - x3
          x31 = -x13
          x23 =  x2 - x3
          x32 = -x23
          d(i) = y1 * x23 / x12 / x13 + ( x21 + x23 ) * y2 / x21 / x23 
     $	    + x21 * y3 / x31 / x32
	endif
      enddo

      return
      end

c-------------------------------------------------------------------------------
c routine: dlag3p_deriv
c descrpt: double precision version of lag3p_deriv
c
c changes:
c
c   4/4/06 added routine to the library - wm
c
c-------------------------------------------------------------------------------
c inputs: 
c
c   n = number of points in tabulated data
c   x = x-axis of tabulated data
c   y = input data
c
c-------------------------------------------------------------------------------
c outputs: 
c
c   d = calculated dy/dx by differentiating a 3-rd order lagrange 
c       polynomial
c
c-------------------------------------------------------------------------------

      subroutine dlag3p_deriv( n, x, y, d )

      implicit none

      integer*4 n, i
      real*8    x(*), y(*), d(*)

      real*8    x1, x2, x3, y1, y2, y3
      real*8    x12, x13, x23, x21, x31, x32

      ! do interior points

      do i = 1, n
	if ( i .eq. 1) then
	  x1 = x(i)
          x2 = x(i+1)
          x3 = x(i+2)
          y1 = y(i)
          y2 = y(i+1)
          y3 = y(i+2)
          x12 =  x1 - x2
          x21 = -x12
          x13 =  x1 - x3
          x31 = -x13
          x23 =  x2 - x3
          x32 = -x23
          d(1) = ( x12 + x13 ) * y1 / x12 / x13 +  x13 * y2 / x21 / x23 
     $      + x12 * y3 / x31 / x32
	else if ( i .eq. n ) then
	  x1 = x(i-2)
          x2 = x(i-1)
          x3 = x(i)
          y1 = y(i-2)
          y2 = y(i-1)
          y3 = y(i)
          x12 = x1 - x2
          x21 = -x12
          x13 = x1 - x3
          x31 = -x13
          x23 = x2 - x3
          x32 = -x23
          d(n) = x32 * y1 / x12 / x13 + x31 * y2 / x21 / x23 + 
     $      ( x31 + x32 ) * y3 / x31 / x32
	else
	  x1 = x(i-1)
          x2 = x(i)
          x3 = x(i+1)
          y1 = y(i-1)
          y2 = y(i)
          y3 = y(i+1)
          x12 = x1 - x2
          x21 = -x12
          x13 = x1 - x3
          x31 = -x13
          x23 = x2 - x3
          x32 = -x23
          d(i) = y1 * x23 / x12 / x13 + ( x21 + x23 ) * y2 / x21 / x23 
     $	    + x21 * y3 / x31 / x32
	endif
      enddo

      return
      end

	
	


	block data mse2_bd
      include 'mse_lib2.inc'
      data n_spave_vars /9/, n_u /5/, n_w /3/
      data use_cer/0/, certree/cerquick/,use_cer330/0/, use_cer210/0/
      data use_fwtgam/0/
      data beam_v_on_level/79.0e3,79.0e3,58.0e3,58.0e3/
      data msetup_filename/'hack'/


      end


      subroutine mse_fastEr_correction(ishot,ntimes,
     $         raxis,times,dt,correct,error,tan_gamma_std)
      use qsort_c_module
      implicit none
      include "mdslib.inc"
      include "mse_lib2.inc"

      integer*4 ishot,ntimes
      real*4 raxis(ntimes),times(ntimes)
      real*4    tan_gamma_std( ntimes, max_mse_chans )
      real*4    dt,ldt
      integer*4 error,status,length,stat,worklen
      integer*4 sock
      integer*4 ncode
      character*10 cerbranch
      character*512 cmd
      integer*4 cerchans,cerpts
      real, allocatable:: signal(:),time(:),view_phi(:),stimes(:)
      real, allocatable:: cr(:),rotc(:),rot_err(:)
      integer*4, allocatable:: childnodes(:),npts(:),work(:)
      character*11,allocatable:: parents(:)
      character*80 node
      real t(4096)
      integer*4 i,j,k,count,goodtimes(ntimes)
      real far_cer



      real*4    correct(ntimes,max_mse_chans) 
      real*4 cer_r(ntimes,max_cer_chans+1)
      real*4 cer_rotc(ntimes,max_cer_chans+1)
      real*4 rwork(max_cer_chans+1)
      integer*4 ind(max_cer_chans+1)
      real*4    eval_linear
      real*4    r_copy(max_sightlines)
      real*4 A1,A5,mrotc(max_cer_chans+1)
      real*4 mach_eps
      real*4 dbydt
      integer*8 mdssize
      integer qfirst


      if ( qlevel .ge. 1 ) print *, '(mse_lib2:mse_fastEr_correction)'

      qfirst = 0
      far_cer = 10.0
      ldt = dt / 2.0
      mach_eps = EPSILON(ldt)
      correct(:,:) = 1.0
      cer_r(:,:) = far_cer
      cer_rotc(:,:) = 0.0
      goodtimes(:) = 0
      error = 0
      ! connect to server
        sock = MdsConnect('atlas.gat.com'//char(0))
        if ( sock .eq. -1) then
           print *,"Couldn\'t open mdsplus server"
           error = -1
           return
        endif

      ! open the CER tree
        status = MdsOpen('IONS'//char(0),ishot)
        if ( mod(status,2) .eq. 0) then
           print *,"Couldn\'t open mdsplus tree:",status
           error = -1
           return
        endif


       ! 
        select case (certree)
	   case (cerquick)
             cerbranch = "CERQUICK"
	   case (cerneur)
             cerbranch = "CERNEUR"
	   case (cerfit)
             cerbranch = "CERFIT"
	   case (cerauto)
             cerbranch = "CERAUTO"
        end select
        if ( qlevel .ge. 1 ) then
              print *,'           cerbranch    ',      cerbranch
        endif
	      
        status = MdsSetDefault("CER."//trim(cerbranch)//
     $                 ".TANGENTIAL"//char(0))
        if ( qlevel .ge. 8 ) then
              print *,'           mdssetdefault    ',      status
        endif
        if ( mod(status,2) .eq. 0) then
           print *,"Couldn\'t set default to",cerbranch," :",status
           error = -1
           return
        endif
      
       
        cmd='size(getnci(".CHANNEL*:TIME","NID_NUMBER"))'
        status = MdsValue(trim(cmd)//char(0),
     $         descr(IDTYPE_LONG, cerchans,0),0,mdssize)
        if ( qlevel .ge. 8 ) then
              print *,'           getnumchans    ',  cerchans,    status
        endif
        if ( mod(status,2) .eq. 0) then
           print *,"Couldn\'t get number of channels: ",status
           error = -1
           return
        endif

	if (cerchans .gt. max_cer_chans) then
	      print *,'More cerchans than compiled-in maximum: ',
     $            cerchans, ' > ' , max_cer_chans
	      cerchans = max_cer_chans
	endif

        if(allocated(childnodes)) deallocate(childnodes)
        allocate(childnodes(1:cerchans),stat=status)
        if ( qlevel .ge. 8 ) then
              print *,'           allocate childnodes  ', 
     $               cerchans, status
        endif
        if (status .ne. 0) then
            print *,"Could not allocate memory for cer childnodes"
            error = -1
            return
        endif

        cmd='getnci(".CHANNEL*:TIME","NID_NUMBER")'
        status = MdsValue(trim(cmd)//char(0),
     $        descr(IDTYPE_LONG, childnodes,cerchans,0),0,mdssize)
        if ( qlevel .ge. 8 ) then
              print *,'           childnodes nid numbers    ',  status
        endif
        if ( mod(status,2) .eq. 0) then
           print *,"Couldn\'t get childnodes nid numbers: ",status
           error = -1
           return
        endif

        if(allocated(parents)) deallocate(parents)
        allocate(parents(1:cerchans),stat=status)
        if (status .ne. 0) then
            print *,"Could not allocate memory for cer parents"
            error = -1
            return
        endif

        cmd='getnci(getnci($,"PARENT"),"NODE_NAME")'
        status = MdsValue(trim(cmd)//char(0),
     $          descr(IDTYPE_LONG,childnodes,cerchans,0),
     $          descr(IDTYPE_CSTRING, parents,cerchans,0,11),
     $          0,mdssize)
        if ( mod(status,2) .eq. 0) then
           print *,"Couldn\'t get parent node names: ",status
           error = -1
           return
        endif

        if(allocated(npts)) deallocate(npts)
        allocate(npts(1:cerchans),stat=status)
        if (status .ne. 0) then
            print *,"Could not allocate memory for cer npts"
            error = -1
            return
        endif

        cer_r(:,cerchans+1) = 2.36
        cer_rotc(:,cerchans+1) = 0
	do i = 1, cerchans

          if ( qlevel .ge. 8 ) then
              print *,'           process cer chan     ', i
          endif

          node = trim(parents(i)) // ":TIME"
          cmd='size('//trim(node)//')'
          status = MdsValue(trim(cmd)//char(0),
     $            descr(IDTYPE_LONG, cerpts,0),0,mdssize)
          if ( qlevel .ge. 8 ) then
              print *,'        read size of time arrays ', 
     $                node,cerpts,status
          endif
          if ( mod(status,2) .eq. 0) then
	     ! this error is fairly normal when data not available
	     ! don't report
             if ( qlevel .ge. 8 ) then
               print *,"Couldn\'t read size of time arrays: ",status
               print *,"Cmd: ",cmd
             endif
             error = -1
             npts(i) = 0
             goto 10
          endif
          npts(i) = cerpts

           if (npts(i)  .eq. 0) then
	        print *,'npts is zero'
		goto 10
           endif

           worklen = max(npts(i),max_mse_chans)
           if(allocated(work)) deallocate(work)
           allocate(work(1:worklen),stat=status)
           if ( qlevel .ge. 8 ) then
              print *,'           cer work size    ',      npts(i)
           endif
           if (status .ne. 0) then
               print *,"Could not allocate memory for cer work array"
               error = -1
               goto 10
           endif

          if(allocated(view_phi)) deallocate(view_phi)
          allocate(view_phi(1:npts(i)),stat=status)
          if (status .ne. 0) then
              print *,"Could not allocate memory for cer view_phi"
              error = -1
              goto 10
          endif



          node = trim(parents(i)) // ':VIEW_PHI'
          status = MdsValue(trim(node)//char(0),
     $            descr(IDTYPE_FLOAT, view_phi,npts(i),0),0,mdssize)
          if ( mod(status,2) .eq. 0) then
             print *,"Couldn\'t get view_phi for node: ",node," : ",status
             error = -1
             goto 10
          endif

	  if ( view_phi(1) .gt. 10.0 
     $         .and. view_phi(1) .lt. 25.0) then
              continue
	  else      
	      if ( (use_cer330 .ne. 0) .and. (view_phi(1) .gt. 310.0 
     $         .and. view_phi(1) .lt. 330.0)) then
                   continue
              else
	         if ( (use_cer210 .ne. 0) .and. (view_phi(1) .gt. 210.0 
     $            .and. view_phi(1) .lt. 230.0)) then
                    continue
                 else
		    goto 10
                 endif
              endif   
          endif

          if(allocated(time)) deallocate(time)
          allocate(time(1:npts(i)),stat=status)
          if (status .ne. 0) then
              print *,"Could not allocate memory for cer time"
              error = -1
              goto 10
          endif
          node = trim(parents(i)) // ':TIME'
          status = MdsValue(trim(node)//char(0),
     $            descr(IDTYPE_FLOAT, time,npts(i),0),0,mdssize)
          if ( mod(status,2) .eq. 0) then
             print *,"Couldn\'t get time for node: ",node," : ",status
             error = -1
             goto 10
          endif

          if(allocated(stimes)) deallocate(stimes)
          allocate(stimes(1:npts(i)),stat=status)
          if (status .ne. 0) then
              print *,"Could not allocate memory for cer stimes"
              error = -1
              goto 10
          endif
          node = trim(parents(i)) // ':STIME'
          status = MdsValue(trim(node)//char(0),
     $            descr(IDTYPE_FLOAT, stimes,npts(i),0),0,mdssize)
          if ( mod(status,2) .eq. 0) then
             print *,"Couldn\'t get stimes for node: ",node," : ",status
             error = -1
             goto 10
          endif

          if(allocated(cr)) deallocate(cr)
          allocate(cr(1:npts(i)),stat=status)
          if (status .ne. 0) then
              print *,"Could not allocate memory for cer r"
              error = -1
              goto 10
          endif
          node = trim(parents(i)) // ':R'
          status = MdsValue(trim(node)//char(0),
     $            descr(IDTYPE_FLOAT, cr,npts(i),0),0,mdssize)
          if ( mod(status,2) .eq. 0) then
             print *,"Couldn\'t get r for node: ",node," : ",status
             error = -1
             goto 10
          endif

          if(allocated(rotc)) deallocate(rotc)
          allocate(rotc(1:npts(i)),stat=status)
          if (status .ne. 0) then
              print *,"Could not allocate memory for cer rotc"
              error = -1
              goto 10
          endif
          node = trim(parents(i)) // ':ROTC'
          status = MdsValue(trim(node)//char(0),
     $            descr(IDTYPE_FLOAT, rotc,npts(i),0),0,mdssize)
          if ( mod(status,2) .eq. 0) then
             print *,"Couldn\'t get rotc for node: ",node," : ",status
             error = -1
             goto 10
          endif

          if(allocated(rot_err)) deallocate(rot_err)
          allocate(rot_err(1:npts(i)),stat=status)
          if (status .ne. 0) then
              print *,"Could not allocate memory for cer rot_err"
              error = -1
              goto 10
          endif
          node = trim(parents(i)) // ':ROT_ERR'
          status = MdsValue(trim(node)//char(0),
     $            descr(IDTYPE_FLOAT, rot_err,npts(i),0),0,mdssize)
          if ( mod(status,2) .eq. 0) then
             print *,"Couldn\'t get rot_err for node: ",node," : ",
     $             	     status
             error = -1
             goto 10
          endif

          do j = 1,ntimes
	     call rwhere(npts(i),(time .ge. 
     $	          ((times(j)-ldt-mach_eps)*1000.0) .and.
     $            (time + stimes) .le. ((times(j)+ldt+mach_eps)*1000.0))
     $              ,count,work)
	      if (count .eq. 0 ) then
	          if(mse_strict .eq. 0)  then
	             call rwhere(npts(i),(time .ge. times(j)*1000.0)
     $                         ,count,work)
                     if(count .gt. 0 .and. work(1) .ne. 0 .and.
     $	                  (max_t_beam_off .eq. 0.0 .or. 
     $                     (max_t_beam_off) .le. 
     $                    (time(work(1)) - time(work(1)-1)))) then
                      ! linear interpolation
		         dbydt = (cr(work(1)) - cr(work(1)-1)) /
     $                           (time(work(1)) - time(work(1)-1))
                         cer_r(j,i) = (times(j)*1000.0
     $                                       -time(work(1)-1))
     $                                   * dbydt + cr(work(1)-1)
		         dbydt = (rotc(work(1)) - rotc(work(1)-1)) /
     $                           (time(work(1)) - time(work(1)-1))
                         cer_rotc(j,i) = (times(j)*1000.
     $                                       -time(work(1)-1))
     $                           * dbydt + rotc(work(1)-1)
                       if ( qlevel .ge. 3 ) then
		         print *,'interpolate cer',i,cer_r(j,i),
     $                            cer_rotc(j,i)
		         print *, time(work(1)),time(work(1)-1)
		         print *, cr(work(1)),cr(work(1)-1)
		         print *, rotc(work(1)),rotc(work(1)-1)
                       endif
                       goodtimes(j) = 1
	   		 

                     else
                       if ( qlevel .ge. 3 ) then
                         if(count .gt. 0) then
		            print *,"couldn't interpolate cer",i,cer_r(j,i),
     $                            cer_rotc(j,i),work(1)
		            print *, time(work(1)),time(work(1)-1)
		            print *, cr(work(1)),cr(work(1)-1)
		            print *, rotc(work(1)),rotc(work(1)-1)
                         else
                            print *,"Count,npts is ",count,npts(i)
                            print *,"max(time), sample time ",
     $                              maxval(time), times(j)*1000.0
                         endif
                       endif
	                cer_r(j,i) = far_cer
		        cer_rotc(j,i) = -1.e30
                     endif
	          else
		     ! mse strict set => no cer interpolation allowed
                       if ( qlevel .ge. 3 ) then
		         print *,"couldn't interpolate cer",i,cer_r(j,i),
     $                            cer_rotc(j,i)
                       endif
	             cer_r(j,i) = far_cer
		     cer_rotc(j,i) = 0
	          endif
	      else if (count .eq. 1) then
                     goodtimes(j) = 1
	             cer_r(j,i) = cr(work(1))
	             cer_rotc(j,i) = rotc(work(1))
                   if ( qlevel .ge. 3 ) then
		     print *,'one cer value',cer_r(j,i),
     $                            cer_rotc(j,i)
                   endif
              else if(count .gt. 1) then
	             cer_r(j,i) = sum(cr(work(1:count)))/count
	             cer_rotc(j,i) = sum(rotc(work(1:count)))/count
                     goodtimes(j) = 1
                   if ( qlevel .ge. 3 ) then
                     print *,'count is ',count
		     print *,time(work(1:count))
		     print *,rotc(work(1:count))
		     print *,'average value',i,cer_r(j,i),
     $                            cer_rotc(j,i)
                   endif
              else
                  if ( qlevel .ge. 3 ) then
                     print *,"No cer data in range"
                     print *,(times(j)-ldt-mach_eps)*1000.0,"to"
                     print *,((times(j)+ldt+mach_eps)*1000.0)
                     print *,"stimes",minval(stimes),maxval(stimes)
                  endif
              endif

	  enddo
          

10        continue

	enddo
        do j = 1,ntimes
            if(goodtimes(j) .gt. 0) then
	    r_copy = r
	    call rwhere(max_mse_chans,
     $  	    ((r .lt. minval(cer_r(j,:)))
     $	      .and. (r .lt. raxis(j))),count,work)
            if ( goodtimes(j) .gt. 0 .and. count .gt. 0) then
               r_copy(:) = r(:)
               r_copy(work(1:count)) = r(work(1:count)) + 
     $	            2.*(raxis(j)-r(work(1:count)))
	       rwork(:) = cer_r(j,:)
	       ind(:) = (/ (k,k=1,(max_cer_chans+1)) /)
	       call QsortC(ind,rwork)
               cer_r(j,:) = rwork(ind)
	       rwork(:) = cer_rotc(j,ind)
               cer_rotc(j,:) = rwork(:)
	       do k=1,n_mse_chans
                  mrotc(k) = 1000. * eval_linear( cerchans,
     $                r_copy(k),cer_r(j,:),cer_rotc(j,:))
               enddo
                mrotc(work(1:count)) = mrotc(work(1:count)) *
     $            r_copy(work(1:count)) / r(work(1:count))
	       do k=1,n_mse_chans
                   A1 = a_coefs_mrz(k,1)
                   A5 = a_coefs_mrz(k,5)
		   correct(j,k)=1.0 / (1. - A5*mrotc(k)/A1)
               enddo
	    else
               if ( qfirst .eq. 0 ) then
                   print *,"Can't do cer correction for ",times(j)
                   print *,"set quiet>0 for more msgs"
                   qfirst = 1
               else if ( qlevel .ge. 1 ) then
                   print *,"Can't do cer correction for ",times(j)
                   print *,"goodtimes(j) ",goodtimes(j)
                   print *,"count  ",count
                   print *,"set tan_gamma_std to ",mse_badchannel_std
                   print *,"min  cer: ",minval(cer_r(j,:)),
     $             " raxis(j): ",raxis(j)
               endif
	       correct(j,:) = 1.0
               tan_gamma_std(j,1:max_mse_chans) = mse_badchannel_std
            endif
            else
               if (qlevel .eq. 1) then
                 print *,'no cer data at time ',times(j)
               endif
               tan_gamma_std(j,1:max_mse_chans) = mse_badchannel_std
            endif
         enddo


      end
      

      subroutine rwhere (num, mask, count, out)
      integer*4 num,count
      logical   mask(num)
      integer*4 out(num)


      integer*4 i,j



      count = 0
      do i=1,num 
         if (mask(i)) then
	    count = count + 1
	    out(count) = i
	 endif
      enddo
      return
      end


      subroutine mse_hello_world_f(msg)
      integer*2 msg(64,2)

      character*128 fmsg(2)
      integer*2 imsg(64,2)
      equivalence(fmsg,imsg)

      fmsg(1) = "hello world"
      fmsg(2) = "hello world2"
      msg(:,1) = imsg(:,1)
      msg(:,2) = imsg(:,2)
      return
      end


      
      subroutine mse_hack()

      implicit none
      include   'mse_lib2.inc'

      print *,'MSE Hack'
      print *,msetup_filename
      return
      end


      



    
