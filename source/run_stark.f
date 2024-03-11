      program run_stark

      implicit none

      integer*4 max_mse_chans, max_times, n_t,ptdata_max_pts
      parameter (max_mse_chans = 69)
      parameter (max_times = 1024)
      parameter (n_t = 1)
      parameter (ptdata_max_pts = 128 * 1000)

      integer*4 shot, msefitfun, bksub, quiet
      integer*4 i_t, i_ch, lun, ierr, kerror(max_mse_chans)
      integer*4 status
      real*4 times(max_times), dt_full,pi
      real*4 tgamma( n_t, max_mse_chans )
      real*4 tgammacor( n_t, max_mse_chans )
      real*4 egamma( n_t, max_mse_chans )
      real*4 r(max_mse_chans), z(max_mse_chans) 
      real*4 a1(max_mse_chans), a2(max_mse_chans),
     $  a3(max_mse_chans), a4(max_mse_chans), a5(max_mse_chans),
     $  a6(max_mse_chans), a7(max_mse_chans)
      real*4 mse_time(ptdata_max_pts),mse_sindata(ptdata_max_pts)
      integer*4 mse_npts

      shot = 149695
      shot = 194562 ! short acq
      shot = 194233 ! long beam blips
      shot = 194212
      shot = 194719
      shot = 196262
      dt_full = 0.010
      msefitfun = 3
      bksub = 7
      quiet = 0
      pi = 4.0 * atan(1.0)

      times(1) = 1.9
      times(1) = 6.985
      times(1) = 6.100
      times(1) = 6.993
      times(1) = 2.0
      times(1) = 5.980
      times(1) = 1.56
c      do i_t = 1,n_t
c          times(i_t) = -0.055 + (i_t-1) * 0.01
c      enddo
      call set_mse_quiet(quiet)
      !call set_mse_beam_logic(0,0.0,0,0)
      call set_mse_beam_logic(0,1000.0,0,0)
      call read_getdat_data( shot, 'ms1a', 0.0, ptdata_max_pts,
     $  mse_time,mse_sindata, mse_npts, ierr )
      !call set_cer_correction(1,0,1,0)
c     call set_mse_beam_on_vlevel(78.0e3,0.0,0.0,0.0)
      call stark2cer( shot, times, n_t, dt_full, msefitfun, tgamma, 
     $  egamma, r, z, a1, a2, a3, a4, a5, a6, a7, kerror, bksub, 
     $  quiet ,tgammacor)

      lun = 6
c      open ( lun, file = 'tgamma_123202', status = 'replace', iostat =
c     $  ierr, err = 14 )
      write ( lun, '(1x,a16)' ) 'tgamma (degrees)'
      write ( lun, '(1x)' )
      write ( lun, '(1x,a3,256f10.3,256f10.3)' ) ' ch', ( times(i_t),
     $  i_t = 1, n_t ), ( times(i_t), i_t = 1, n_t ) 
      write ( lun, '(a120)' ) '----------------------------------------'
     $ // '------------------------------------------------------------'
     $ // '--------------------'
      do i_ch = 1, max_mse_chans
        write ( lun, '(1x,i3,256f10.6,256f10.6,256f10.6)' ) i_ch, ( 
     $	tgamma( i_t, i_ch ), i_t = 1, n_t ),(
     $	tgammacor( i_t, i_ch ), i_t = 1, n_t ),(
     $	egamma( i_t, i_ch ), i_t = 1, n_t )
      enddo
      close( lun )

 14   if ( ierr .ne. 0 ) then
        print *, 'ERROR opening file tgamma_123202, iostat = ', ierr
      endif

      stop
      end
