pro get_mse_configuration2, shot, config,quiet=quiet

  if ( keyword_set(quiet) eq 0 ) then quiet = 0L

  image = mse_shared_image()
  ptdata_max_pts = 131072L
  max_mse_channels = 0L
  routine = 'get_mse_max_chans_0'
  istat = call_external( image, routine, max_mse_channels )
  shot_num = long( shot )
  alpha = fltarr( max_mse_channels )
  omega = fltarr( max_mse_channels )
  theta = fltarr( max_mse_channels )
  phi_d = fltarr( max_mse_channels )
  r_mse = fltarr( max_mse_channels )
  x_mse = fltarr( max_mse_channels )
  y_mse = fltarr( max_mse_channels )
  z_mse = fltarr( max_mse_channels )
  phi_tor = fltarr( max_mse_channels )
  resolution =   fltarr( max_mse_channels )
  a_coefs =      fltarr( max_mse_channels, 7 )
  beam_index   = long( intarr( max_mse_channels ) )
  data_acq_sys = bytarr(8, max_mse_channels )
  mse_energy =   bytarr(8, max_mse_channels )
  mse_viewport = bytarr(8, max_mse_channels )
  xyz_lens =     fltarr( max_mse_channels, 3 )
  mcal1_gsso =   fltarr( max_mse_channels, 4 )
  mcal3_gpsd =   fltarr( max_mse_channels, 4 )
  wavelengths =  fltarr( max_mse_channels, 3 )
  n_chans = 0L
  first_bcoil = 0L
  mrz0_filename = bytarr(128)
  mcg1_filename = bytarr(128)
  mcg3_filename = bytarr(128)
  mcg5_filename = bytarr(128)
  msetup2_filename = bytarr(128)

  ; Define data structure

  mse_geom = { r: 0.0, x: 0.0, y: 0.0, z: 0.0, phi_tor: 0.0, ell: 0.0,         $
    alpha: 0.0, omega: 0.0, theta: 0.0, phi_d: 0.0 }
  mse_a_coefs = { a1: 0.0, a2: 0.0, a3: 0.0, a4: 0.0, a5: 0.0, a6: 0.0,        $
    a7: 0.0 }
  tan_slope = { gain: 0.0, slope: 0.0, bt_scale: 0.0, bt_offset: 0.0 }
  tan_offset = { gain: 0.0, phase: 0.0, dc_offset: 0.0, bt_scale: 0.0 }
  mse_calib_coefs = { tan_slope: tan_slope, tan_offset: tan_offset }
  wavelengths_3 = { full:0.0, half:0.0, third:0.0 }
  filter = { wavelength: 0.0, bandwidth: 0.0, thermal_coef: 0.0, op_temp: 0.0 }
  mse_chan = { view_port: '', energy: '', data_acq_sys: '', beam: 0L,          $
    resolution: 0.0, geom: mse_geom, a_coefs: mse_a_coefs,                     $
    calib: mse_calib_coefs, xyz_lens: fltarr(3), wavelengths: wavelengths_3,   $
    filter: filter }

  ; Call shared library

  routine = 'get_mse_configuration_0'
  istat = call_external( image, routine, shot_num, n_chans, alpha, omega, theta,   $
    phi_d, r_mse, x_mse, y_mse, z_mse, phi_tor, a_coefs, mse_viewport,         $
    mse_energy, data_acq_sys, xyz_lens, resolution, beam_index, mcal1_gsso,    $
    mcal3_gpsd, mrz0_filename, mcg1_filename, mcg3_filename, mcg5_filename,    $
    msetup2_filename, wavelengths, first_bcoil,quiet )

  mse_viewport = string(mse_viewport)
  mse_energy = string(mse_energy)
  data_acq_sys = string(data_acq_sys)
  mrz0_filename = string(mrz0_filename)
  mcg1_filename = string(mcg1_filename)
  mcg3_filename = string(mcg3_filename)
  mcg5_filename = string(mcg5_filename)
  msetup2_filename = string(msetup2_filename)

  ; Stuff the data structure with the return values

  config = { shot: shot_num, msetup_filename: msetup2_filename, mrz_filename:      $
    mrz0_filename, mcalgain_filename: mcg1_filename, mcalgain3_filename:       $
    mcg3_filename, n_chans: n_chans, channel: replicate( mse_chan, n_chans ) }



  for i = 0, n_chans - 1 do begin
    config.channel[i].geom.r = r_mse[i]
    config.channel[i].geom.x = x_mse[i]
    config.channel[i].geom.y = y_mse[i]
    config.channel[i].geom.z = z_mse[i]
    config.channel[i].geom.phi_tor = phi_tor[i]
    config.channel[i].geom.alpha = alpha[i]
    config.channel[i].geom.omega = omega[i]
    config.channel[i].geom.theta = theta[i]
    config.channel[i].geom.phi_d = phi_d[i]
    config.channel[i].a_coefs.a1 = a_coefs[i,0]
    config.channel[i].a_coefs.a2 = a_coefs[i,1]
    config.channel[i].a_coefs.a3 = a_coefs[i,2]
    config.channel[i].a_coefs.a4 = a_coefs[i,3]
    config.channel[i].a_coefs.a5 = a_coefs[i,4]
    config.channel[i].a_coefs.a6 = a_coefs[i,5]
    config.channel[i].a_coefs.a7 = a_coefs[i,6]
    config.channel[i].energy = mse_energy[i]
    config.channel[i].view_port = mse_viewport[i]
    config.channel[i].data_acq_sys = data_acq_sys[i]
    config.channel[i].beam = beam_index[i]
    config.channel[i].resolution = resolution[i]
    config.channel[i].xyz_lens = xyz_lens[i,*]
    config.channel[i].calib.tan_slope.gain = mcal1_gsso[i,0]
    config.channel[i].calib.tan_slope.slope = mcal1_gsso[i,1]
    config.channel[i].calib.tan_slope.bt_scale = mcal1_gsso[i,2]
    config.channel[i].calib.tan_slope.bt_offset = mcal1_gsso[i,3]
    config.channel[i].calib.tan_offset.gain = mcal3_gpsd[i,0]
    config.channel[i].calib.tan_offset.phase = mcal3_gpsd[i,1]
    config.channel[i].calib.tan_offset.bt_scale = mcal3_gpsd[i,2]
    config.channel[i].calib.tan_offset.dc_offset = mcal3_gpsd[i,3]
    config.channel[i].wavelengths.full = wavelengths[i,0]
    config.channel[i].wavelengths.half = wavelengths[i,1]
    config.channel[i].wavelengths.third = wavelengths[i,2]
  endfor

  return

end
