pro filter_scan, shot, x, y, error, arg=arg, title=title, xunits=xunits, yunits=yunits,    $
  zunits=zunits, runid=runid

  ical = 12

  ; Channel in first slot is recored by channel in second slot

  chan_filter_pairs = [ [ '41', '47a' ], [ '42', '49a' ], [ '43', '51a' ], [ '44', '53a' ],   $
       [ '46', '41a' ], [ '47', '42a' ], [ '48', '43a' ], [ '49', '44a' ], [ '50', '41b' ],   $
       [ '51', '42b' ], [ '52', '43b' ], [ '53', '44b' ], [ '54', '62a' ], [ '55', '63a' ],   $
       [ '56', '64a' ], [ '57', '65a' ], [ '58', '66a' ], [ '59', '67a' ], [ '60', '68a' ],   $
       [ '61', '69a' ], [ '62', '54a' ], [ '63', '55a' ], [ '64', '56a' ], [ '65', '57a' ],   $
       [ '66', '58a' ], [ '67', '58a' ], [ '68', '60a' ], [ '69', '61a' ] ]

  chan = arg[0]
  chan_str = strtrim( string( chan ), 2 )
  signal_name_a = 'ms' + chan_str + 'a'
  signal_name_b = 'ms' + chan_str + 'b'

 ; Determin whether there is a dedicated filter signal corresponding to the channel or if
 ; the channel needs to be mapped to another pointname

  if chan ge 41 then begin
    i = 0
    i_max = n_elements( chan_filter_pairs ) / 2 - 1
    while ( i le i_max  ) and ( chan_filter_pairs[0,i] ne chan_str ) do begin
      i = i + 1
    endwhile
    filter_name = 'ms' + chan_filter_pairs[1,i]
  endif else begin
    filter_name = 'msf' + chan_str
  endelse

  gadat2, signal_tb, signal_data_a, signal_name_a, shot, ical = ical
  gadat2, signal_tb, signal_data_b, signal_name_b, shot, ical = ical
  gadat2, filter_tb, filter_data, filter_name, shot, ical = ical

  ; Interpolate the filter waveform onto the mse timebase. Also smooth the data if the
  ; stepper/encoders are used to scan the filter

  if chan ge 46 then begin
    filter_data = smooth( filter_data, 10 )
    signal_data_a = smooth( signal_data_a, 10 )
    signal_data_b = smooth( signal_data_b, 10 )
  endif

  filter_position = interpol( filter_data, filter_tb, signal_tb )

  ; Plot the filter scan
  

;  y_max = max( [ signal_data_a, signal_data_b ], min = y_min )
;  plot, filter_position, signal_data_a, yrange = [ y_min, y_max ], color = 1000
;  oplot, filter_position, signal_data_b, color = 256000
;  xyouts, 0.1, 0.925, 'Channel ' + signal_name_a, color = 1000, /normal
;  xyouts, 0.1, 0.900, 'Channel ' + signal_name_b, color = 256000, /normal

  x = filter_position
  y = signal_data_a

  return

end
