;---------------------------------------------------------------------------------------------
; Procedure: get_fast_mse
;   Retrieves all available or selected fast mse data for a specified shot
;---------------------------------------------------------------------------------------------
; Author: Mike Makowski
;---------------------------------------------------------------------------------------------
; Inputs:
;
;      shot = shot number 
;   ch_nums = keyword - array of integers containing the desired channels to retrieve,
;             If unspecified, all available channels are returned
;   get_pem = keyword - flag for retreiving PEM data. Set to any non-zero value
;             to retrieve all available PEM data. The default is to not retrieve
;             pem data
;---------------------------------------------------------------------------------------------
; Output:
;
; fast_data = returned structure containing the fast data along with
;             incidental information
;---------------------------------------------------------------------------------------------
; Revision history (dd-mm-yy):
;
;   23-03-05 - Wrote routine
;---------------------------------------------------------------------------------------------

pro get_fast_mse, shot, fast_data, ch_nums = ch_nums, get_pem = get_pem

  mdsconnect, 'msepc1.gat.com'
  mdsopen, 'msefaq', shot, status = status
  if ( status mod 2 eq 0 ) then begin
    fast_data = { error: 'No fast data for this shot' }
    return
  endif

  if ( shot gt 108800 ) then begin
    n_ch_max = 45
  endif else if ( shot gt 100500 ) then begin
    n_ch_max = 40
  endif else if ( shot gt  97400 ) then begin
    n_ch_max = 36
  endif else if ( shot lt  91300 ) then begin
    n_ch_max = 35
  endif else if ( shot lt  80540 ) then begin
    n_ch_max = 16
  endif else if ( shot lt  77000 ) then begin
    n_ch_max =  8
  endif else begin
    n_ch_max = 0
  endelse
  
  if ( n_ch_max gt 0 ) and not keyword_set(ch_nums) then begin
    channel_nums = indgen(n_ch_max) + 1
  endif else if keyword_set(ch_nums) then begin
    valid_ch_nums = where( ( 1 le ch_nums ) and ( ch_nums le n_ch_max ) )
    if valid_ch_nums[0] ne -1 then begin
      channel_nums = ch_nums( valid_ch_nums )
    endif else begin
      fast_data = { error: 'Selected channels do not exist for this shot' }
      return
    endelse
  endif else begin
    fast_data = { error: 'No data for this shot' }
    return
  endelse
  n_ch = n_elements( channel_nums )
  
  time_base = mdsvalue( '\msefaq::top:timebase', shot, status = status )
  if ( status mod 2 eq 0 ) then begin
    fast_data = { error: 'No data for this shot' }
    return
  endif
    
  n_pts = n_elements( time_base )
  if ( n_pts le 3 ) then begin
    fast_data = { error: 'No data for this shot' }
    return
  endif

  ; Fetch the pmt data

  y = fltarr( n_ch_max, n_pts )
  good_channels = intarr(n_ch)

  for i = 0, n_ch - 1 do begin
    i_ch = channel_nums[i]
    if ( (  1 le i_ch ) and ( i_ch le 11 ) ) or                                              $
       ( ( 37 le i_ch ) and ( i_ch le 45 ) ) then begin
      node_name = '\msefaq::top.v315.raw:ch_' + strtrim( string( i_ch ), 2 )
    endif else if ( 12 le i_ch ) and ( i_ch le 26 ) then begin
      node_name = '\msefaq::top.v045.raw:ch_' + strtrim( string( i_ch ), 2 )
    endif else if ( 27 le i_ch ) and ( i_ch le 36 ) then begin
      node_name = '\msefaq::top.v015.raw:ch_' + strtrim( string( i_ch ), 2 )
    endif
    u = mdsvalue( node_name, shot, status = status )
    if ( status mod 2 ne 0 ) then begin
      y[i_ch-1,*] = u
      good_channels[i] = i_ch
    endif
  endfor

  w_good_channels =  where( good_channels gt 0 )
  if ( w_good_channels[0] ne -1 ) then begin
    good_channels = good_channels( w_good_channels )
    n_good_chs = n_elements( good_channels )
  endif else begin
    fast_data = { error: 'No data found for this shot' }
    return    
  endelse

  ; Fetch the pem data

  if keyword_set(get_pem) then begin
    y_pems = fltarr( 6, n_pts )
    good_pems = strarr(6)
    view = [ 'v315', 'v045', 'v015' ]
    for i_view = 0, 2 do begin
      freq= [ '40', '46' ]
      for i_freq = 0, 1 do begin
        node_name = '\msefaq::top.' + view[i_view] + '.raw:pem_' + freq[i_freq]
        u = mdsvalue( node_name, shot, status = status )
        if ( status mod 2 ne 0 ) then begin
          i_sequence = 2 * i_view + i_freq
          y_pems[i_sequence,*] = u
          good_pems[i_sequence] = view[i_view] + '_pem_' + freq[i_freq]
        endif
      endfor
    endfor
    w_good_pems = where( good_pems ne '' )
    if w_good_pems[0] ne -1 then begin
      good_pems = good_pems( w_good_pems )
      n_good_pems = n_elements( good_pems )
    endif else begin
      n_good_pems = 0
    endelse
  endif

  ; Create and stuff the data structure

  if keyword_set(get_pem) then begin
    if n_good_pems eq 0 then begin
      fast_data = { shot: shot,                                                              $
                    ch: replicate( { ch_num: 0, waveform: fltarr(n_pts) }, n_good_chs ),     $
                    pems: 'No PEM data for this shot',                                       $
                    time_base: time_base,                                                    $
                    n_pts: n_pts,                                                            $
                    n_good_chs: n_good_chs,                                                  $
                    n_good_pems: 0,                                                          $
                    good_channels: good_channels }
    endif else begin
      fast_data = { shot: shot,                                                              $
                    ch: replicate( { ch_num: 0, waveform: fltarr(n_pts) }, n_good_chs ),     $
                    pems: replicate( { name: '', waveform: fltarr(n_pts) }, n_good_pems ),   $
                    time_base: time_base,                                                    $
                    n_pts: n_pts,                                                            $
                    n_good_chs: n_good_chs,                                                  $
                    n_good_pems: n_good_pems,                                                $
                    good_pems: good_pems,                                                    $
                    good_channels: good_channels }
      for k = 0, n_good_pems - 1 do begin
        fast_data.pems[k].name = good_pems[k]
        fast_data.pems[k].waveform = y_pems[w_good_pems[k],*]
      endfor
    endelse
  endif else begin
    fast_data = { shot: shot,                                                                $
                  ch: replicate( { ch_num: 0, waveform: fltarr(n_pts) }, n_good_chs ),       $
                  time_base: time_base,                                                      $
                  n_pts: n_pts,                                                              $
                  n_good_chs: n_good_chs,                                                    $
                  good_channels: good_channels }
  endelse

  for j = 0, n_good_chs - 1 do begin
    j_ch = good_channels[j] - 1
    fast_data.ch[j].ch_num = good_channels[j]
    fast_data.ch[j].waveform = y[j_ch,*]
  endfor

  return

end

