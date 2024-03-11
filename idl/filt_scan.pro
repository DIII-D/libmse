dfilt_scan, shot, ch

f_name = 'msf' + strtrim( string(ch) )
a_name = 'ms' + strtrim( string(ch) ) + 'a'
b_name = 'ms' + strtrim( string(ch) ) + 'b'

gadat2, tf, msf, 'f_name. shot
gadat2, tms, msa, a_name, shot
gadat2, tms, msb, b_name, shot


