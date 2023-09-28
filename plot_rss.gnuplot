set datafile separator ','
set terminal png
set output output_file
set title wl_title
set xlabel "Time (s)"
set ylabel "RSS (MB)"
plot input_file \
    using 1:2 title '' with lines ls 1