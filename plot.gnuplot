set terminal pngcairo
set key off
set title "Matmul Obj Access Scatter Plot"
set output 'dummy.png'
set xlabel 'Time (s)'
set ylabel 'Address (bytes)'
# plot "/home/lyuze/cpython/test/thread_module/data.txt" using 1:2:3 with image;
# set yrange [139905064568200.0:139905070834200.0]
plot "/home/lyuze/cpython/obj_heats/heats_sorted_no_ones_filtered.txt" using ($1 - 1999780.688818162):2 \
    with points pointtype 1 pointsize 0.8 \
    title "Data points";