set terminal pngcairo
set key off
set title "Matmul Obj Access Scatter Plot"
set output '/home/lyuze/workspace/obj_heats/obj_heats.png'
set xlabel 'Time (s)'
set ylabel 'Address (bytes)'
# plot "/home/lyuze/cpython/test/thread_module/data.txt" using 1:2:3 with image;
# set yrange [140737352853296.0:140737352876336.0]
plot "/home/lyuze/workspace/obj_heats/matmul_list_no_zeros_ones.txt" using ($1):2 \
    with points pointtype 1 pointsize 0.8 \
    title "Data points";