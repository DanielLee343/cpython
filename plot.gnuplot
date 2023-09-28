set terminal pngcairo
set key off
set title "Matmul Obj Access Scatter Plot"
set output '/home/lyuze/workspace/obj_heats/obj_heats_all.png'
set xlabel 'Time (s)'
set ylabel 'Address (bytes)'
# plot "/home/lyuze/cpython/test/thread_module/data.txt" using 1:2:3 with image;
set yrange [140737232904240.0:140737354133504.0]
plot "/home/lyuze/workspace/obj_heats/matmul_list.txt" using ($1):2 \
    with points pointtype 1 pointsize 0.8 \
    title "Data points";