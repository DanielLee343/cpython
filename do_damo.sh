#!/bin/bash
# cxl, base
# DAMON=$HOME/damo/damo
DAMON=damo
env=$1
# which workload
func=$2
# bm (bare metal), faas, add later
# local wl_args=$3
echo "running $func in $env"
if [ "$env" = "cxl" ]; then
    cmd_prefix="numactl --cpunodebind 0 --membind 1 -- "
elif [ "$env" = "base" ]; then
    cmd_prefix="numactl --cpunodebind 0 --membind 0 -- "
elif [ "$env" = "cpu0" ]; then
    cmd_prefix="numactl --cpunodebind 0 -- "
elif [ "$env" = "interleave" ]; then
    cmd_prefix="numactl --cpunodebind 0 --interleave all -- "
elif [ "$env" = "org" ]; then
    cmd_prefix=""
else
    echo "wrong env, pls try again!"; exit 1
fi
echo 1 | sudo tee /proc/sys/vm/drop_caches

cd $HOME/workspace/py_track
$cmd_prefix /home/lyuze/workspace/cpython/python ./test_trace_thread.py $func 2>/dev/null &
check_pid=$!
echo "workload pid is" $check_pid
sudo $DAMON record -s 1000 -a 100000 -u 1000000 -n 5000 -m 6000 -o $HOME/workspace/obj_heats/"$func".data $check_pid

echo "post processing..."

# generate obj heatmap
# python3 /home/lyuze/workspace/py_track/process_heats.py > /dev/null
# gnuplot /home/lyuze/workspace/py_track/plot.gnuplot

# sudo $DAMON report heats -i $HOME/workspace/obj_heats/"$func".data --resol 1000 2000 \
#     --abs_addr --heatmap $HOME/workspace/obj_heats/"$func".png &

# sudo damo report heats -i $HOME/workspace/obj_heats/matmul_list.data --resol 1000 5000 \
#     --abs_addr --address_range 140737326235824  140737352876912 \
#     --heatmap $HOME/workspace/obj_heats/test.png

# sudo $DAMON report heats -i $HOME/workspace/obj_heats/"$func".data --resol 1000 3000 \
#     --abs_addr > $HOME/workspace/obj_heats/"$func"_abs_addr.txt

# sudo $DAMON report heats -i $HOME/workspace/obj_heats/matmul_list.data --resol 1000 2000 > $HOME/workspace/obj_heats/test.txt