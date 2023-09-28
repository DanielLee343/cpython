#!/bin/bash
# cxl, base
DAMON=$HOME/damo/damo
# DAMON=damo
env=$1
func=$2
do_bk=$3
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
PLAYGROUND_DIR=/home/lyuze/workspace/obj_heats
gen_rss()
{
    local check_pid=$1
    local workload_name=$2
    # local do_graph=$3
    RSS_FILE=$PLAYGROUND_DIR/"$workload_name"_rss.csv
    rm -rf $RSS_FILE
    # if [ "$do_graph" = true ]; then
    time=0
    # echo "start gen rss at "$(date)
    while [ -d "/proc/${check_pid}" ]
    do
        cur_rss=$(ps aux | grep $check_pid | awk '{print $6}' | head -n 1 | awk '{print $1/1024}' | bc)
        # time=$(echo "$time + 0.5" | bc)
        time=$(date -u '+%s.%N')
        echo $time","$cur_rss >> $RSS_FILE
        sleep 0.48
    done
    gnuplot -e "output_file='$PLAYGROUND_DIR/"$workload_name"_rss.png'; \
        input_file='$RSS_FILE'; \
        wl_title='$workload_name'" \
        $SCRIPT_DIR/plot_rss.gnuplot 
    echo "plot rss done"
}

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
if [ "$do_bk" = "" ]; then
    exit 1
fi
echo 1 | sudo tee /proc/sys/vm/drop_caches

cd $HOME/workspace/py_track
$cmd_prefix /home/lyuze/workspace/cpython/python ./test_trace_thread.py $func $do_bk &
check_pid=$!
echo "workload pid is" $check_pid
# gen_rss $check_pid "$func"
sudo $DAMON record -s 1000 -a 100000 -u 1000000 -o $HOME/workspace/obj_heats/"$func".data $check_pid

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