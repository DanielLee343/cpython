#!/bin/bash
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
        time=$(echo "$time + 0.1" | bc)
        echo $time","$cur_rss >> $RSS_FILE
        sleep 0.1
    done
    gnuplot -e "output_file='$PLAYGROUND_DIR/"$workload_name"_rss.png'; \
        input_file='$RSS_FILE'; \
        wl_title='$workload_name'" \
        $SCRIPT_DIR/plot_rss.gnuplot 
    echo "plot rss done"
}

func=$1
do_bk=$2
echo 1 | sudo tee /proc/sys/vm/drop_caches

cd $HOME/workspace/py_track
$cmd_prefix /home/lyuze/workspace/cpython/python ./test_trace_thread.py $func $do_bk &
check_pid=$!
echo "workload pid is" $check_pid
gen_rss $check_pid $func