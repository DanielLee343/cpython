import pandas as pd
import os

org_heats_dir = "/home/lyuze/cpython/obj_heats"
interested_file = "linpack_np_heats.txt"
org_heats_file = os.path.join(org_heats_dir, interested_file)
columns = ["timestamp", "addr", "inc", "dec"]
org_df = pd.read_csv(org_heats_file, sep="\t", header=None, names=columns)
org_df['addr'] = org_df['addr'].apply(lambda x: int(x, 16))
print(org_df)

# group the df by timestamp and sort the addr
org_df = org_df.sort_values(by=['timestamp', 'addr'])
print(org_df)
sorted_file = os.path.join(org_heats_dir, "heats_sorted.txt")
org_df.to_csv(sorted_file, sep='\t', index=False, header=False)

# Filter out rows where both inc and dec are zeros
filtered_df = org_df[(org_df["inc"] != 0) & (org_df["dec"] != 0)]
sorted_filtered_file = os.path.join(org_heats_dir, "heats_sorted_filtered.txt")
filtered_df.to_csv(sorted_filtered_file, sep='\t', index=False, header=False)

filtered_df_no_ones = filtered_df[(filtered_df["inc"] != 1) & (filtered_df["dec"] != 1)]
sorted_filtered_no_ones_file = os.path.join(org_heats_dir, "heats_sorted_no_ones_filtered.txt")
filtered_df_no_ones.to_csv(sorted_filtered_no_ones_file, sep='\t', index=False, header=False)

def get_time_addr_range(filtered_df):
    num_rows = filtered_df.shape[0]
    print("number of rows after filter:\n", num_rows)
    # setup time range
    time_start = filtered_df.iloc[0]['timestamp']
    time_end = filtered_df.iloc[-1]['timestamp']
    time_range = []
    time_range.append(0)
    time_range.append(round(time_end - time_start, 2))
    print(time_range)

    # setup addr range
    grouped = filtered_df.groupby('timestamp')
    for ts, group_df in grouped:
        start_addr = int(group_df.iloc[0]['addr'])
        print(start_addr)
        end_addr = int(group_df.iloc[-1]['addr'])
        print(end_addr)
        print()
        addr_range = []
        addr_range.append(start_addr)
        addr_range.append(end_addr)
    print(addr_range)

# def plot_range(orig_range, use_absolute_val=False):
#     plot_range = [x for x in orig_range]
#     if not use_absolute_val:
#         plot_range[0] -= orig_range[0]
#         plot_range[1] -= orig_range[0]
#     return plot_range

import subprocess
def plot_heatmap(data_file, output_file="/home/lyuze/cpython/heats.png"):
    terminal = output_file.split(".")[-1]
    if not terminal in ["pdf", "jpeg", "png", "svg"]:
        print("Unsupported plot output type.")
        exit(-1)
    x_range = c # relative time
    y_range = addr_range # absolute addr
    gnuplot_cmd = """
    set term %s;
    set output '%s';
    set key off;
    set xrange [%f:%f];
    set yrange [%f:%f];
    set xlabel 'Time (s)';
    set ylabel 'Address (MB)';
    plot '%s' using 1:2:3 with image;""" % (
        terminal,
        output_file,
        x_range[0],
        x_range[1],
        y_range[0],
        y_range[1],
        data_file,
    )
    command = ["gnuplot", "-e", gnuplot_cmd]
    dummy_cmd = ["echo", "1"]
    result = subprocess.run(dummy_cmd, stdout = subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    print("stdout:")
    print(result.stdout)

    print("stderr:")
    print(result.stderr)

# plot_heatmap(data_file)
# get_time_addr_range(filtered_df)