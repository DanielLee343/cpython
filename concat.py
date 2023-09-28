import pandas as pd

# Read the CSV file into a DataFrame
csv_file = pd.read_csv('/home/lyuze/workspace/obj_heats/matmul_list_rss.csv', sep=',', header=None, names=['time', 'mem'])

# Read the text file into another DataFrame
txt_file = pd.read_csv('/home/lyuze/workspace/obj_heats/obj_cnt.txt', sep='\t', header=None, names=['time', 'count'])

# max_length = max(len(csv_file), len(txt_file))
# csv_file['mem'] = csv_file['mem']._append(pd.Series([0] * (max_length - len(csv_file))), ignore_index=True)
# txt_file['count'] = txt_file['count']._append(pd.Series([0] * (max_length - len(txt_file))), ignore_index=True)

# Concatenate the two DataFrames horizontally along the columns axis
result_df = pd.concat([csv_file['mem'], txt_file['count']], axis=1)

print(result_df)

import matplotlib.pyplot as plt
plt.plot(result_df['mem'], result_df['count'], marker='o', linestyle='-')
plt.xlabel('mem')
plt.ylabel('count')
plt.title('Memory footprint VS obj count')
plt.savefig("mem_vs_cnt.png")