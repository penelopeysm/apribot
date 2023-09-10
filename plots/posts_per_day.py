import pandas as pd
import matplotlib.pyplot as plt
import sqlite3
from dateutil import parser
import numpy as np
import seaborn as sns

sns.set_style("whitegrid")

con = sqlite3.connect("data/posts.db")
df = pd.read_sql_query("SELECT * from posts", con)
con.close()

df['time'] = df['time'].apply(lambda x: parser.parse(x).date())

counts = df.groupby(['time']).size()
# plt.plot(counts.index, counts.values, color="red", linewidth=1)
# plt.plot(counts.index, np.convolve(counts.values, np.ones(7)/7,
#                                    mode='same'), linewidth=2, linestyle="--",
#          color="black")
# plt.legend(['posts per day', '7-day moving average'])
# plt.title("/r/pokemontrades activity")
# plt.show()

for p in df.loc[df['time'] == parser.parse("2023-06-01").date(), "title"].values:
    print(p)
