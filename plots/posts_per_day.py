import os
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import numpy as np
import seaborn as sns
import psycopg2

sns.set_style("whitegrid")

con = psycopg2.connect(os.environ['FLY_PG_PROXY_CONN_STRING'])
cur = con.cursor()
cur.execute(
    "SELECT date(utc_time), count(utc_time) FROM posts GROUP BY date(utc_time) ORDER BY date(utc_time) DESC LIMIT 200 OFFSET 1;"
)
df = pd.DataFrame(cur.fetchall(), columns=['date', 'num_posts'])
cur.execute(
    "SELECT date(utc_time), count(utc_time) FROM posts WHERE hit GROUP BY date(utc_time) ORDER BY date(utc_time) DESC LIMIT 200 OFFSET 1;"
)
df2 = pd.DataFrame(cur.fetchall(), columns=['date', 'num_posts'])
con.close()

df['date'] = pd.to_datetime(df['date'])
df = df.sort_values(by='date')
print(df)

NDAYS_MOVING_AVG = 7
fig, ax = plt.subplots(figsize=(6, 3.5))

def get_moving_average(ydata, ndays):
    return np.convolve(ydata, np.ones(ndays)/ndays, mode='same')

# All posts
ax.plot(df['date'], df['num_posts'], color="red", linewidth=1)
# Aprimon posts
ax.plot(df2['date'], df2['num_posts'], color="blue", linewidth=1)
# Moving averages
ax.plot(df['date'][NDAYS_MOVING_AVG-1:-NDAYS_MOVING_AVG+1],
        get_moving_average(df['num_posts'], NDAYS_MOVING_AVG)[NDAYS_MOVING_AVG-1:-NDAYS_MOVING_AVG+1],
        linewidth=2, linestyle="--", color="black")
ax.plot(df2['date'][NDAYS_MOVING_AVG-1:-NDAYS_MOVING_AVG+1],
        get_moving_average(df2['num_posts'], NDAYS_MOVING_AVG)[NDAYS_MOVING_AVG-1:-NDAYS_MOVING_AVG+1],
        linewidth=2, linestyle="--", color="black")

ax.legend(['all posts', 'Aprimon posts', f'{NDAYS_MOVING_AVG}-day moving averages'])
ax.set(title="/r/pokemontrades activity", 
       xlabel="Date", ylabel="Number of posts")
ax.xaxis.set_major_formatter(mdates.DateFormatter('%d %b'))
for label in ax.get_xticklabels(which='major'):
    label.set(rotation=30, horizontalalignment='right')

plt.tight_layout()
# plt.savefig('/Users/pysm/Desktop/a.png', dpi=300)
plt.show()
