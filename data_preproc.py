import numpy as np
import pandas as pd
import os

os.chdir('/home/mje/Projects/MEG_RP_behav/MEG_data/exports')

files = !ls *.csv


all_df = []
for j, file in enumerate(files):
    df = pd.read_csv(file, header=None, names=["value"])
    if file.find("plan") is -1:
        df["condition"] = "classic"
    else:
        df["condition"] = "plan"

    if file.find("-2_-2") > 0:
        df["time"] = "plan"
    elif file.find("-2_0") > 0:
        df["time"] = "early"
    elif file.find("0_0") > 0:
        df["time"] = "late"

    df["ROI"] = file.split("_")[2]
    df["id"] = np.arange(1, 13)
    all_df += [df]

    if j == 0:
        total_df = df
    else:
        total_df = pd.concat([total_df, df])

