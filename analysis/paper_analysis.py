import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import argparse
import datetime


plt.rcParams["font.family"] = "Times New Roman"
plt.rcParams.update({'font.size': 16})
plt.rcParams["figure.figsize"] = (12,8)

# For CSV file with columns:
#module,ProducerName,switches,file,rank,flushes,record_id,max_byte,type,job_id,op,cnt,seg:off,seg:pt_sel,seg:dur,seg:len,seg:ndims,seg:reg_hslab,seg:irreg_hslab,seg:data_set,seg:npoints,seg:timestamp

class generate_heatmaps():

    def __init__(self, dataframe, output_path):

        self.df = dataframe
        self.out = output_path

    def temporal(self, normalized=True):
        
        luster10million = [255515, 255673,255674,255675,255669]
        luster5million = [255668, 255670,255671,255672, 255667]
        nfs10million = [255686, 255687,255688,255689,255685]
        nfs5million = [255677, 255678,255681,255684,255676]

        filesystem = "luster10million"

        df = self.df[self.df['len']>0].copy()
        df = self.df[self.df['job_id'].isin(luster10million)].copy()

        # df['rank_time'] = (df['timestamp'] - min(df['timestamp']))
        df['rank_time'] = df['time']
        df['rank_time'] = pd.cut(df['rank_time'], bins=40, precision=1, include_lowest=True)
        print(df['rank_time'])

        df['rank'] = df['rank'].astype(int)

        df = df.groupby(by=["rank"])["rank_time"].value_counts(normalize=normalized).reset_index()
        df = df.pivot(index="rank", columns="level_1", values="rank_time")

        fig, ax = plt.subplots(1,1)
        sns.heatmap(ax=ax, data=df, cmap="mako")
        ax.set_ylabel("Rank ID")
        ax.set_xlabel("Time interval")
        plt.xticks(fontsize=12)
        plt.tight_layout()
        plt.title("\"Open\" operations for the HACC application", loc='left')
        plt.title("Luster - 10 million particles/rank", loc='right', fontsize=13, color='grey', style='italic')

        plt.savefig(self.out + "/temporal_by_occurence_normalized" + str(normalized) + "-" + str(filesystem) + ".jpg")
        plt.clf()
        plt.close()

    def temporal_by_amount(self):

        df = self.df[self.df['seg:len']>0].copy()
        df['rank_time'] = self.df['seg:timestamp'] - min(self.df['seg:timestamp'])
        df['rank_time'] = pd.cut(df['rank_time'],bins=8)
        df = df.groupby(by=["op","rank_time"])["seg:len"].sum().reset_index()
        df = df.pivot(index="op", columns="rank_time", values="seg:len")
        
        fig, ax = plt.subplots(1,1)
        sns.heatmap(ax=ax, data=df, cmap="mako")
        ax.set_ylabel("operation")
        ax.set_xlabel("timestamp")
        plt.tight_layout()
        plt.savefig(self.out + "/temporal_by_amount.jpg")
        plt.clf()
        plt.close()
    
    def spatial_load_imbalance(self):

        df = self.df[self.df['seg:len']>0].copy()
        df = df.groupby(by=["rank","op"]).apply(lambda x: (max(x["seg:len"]))/max(x["cnt"])).reset_index()
        df = df.pivot(index="op", columns="rank", values=0)
        
        fig, ax = plt.subplots(1,1)
        sns.heatmap(ax=ax, data=df, cmap="mako", cbar_kws={'label': 'amount per operation'})
        ax.set_ylabel("Operation")
        ax.set_xlabel("Rank Number")
        plt.tight_layout()
        plt.savefig(self.out + "/spatial_load_balance.jpg")
        plt.clf()
        plt.close()

    def spatial_performance_amount(self):

        df = self.df[self.df['len']>0].copy()
        df = df.groupby(by=["module","ProducerName","rank","op", "job_id"]).apply(lambda x: (max(x["len"])/max(x["dur"]))/1000).reset_index()
        df = df.pivot(index="job_id", columns="rank", values=0)
        fig, ax = plt.subplots(1,1)
        
        sns.heatmap(ax=ax, data=df, cmap="mako", cbar_kws={'label': '[Bytes/miliseconds]'})
        ax.set_ylabel("Job ID")
        ax.set_xlabel("Rank")
        plt.tight_layout()
        plt.savefig(self.out + "/spatial_performance_amount_job_id.jpg")
        plt.clf()
        plt.close()

    def spatial_performance_occurence(self):

        df = self.df[self.df['seg:len']>0].copy()
        df["op"] = df["op"].replace(['writes_segment'], 'Writes')
        df = df.groupby(by=["#module","ProducerName","rank","op"]).apply(lambda x: max(x["seg:dur"])/max(x["cnt"])).reset_index()
        df = df.pivot(index="op", columns="rank", values=0)

        fig, ax = plt.subplots(1,1)
        sns.heatmap(ax=ax, data=df, cmap="mako", cbar_kws={'label': '[Mean time/operation]'})
        ax.set_ylabel("Operation")
        ax.set_xlabel("Rank Number")
        plt.tight_layout()
        plt.savefig(self.out + "/spatial_performance_occurence.jpg")
        plt.clf()
        plt.close()

    def execution_during_time(self):

        df = self.df[self.df['seg:len']>0].copy()
        df["op"] = df["op"].replace(['writes_segment'], 'Writes')
        df['end'] = pd.to_datetime(self.df['seg:timestamp'], unit='s') 
        df["start"] = df["end"]

        for i in df.index:
            df["start"][i] = df["end"][i] - datetime.timedelta(milliseconds=self.df['seg:dur'][i])
            print(df["end"][i], self.df['seg:dur'][i])
        
        print(df["start"])
        print(df["end"])

        fig, ax = plt.subplots(1, figsize=(16,6))
        plt.barh(pd.to_numeric(df["rank"]), df.start, left=df.end)
        ax.set_xlabel("Timestamp")
        ax.set_ylabel("Rank")
        # plt.tight_layout()
        plt.savefig(self.out + "/gantt_execution.jpg")
        # plt.clf()
        # plt.close()

if __name__=="__main__":

    parser = argparse.ArgumentParser(description = "Generate plots for CSV file.")
    parser.add_argument('-f', dest='file', type=str, help="CSV file with data.")
    parser.add_argument('-path', dest='output_path', type=str, help="Output path for plots.", default="./figures/")
    args = parser.parse_args()
    
    # Read data as dataframe and generate HEATMAP plots
    # df = pd.read_csv(args.file)
    df = pd.read_parquet(args.file, engine='pyarrow')
    plots = generate_heatmaps(df, args.output_path)

    # Temporal with no normalization
    plots.temporal()
    # # Temporal with normalization
    # plots.temporal(normalized=True)
    # # Temporal by amount
    # plots.temporal_by_amount()

    # # Spatial load imbalance
    # plots.spatial_load_imbalance()

    # # Spatial performance by amount
    plots.spatial_performance_amount()

    # # Spatial performance by occurence
    # plots.spatial_performance_occurence()

    # # Time plot using Gantt Chart
    # plots.execution_during_time()