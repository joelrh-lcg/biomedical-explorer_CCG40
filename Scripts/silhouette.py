import numpy as np
import pandas as pd
from math import ceil
from sklearn.cluster import KMeans
from sklearn.metrics import silhouette_score, silhouette_samples
from scipy.sparse import csr_matrix
import joblib
#import argparse
from concurrent.futures import ProcessPoolExecutor
import time
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import sys
import datetime
import os

#class Logger(object):
#    "this class is for print the output of the script to both stdout and log file"
#    def __init__(self):
#        self.terminal = sys.stdout
#        self.log = open(os.path.join(args.plots, 'log.txt'), "w")
#
#    def write(self, message):
#        self.terminal.write(message)
#        self.log.write(message)
#
#    def flush(self):
#        #this flush method is needed for python 3 compatibility.
#        #this handles the flush command by doing nothing.
#        #you might want to specify some extra behavior here.
#        pass

def testK(k):
    labels = KMeans(n_clusters=k, init="k-means++", random_state=200).fit(tf_matrix).labels_
    score = silhouette_score(tf_matrix, labels, metric="euclidean", sample_size=1000,random_state=200)

    return score

def plotK(n_clusters, plots):
    fig, (ax1, ax2) = plt.subplots(1, 2)
    fig.set_size_inches(18, 7)
    # The 1st subplot is the silhouette plot
    # The silhouette coefficient can range from -1, 1 but in this example all
    # lie within [-0.1, 1]
    ax1.set_xlim([-0.1, 1])
    # The (n_clusters+1)*10 is for inserting blank space between silhouette
    # plots of individual clusters, to demarcate them clearly.
    ax1.set_ylim([0, tf_matrix.shape[0] + (n_clusters + 1) * 10])
    clusterer = KMeans(n_clusters=n_clusters, init="k-means++", random_state=200)
    cluster_labels = clusterer.fit_predict(tf_matrix)
    silhouette_avg = global_scores[n_clusters]
    # Compute the silhouette scores for each sample
    sample_silhouette_values = silhouette_samples(tf_matrix, cluster_labels)
    y_lower = 10

    for i in range(n_clusters):
        # Aggregate the silhouette scores for samples belonging to
        # cluster i, and sort them
        ith_cluster_silhouette_values = sample_silhouette_values[cluster_labels == i]
        ith_cluster_silhouette_values.sort()
        size_cluster_i = ith_cluster_silhouette_values.shape[0]
        y_upper = y_lower + size_cluster_i
        color = cm.nipy_spectral(float(i) / n_clusters)
        ax1.fill_betweenx(
            np.arange(y_lower, y_upper),
            0,
            ith_cluster_silhouette_values,
            facecolor=color,
            edgecolor=color,
            alpha=0.7,
        )
        # Label the silhouette plots with their cluster numbers at the middle
        ax1.text(-0.05, y_lower + 0.5 * size_cluster_i, str(i))
        # Compute the new y_lower for next plot
        y_lower = y_upper + 10  # 10 for the 0 samples

    ax1.set_title("The silhouette plot for the various clusters.")
    ax1.set_xlabel("The silhouette coefficient values")
    ax1.set_ylabel("Cluster label")
    # The vertical line for average silhouette score of all the values
    ax1.axvline(x=silhouette_avg, color="red", linestyle="--")
    ax1.set_yticks([])  # Clear the yaxis labels / ticks
    ax1.set_xticks([-0.1, 0, 0.2, 0.4, 0.6, 0.8, 1])
    # 2nd Plot showing the actual clusters formed
    colors = cm.nipy_spectral(cluster_labels.astype(float) / n_clusters)
    ax2.scatter(
        X[:, 0], X[:, 1], marker=".", s=30, lw=0, alpha=0.7, c=colors, edgecolor="k"
    )
    # Labeling the clusters
    centers = clusterer.cluster_centers_
    # Draw white circles at cluster centers
    ax2.scatter(
        centers[:, 0],
        centers[:, 1],
        marker="o",
        c="white",
        alpha=1,
        s=200,
        edgecolor="k",
    )

    for i, c in enumerate(centers):
        ax2.scatter(c[0], c[1], marker="$%d$" % i, alpha=1, s=50, edgecolor="k")

    ax2.set_title("The visualization of the clustered data.")
    ax2.set_xlabel("Feature space for the 1st feature")
    ax2.set_ylabel("Feature space for the 2nd feature")
    plt.suptitle(
        "Silhouette analysis for KMeans clustering on sample data with n_clusters = %d"
        % n_clusters,
        fontsize=14,
        fontweight="bold",
    )
    plt.savefig(os.path.join(plots, f"plot_k{n_clusters}.png"))

def eval_sil(searchm, lim_inf, lim_sup, file_matrix, plots, threads, iter = 10,  step = 1):

    #parser=argparse.ArgumentParser(description=
    #    'This script seeks the best number of K clusters based on the average score of the silhouette.')

    #parser.add_argument('-sm', '--search', dest='searchm',
    #    required=False, type=str, default="l", 
    #    help='Seach mode: linear for test all the range of K and bipartite for optimize the search.')  
    #parser.add_argument('--li', dest='lim_inf', required=False, type=int,
    #    default=2, help='Lower search range limit.')
    #parser.add_argument('--ls', dest='lim_sup', required=False, type=int,
    #    default=50,help='Upper search range limit.')
    #parser.add_argument('-s', '--step', dest='step', required=False, type=int, default=1,
    #    help='For linear mode this is normal step in range function')
    #parser.add_argument('-i', '--iter', dest='iter', required=False, type=int, default=10,
    #    help='For bipartite mode the max iterations to be calculated within the interval.')
    #parser.add_argument('-m', '--matrix', dest='file_matrix', required=True, type=str,
    #    help='Vectorization matrix file path (tsv).')
    #parser.add_argument('-p', '--plots', dest='plots', required=True, type=str,
    #    help='Path to save plot images and joblib dictionary with silouehette scores.')
    #parser.add_argument('-c', '--cores', dest='threads', required=False, type=int,
    #    default=1,help='Cores to parallel silhouette test.')
    #args=parser.parse_args()

    if not os.path.exists(plots): os.makedirs(plots)

    #sys.stdout = Logger()
    start_time = time.time()


    print("\n\n**********************************************************************\n")

    print("Run started at: ", datetime.datetime.now(), '\n')

    print("Running with input parameters:\n")
    print("Search mode: ", searchm)
    print("Lower limit: ", lim_inf)
    print("Upper limit: ", lim_sup)
    if searchm == "l": print("Step: ", step)
    elif searchm == "b": print("Max iter: ", iter)
    print("Vect matrix: ", file_matrix)
    print("Joblib dict output: ", os.path.join(plots, 'scores.joblib'))
    print(f"Running in {threads} threads...")


    print("\nReading matrix input...")
    global tf_matrix
    tf_matrix=[]
    with open(file_matrix, mode='r') as file:
        for line in file:
            tf_matrix.append(line.strip('\n\r').split("\t"))
    tf_matrix = csr_matrix(tf_matrix, dtype='double')
    print("Done!")

    print(f"Searching best K(clusters) for {tf_matrix.shape[0]} articles with {tf_matrix.shape[1]} features...")

    #lim_inf = args.lim_inf
    #lim_sup = args.lim_sup
    global global_scores
    global_scores = {}

    threads = int(threads)

    if searchm == "Linear":
        K = range(lim_inf, lim_sup+step, step)
        print(f"\nSearching in the interval between {K[0]} and {K[-1]}, with {len(K)} iterations...")
        with ProcessPoolExecutor(max_workers=threads) as executor:
            scores = list(executor.map(testK, K))
        for i in range(len(scores)): 
            print("Silhouette score for k(clusters) = " +str(K[i]) + " is " + str(scores[i]))
        max_k = K[max(range(len(scores)), key=scores.__getitem__)]
        print(f"\nMax average score found in k = {max_k}")
        for k, score in zip(K, scores): global_scores[k] = score
    elif searchm == "Binary":
        search = True
        while(search):
            step = ceil((lim_sup-lim_inf)/iter)
            K = range(lim_inf, lim_sup+step, step)
            print(f"\nSearching in the interval between {K[0]} and {K[-1]}, with {len(K)} iterations and step={step}...")
            with ProcessPoolExecutor(max_workers=threads) as executor:
                scores = list(executor.map(testK, K))
            for i in range(len(scores)): 
                print("Silhouette score for k(clusters) = " +str(K[i]) + " is " + str(scores[i]))
            for k, score in zip(K, scores): global_scores[k] = score
            if step > 1:
                max_k = K[max(range(len(scores)), key=scores.__getitem__)]
                print(f"\nMax average score found in k = {max_k}")
                lim_inf = max_k-ceil(step/2)
                if lim_inf <= 1: lim_inf = 2
                lim_sup=max_k+ceil(step/2)
                if(lim_inf <= 0): lim_inf = lim_inf - lim_inf + 2
            else: search = False
    else: raise("Select a valid search mode.")

    global_scores = dict(sorted(global_scores.items(), key=lambda item: item[1], reverse=True))
    print(f"\n\nMax silhouette average score found in k = {list(global_scores.keys())[0]}\n")

    joblib.dump(global_scores, os.path.join(plots, 'scores.joblib'))
    #df = pd.DataFrame.from_dict(global_scores)
    #df.to_csv(os.path.join(plots, 'scores.csv'), index = False, header= True)

    print("Calculated in %s seconds." % (time.time() - start_time), '\n')

    print("Plotting...")

    global_scores1 = dict(sorted(global_scores.items(), key=lambda item: item[0], reverse=True))
    plt.plot(list(global_scores1.keys()), list(global_scores1.values()))
    plt.savefig(os.path.join(plots, 'scores.png'))

    global X
    X = tf_matrix.toarray()

    with ProcessPoolExecutor(max_workers=threads) as executor:
        for k in list(global_scores.keys())[0:5]:
            executor.submit(plotK(k, plots))

    print("Total time: %s seconds." % (time.time() - start_time), '\n')

    print("\n**********************************************************************\n")

    return global_scores
