import matplotlib.pyplot as plt
import numpy as np
import csv

def distribution_bool_list():
    with open('distribution_bool_list.data') as csvfile:
        reader = csv.reader(csvfile, delimiter=',')
        data = np.array([[ int(s) for s in row] for row in reader])[0]
    X = np.arange(len(data))
    plt.bar(X, data)
    plt.savefig('distribution_bool_list.png')
    plt.clf()

def time_nat_list():
    with open('time_nat_list_naive.data') as csvfile:
        reader = csv.reader(csvfile, delimiter=',')
        data_naive = np.array([[ float(s) for s in row] for row in reader])[0]

    with open('time_nat_list_store.data') as csvfile:
        reader = csv.reader(csvfile, delimiter=',')
        data_store = np.array([[ float(s) for s in row] for row in reader])[0]


    Xn = np.arange(1, len(data_naive)+1)
    Xs = np.arange(1, len(data_store)+1)
    plt.plot(Xn,data_naive, color='b', label='naive')
    plt.plot(Xs,data_store, color='r', label='store cardinal')
    plt.xlabel('Size of the generated data')
    plt.ylabel('Time in ms')
    plt.legend()
    plt.savefig('time_nat_list.png')
    plt.clf()

if __name__ == '__main__':
    distribution_bool_list()
    time_nat_list()
