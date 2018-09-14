
# coding: utf-8

# In[2]:


import math
import operator
import numpy as np

class KnnBase(object):
    def __init__(self, k, p, weights=None):
        self.k = k
        self.p = p
        self.weights = np.array(range(k+1, 1, -1))/sum(np.array(range(k+1, 1, -1)))

    def euclidean_distance(self, data_point1, data_point2):
        if len(data_point1) != len(data_point2) :
            raise ValueError('feature length not matching')
        else:
            distance = 0
            for x in range(len(data_point1)):
                distance += pow((data_point1[x] - data_point2[x]), 2)
            return math.sqrt(distance)
    def fit(self, train_feature, train_label):
        self.train_feature = train_feature
        self.train_label = train_label

    def get_neighbors(self, train_set, test_set, k):
        ''' return k closet neighbour of test_set in training set'''
        # calculate euclidean distance
        euc_distance = np.sqrt(np.sum((train_set - test_set)**2 , axis=1))
        # return the index of nearest neighbour
        return np.argsort(euc_distance)[0:k], np.sort(euc_distance)[0:k]


# In[10]:


class KnnClassifier(KnnBase):

    def predict(self, test_feature_data_point):
        # get the index of all nearest neighbouring data points
        nearest_data_point_index = self.get_neighbors(self.train_feature, test_feature_data_point, self.k)
        vote_counter = {}
        # to count votes for each class initialise all class with zero votes
        print('Nearest Data point index ', nearest_data_point_index)
        for label in set(self.train_label):
            vote_counter[label] = 0
        # add count to class that are present in the nearest neighbors data points
        for class_index in nearest_data_point_index:
            closest_lable = self.train_label[class_index]
            vote_counter[closest_lable] += 1
        print('Nearest data point count', vote_counter)
        # return the class that has most votes
        return max(vote_counter.items(), key = operator.itemgetter(1))[0]


# In[11]:


class KnnRegression(KnnBase):

    def predict(self, test_feature_data_point):
        nearest_data_point_index, nearest_data_point_dists = self.get_neighbors(self.train_feature, test_feature_data_point, self.k)
        total_val = 0.0
        dist_weights = np.power((1. / nearest_data_point_dists), self.p) / np.sum(np.power((1. / nearest_data_point_dists), self.p))  
        # calculate the sum of all the label values
#         for index in nearest_data_point_index:
#             total_val += self.train_label[index]
        for index, value in enumerate(nearest_data_point_index):
            total_val += dist_weights[index]*self.train_label[value]
        return total_val


# In[12]:


def get_rmse(y, y_pred):
    '''Root Mean Square Error
    https://en.wikipedia.org/wiki/Root-mean-square_deviation
    '''
    mse = np.mean((y - y_pred)**2)
    return np.sqrt(mse)

def get_mape(y, y_pred):
    '''Mean Absolute Percent Error
    https://en.wikipedia.org/wiki/Mean_absolute_percentage_error
    '''
    perc_err = (100*(y - y_pred))/y
    return np.mean(abs(perc_err))

def get_accuracy(y, y_pred):
    cnt = (y == y_pred).sum()
    return round(cnt/len(y), 2)

