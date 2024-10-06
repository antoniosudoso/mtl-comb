import os
os.environ['TF_CPP_MIN_LOG_LEVEL'] = '2'
import numpy as np
import tensorflow as tf
from util import get_data, standardize_series
import argparse
import pandas as pd

###################### SET THESE PARAMETERS ACCORDINGLY ######################

BASE_PATH = '/home/antonio/Metalearning'
SERIES_TYPE = 'yearly'
MAX_LEN = 32
LAMBDA = 0.1

#############################################################################

if __name__ == '__main__':

    print('Reading ' + SERIES_TYPE + ' time series...')
    ts_train_list, ts_test_list, mat_forecast_list, div_label_list = get_data(BASE_PATH, SERIES_TYPE, period='test')
    print('\tDone!')
    print('Processing time series...')
    train_std_data = tuple(map(lambda x: standardize_series(x), ts_train_list))
    ts_train_list_std = list(map(lambda x: x[0], train_std_data))
    ts_test_list_std = np.array(ts_test_list)
    ts_train = tf.keras.preprocessing.sequence.pad_sequences(ts_train_list_std, maxlen=MAX_LEN, dtype='float64', padding="pre", truncating="pre", value=0.0)
    ts_train = ts_train.reshape((ts_train.shape[0], ts_train.shape[1], 1))
    ts_test = ts_test_list_std.copy()
    ts_mat_forecast = np.array(mat_forecast_list)
    if len(div_label_list) == 0:
        ts_div_label = np.zeros((ts_mat_forecast.shape[0],ts_mat_forecast.shape[1]))
    else:
        ts_div_label = np.array(div_label_list)
    print('\ttrain series shape:', ts_train.shape)
    print('\ttest series shape:', ts_test.shape)
    print('\tforecast matrix shape:', ts_mat_forecast.shape)
    print('\tdiversity label shape:', ts_div_label.shape)

    # print('Saving true values for the out-of-sample period')
    # df = pd.DataFrame(ts_test)
    # df.to_csv('./t_' + SERIES_TYPE + '.csv', index=False)
    # print('\tDone!')
    
    model_path = './saved_model_M4/' + SERIES_TYPE + '/main/f_model_' + str(LAMBDA)
    f_model = tf.keras.models.load_model(model_path, compile=False)
    f = f_model.predict([ts_train, ts_mat_forecast, ts_div_label])
    df = pd.DataFrame(f)
    df.to_csv('./saved_model_M4/' + SERIES_TYPE + '/f_' + SERIES_TYPE + '.csv', index=False)
    print('\tDone!')


