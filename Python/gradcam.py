import os

from matplotlib import pyplot as plt

os.environ['TF_CPP_MIN_LOG_LEVEL'] = '2'
import numpy as np
import tensorflow as tf
from util import get_data, standardize_series
import argparse
import pandas as pd

BASE_PATH = './'
SERIES_TYPE = 'quarterly'
MAX_LEN = 64
SERIES_ID = '...'


def grad_cam_heatmap(model, layer_name, data, pred_index):
    # First, we create a model that maps the input time series to the activations
    # of the last conv layer as well as the output predictions
    grad_model = tf.keras.models.Model([model.inputs], [model.get_layer(layer_name).output, model.output])

    # Then, we compute the gradient of the top predicted class for our input image
    # with respect to the activations of the last conv layer
    with tf.GradientTape() as tape:
        last_conv_layer_output, preds = grad_model(data)
        class_channel = preds[:, pred_index]

    # This is the gradient of the output neuron (top predicted or chosen)
    # with regard to the output feature map of the last conv layer
    grads = tape.gradient(class_channel, last_conv_layer_output)

    # This is a vector where each entry is the mean intensity of the gradient
    # over a specific feature map channel
    pooled_grads = tf.reduce_mean(grads, axis=0)

    # We multiply each channel in the feature map array
    # by "how important this channel is" with regard to the top predicted class
    # then sum all the channels to obtain the heatmap class activation
    last_conv_layer_output = last_conv_layer_output[0]
    heatmap = last_conv_layer_output * pooled_grads
    heatmap = tf.reduce_mean(heatmap, axis=1)
    # For visualization purpose, we will also normalize the heatmap between 0 & 1
    heatmap = tf.maximum(heatmap, 0) / tf.math.reduce_max(heatmap)
    heatmap = np.expand_dims(heatmap, 0)
    return heatmap

def plot_heatmap(data, cls_out, model, method_ids):
    fig, axs = plt.subplots(len(method_ids), 1, figsize=(10, 14))
    for i in range(0, len(method_ids)):
        pred_method = method_ids[i]
        heatmap = grad_cam_heatmap(model, 'conv1d_5', data, pred_index=pred_method)
        im = axs[i].imshow(np.expand_dims(heatmap, axis=2), cmap='Reds', aspect="auto", interpolation='nearest',
                   extent=[0, len(data.flatten()), data.flatten().min(), data.flatten().max()], alpha=0.5)
        axs[i].plot(data.flatten(), 'k')
        plt.colorbar(im, ax=axs[i])
        axs[i].set_title(str(pred_method) + ' Score: ' + str(cls_out[pred_method]))
    #plt.tight_layout()
    #fig.subplots_adjust(left=0.02, bottom=0.06, right=0.95, top=0.94, wspace=0.05)
    plt.show()


if __name__ == '__main__':

    print('Reading ' + SERIES_TYPE + ' time series...')
    ts_train_list, ts_test_list, mat_forecast_list, div_label_list = get_data(BASE_PATH, SERIES_TYPE, period='test')
    print('\tDone!')
    print('Processing time series...')
    train_std_data = tuple(map(lambda x: standardize_series(x), ts_train_list))
    ts_train_list_std = list(map(lambda x: x[0], train_std_data))
    ts_test_list_std = np.array(ts_test_list)
    ts_train = tf.keras.preprocessing.sequence.pad_sequences(ts_train_list_std, maxlen=MAX_LEN, dtype='float64',
                                                             padding="pre", truncating="pre", value=0.0)
    ts_train = ts_train.reshape((ts_train.shape[0], ts_train.shape[1], 1))
    ts_test = ts_test_list_std.copy()
    ts_mat_forecast = np.array(mat_forecast_list)
    if len(div_label_list) == 0:
        ts_div_label = np.zeros((ts_mat_forecast.shape[0], ts_mat_forecast.shape[1]))
    else:
        ts_div_label = np.array(div_label_list)
    print('\ttrain series shape:', ts_train.shape)
    print('\ttest series shape:', ts_test.shape)
    print('\tforecast matrix shape:', ts_mat_forecast.shape)
    print('\tdiversity label shape:', ts_div_label.shape)

    model_path = '...'
    cls_model = tf.keras.models.load_model(model_path, compile=False)

    my_model = tf.keras.models.Model([cls_model.inputs], [cls_model.output[1]])

    data = np.expand_dims(ts_train[SERIES_ID], axis=0)
    cls_out = my_model.predict(data).flatten()

    plot_heatmap(data, cls_out, my_model, [0, 1, 2, 3, 4, 5, 6, 7, 8]) # heatmaps for all the methods
