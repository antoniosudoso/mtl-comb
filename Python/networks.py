import tensorflow as tf
from losses import *


def se_block(in_block, ratio=16):
    filters = in_block.shape[-1]
    x = tf.keras.layers.GlobalAveragePooling1D()(in_block)
    x = tf.keras.layers.Dense(filters // ratio, activation='relu', kernel_initializer='he_normal', use_bias=False)(x)
    x = tf.keras.layers.Dense(filters, activation='sigmoid', kernel_initializer='he_normal', use_bias=False)(x)
    return tf.keras.layers.Multiply()([in_block, x])


def cnn_feature_extractor(x_raw, task):
    x = tf.keras.layers.Conv1D(filters=64, kernel_size=2, activation='relu', padding="same",
                               kernel_initializer='he_uniform')(x_raw)
    # x = tf.keras.layers.BatchNormalization()(x)
    x = se_block(x)
    x = tf.keras.layers.Conv1D(filters=128, kernel_size=4, activation='relu', padding="same",
                               kernel_initializer='he_uniform')(x)
    # x = tf.keras.layers.BatchNormalization()(x)
    x = se_block(x)
    x = tf.keras.layers.Conv1D(filters=64, kernel_size=8, activation='relu', padding="same",
                               kernel_initializer='he_uniform', name='last_conv_' + task)(x)
    x = tf.keras.layers.GlobalAveragePooling1D()(x)
    # x = tf.keras.layers.Dense(128, activation='relu')(x)
    return x


def build_baseline(ts_length, f_methods, f_horizon):
    """
    :param ts_length: sequence length
    :param f_methods: number of forecasting methods
    :param f_horizon: forecasting horizon
    :return: model
    """
    x_raw = tf.keras.Input(shape=(ts_length, 1))
    x_for = tf.keras.Input(shape=(f_methods, f_horizon))

    # REGRESSION SUBNETWORK
    x = cnn_feature_extractor(x_raw, 'reg')
    softmax_output = tf.keras.layers.Dense(f_methods, activation='softmax', name="softmax_output")(
        x)  # weights of the forecast combination

    repeat_softmax_output = tf.keras.layers.RepeatVector(f_horizon)(softmax_output)
    permute_input_forecasts = tf.keras.layers.Permute((2, 1))(x_for)
    output = tf.keras.layers.Multiply()([permute_input_forecasts, repeat_softmax_output])
    overall_output = tf.keras.layers.Lambda(lambda args: tf.math.reduce_sum(args, axis=-1), name="overall_output")(
        output)  # combined forecasts

    weights_model = tf.keras.Model(inputs=x_raw, outputs=softmax_output, name="weights_model")
    full_model = tf.keras.Model(inputs=[x_raw, x_for], outputs=overall_output, name="baseline")

    opt = tf.keras.optimizers.legacy.Adam()
    full_model.compile(optimizer=opt, loss=comb_loss_wrapper(x_for))
    return full_model, weights_model


def build_network(ts_length, f_methods, f_horizon, lambda_hyper, mu_hyper):
    """
    :param ts_length: sequence length
    :param f_methods: number of forecasting methods
    :param f_horizon: forecasting horizon
    :param lambda_hyper: penalty parameter for orthogonality
    :param mu_hyper: penalty parameter for multi-label classification
    :return: models
    """
    x_raw = tf.keras.Input(shape=(ts_length, 1))
    x_for = tf.keras.Input(shape=(f_methods, f_horizon)) # matrix of forecasts from the individual methods of the pool
    x_cls_true = tf.keras.Input(shape=(f_methods, ))

    # REGRESSION SUBNETWORK
    x_reg_feat = cnn_feature_extractor(x_raw, 'reg')
    x_reg_out = tf.keras.layers.Dense(f_methods, activation='linear', name="reg_output")(x_reg_feat)  # un-normalized weights from the regression subnetwork
    # CLASSIFICATION SUBNETWORK
    x_cls_feat = cnn_feature_extractor(x_raw, 'cls')
    x_cls_out = tf.keras.layers.Dense(f_methods, activation='sigmoid', name="cls_output")(x_cls_feat)  # predicted labels from the classification subnetwork
    x_comb = tf.keras.layers.Multiply()([x_reg_out, x_cls_out])

    softmax_output = tf.keras.layers.Dense(f_methods, activation='softmax', name="softmax_output")(x_comb)  # weights of the forecast combination
    repeat_softmax_output = tf.keras.layers.RepeatVector(f_horizon)(softmax_output)
    permute_input_forecasts = tf.keras.layers.Permute((2, 1))(x_for)
    output = tf.keras.layers.Multiply()([permute_input_forecasts, repeat_softmax_output])
    overall_output = tf.keras.layers.Lambda(lambda args: tf.math.reduce_sum(args, axis=-1), name="overall_output")(output)  # combined forecasts

    weights_model = tf.keras.Model(inputs=x_raw, outputs=softmax_output, name="weights_model")  # returns the weights for a given series
    full_model = tf.keras.Model(inputs=[x_raw, x_for, x_cls_true], outputs=overall_output, name="full_model")  # returns the combined forecasts for a given series
    reg_model = tf.keras.Model(inputs=x_raw, outputs=[x_reg_feat, x_reg_out], name="regression_model")  # returns features and un-normalized weights from regression
    cls_model = tf.keras.Model(inputs=x_raw, outputs=[x_cls_feat, x_cls_out], name="classification_model")  # returns features and labels from classification

    #opt = tf.keras.optimizers.legacy.SGD()
    opt = tf.keras.optimizers.legacy.Adam()
    full_model.compile(optimizer=opt,
                       loss=overall_loss_wrapper(x_for, mu_hyper, x_cls_true, x_cls_out, lambda_hyper, x_reg_feat, x_cls_feat),
                       metrics=[comb_loss_wrapper(x_for), cls_loss_wrapper(x_cls_true, x_cls_out), ort_loss_wrapper(x_reg_feat, x_cls_feat)])

    return full_model, weights_model, reg_model, cls_model
