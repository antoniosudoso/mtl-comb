import tensorflow as tf


def comb_loss_wrapper(f_matrix):
    """
    :param f_matrix: tf.Tensor (n_samples, f_methods, f_horizon)
    """

    def comb_loss(y_true, y_pred):
        """
        :param y_true: tf.Tensor (n_samples, f_horizon)
        :param y_pred: tf.Tensor (n_samples, f_horizon)
        """
        se_num = tf.math.reduce_sum(tf.math.square(y_pred - y_true), axis=1)
        f_average = tf.math.reduce_mean(f_matrix, axis=1)
        se_den = tf.math.reduce_sum(tf.math.square(f_average - y_true), axis=1)
        comb = tf.math.reduce_mean(se_num / se_den)
        return comb

    return comb_loss


def cls_loss_wrapper(y_cls_true, y_cls_pred):
    """
    :param y_cls_true: tf.Tensor (n_samples, f_methods)
    :param y_cls_pred: tf.Tensor (n_samples, f_methods)
    """

    def cls_loss(y_true, y_pred):
        bce = tf.keras.losses.BinaryCrossentropy()
        cls = bce(y_cls_true, y_cls_pred)
        return cls

    return cls_loss


def ort_loss_wrapper(x_reg_feat, x_cls_feat):
    """
    :param x_reg_feat: extracted features from the regression branch
    :param x_cls_feat: extracted features from the classification branch
    """

    def ort_loss(y_true, y_pred):
        ort_mul = tf.linalg.matmul(x_reg_feat, x_cls_feat, transpose_a=False, transpose_b=True)
        # ort = tf.math.square(tf.linalg.norm(ort_mul, ord='fro', axis=(0, 1)))
        ort = tf.reduce_mean(tf.math.square(ort_mul))
        return ort

    return ort_loss


def overall_loss_wrapper(f_matrix, mu_hyper, y_cls_true, y_cls_pred, lambda_hyper, x_reg_feat, x_cls_feat):
    """
    :param f_matrix: tf.Tensor (n_samples, f_methods, f_horizon)
    :param x_cls_feat: tf.Tensor (n_samples, n_features)
    :param x_reg_feat: tf.Tensor (n_samples, n_features)
    :param y_cls_true: tf.Tensor (n_samples, f_methods)
    :param y_cls_pred: tf.Tensor (n_samples, f_methods)
    :param mu_hyper: penalty for classification
    :param lambda_hyper: penalty for orthogonality
    """
    cls = cls_loss_wrapper(y_cls_true, y_cls_pred)
    ort = ort_loss_wrapper(x_reg_feat, x_cls_feat)

    def custom_loss(y_true, y_pred):
        """
        :param y_true: tf.Tensor (n_samples, f_horizon)
        :param y_pred: tf.Tensor (n_samples, f_horizon)
        """
        se_num = tf.math.reduce_sum(tf.math.square(y_pred - y_true), axis=1)
        f_average = tf.math.reduce_mean(f_matrix, axis=1)
        se_den = tf.math.reduce_sum(tf.math.square(f_average - y_true), axis=1)
        comb = tf.math.reduce_mean(se_num / se_den)
        return comb + mu_hyper * cls(0, 0) + lambda_hyper * ort(0, 0)

    return custom_loss
