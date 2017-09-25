""" Helper module for processing data for egambo deep learning """

def batch_features_labels(features, labels, batch_size):
    """
    Split features and labels into batches
    """
    for start in range(0, len(features), batch_size):
        end = min(start + batch_size, len(features))
        yield features[start:end], labels[start:end]

def batch_features_labels_scale(features, labels, error_scale, batch_size):
    """
    Split features, labels and error_sacle into batches
    """
    for start in range(0, len(features), batch_size):
        end = min(start + batch_size, len(features))
        yield features[start:end], labels[start:end], error_scale[start:end]
