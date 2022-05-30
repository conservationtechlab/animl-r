from tensorflow.keras.utils import Sequence
import numpy as np
from PIL import Image, ImageOps, ImageFile
import tensorflow as tf
ImageFile.LOAD_TRUNCATED_IMAGES = True


def crop_generator(image_df, resize=299, buffer=0, batch_size=32, standardize=True):
    """
    Yields the next training batch.
    Suppose `samples` is an array [[image1_filename,label1], [image2_filename,label2],...].
    """
    num_samples = len(image_df.index)
    batch_size = int(batch_size)
    resize = int(resize)
    # offset=0
    while True:  # Loop forever so the generator never terminates
        # shuffle(samples)
        # Get index to start each batch: [0, batch_size, 2*batch_size, ..., max multiple of batch_size <= num_samples]
        for offset in range(0, num_samples, batch_size):
            # Initialise X_train and y_train arrays for this batch
            X_eval = []

            # For each image
            for i in range(offset, min(num_samples, offset + batch_size)):
                # Load image (X) and label (y)
                try:
                    img = Image.open(image_df['file'].iloc[i])
                except OSError:
                    continue
                width, height = img.size

                left = width * image_df['bbox1'].iloc[i]  # boxes[i, 0]
                top = height * image_df['bbox2'].iloc[i]  # boxes[i, 1]
                right = width * (image_df['bbox2'].iloc[i] + image_df['bbox3'].iloc[i])  # (boxes[i, 0] + boxes[i, 2])
                bottom = height * (image_df['bbox2'].iloc[i] + image_df['bbox4'].iloc[i])  # (boxes[i, 1] + boxes[i, 3])


                left = max(0, left - buffer)
                top = max(0, top - buffer)
                right = min(width, right + buffer)
                bottom = min(height, bottom + buffer)

                img = img.crop((left, top, right, bottom))

                test_img = Image.fromarray((img.numpy()).astype(np.uint8)).convert('RGB')
                test_img.save("test.jpg")
                # img=img.resize((resize,resize))

                # apply any kind of preprocessing                img = cv2.resize(img,(resize,resize))
                # Add example to arrays
                if standardize:
                    X_eval.append(np.array(img) / 255)
                else:
                    X_eval.append(np.array(img))

            # Make sure they're numpy arrays (as opposed to lists)
            X_eval = np.array(X_eval)

            # The generator-y part: yield the next training batch            
            yield (X_eval)
            # offset+=batch_size


def resize_with_padding(img, expected_size):
    # img.thumbnail((expected_size[0], expected_size[1]))
    if img.size[0] == 0 or img.size[1] == 0: return img
    if img.size[0] > img.size[1]:
        new_size = (expected_size[0], int(expected_size[1] * img.size[1] / img.size[0]))
    else:
        new_size = (int(expected_size[0] * img.size[0] / img.size[1]), expected_size[1])
    img = img.resize(new_size, Image.BILINEAR)  # NEAREST BILINEAR
    # print(img.size)
    delta_width = expected_size[0] - img.size[0]
    delta_height = expected_size[1] - img.size[1]
    pad_width = delta_width // 2
    pad_height = delta_height // 2
    padding = (pad_width, pad_height, delta_width - pad_width, delta_height - pad_height)
    return ImageOps.expand(img, padding)


class GenerateCropsFromFile(Sequence):
    def __init__(self, x, resize=299, buffer=2, batch_size=32, standardize=True):
        self.x = x
        self.resize = int(resize)
        self.buffer = buffer
        self.batch_size = int(batch_size)
        self.standardize = standardize

    def __len__(self):
        return int(np.ceil(len(self.x.index) / float(self.batch_size)))

    def __getitem__(self, idx):
        imgarray = []
        for i in range(min(len(self.x.index), idx * self.batch_size), min(len(self.x.index), (idx + 1) * self.batch_size)):
            # for i in range(idx * self.batch_size,(idx + 1) *self.batch_size):
            # for i in range(1,2):
            try:
                file = self.x['file'].iloc[i]
                img = Image.open(file)
            except OSError:
                continue
            width, height = img.size

            bbox1 = self.x['bbox1'].iloc[i]
            bbox2 = self.x['bbox2'].iloc[i]
            bbox3 = self.x['bbox3'].iloc[i]
            bbox4 = self.x['bbox4'].iloc[i]

            left = width * bbox1  # boxes[i, 0]
            top = height * bbox2  # boxes[i, 1]
            right = width * (bbox1 + bbox3)  # (boxes[i, 0] + boxes[i, 2])
            bottom = height * (bbox2 + bbox4)  # (boxes[i, 1] + boxes[i, 3])
            #    print(left,right,top,bottom)



            left = max(0, left - self.buffer)
            top = max(0, top - self.buffer)
            right = min(width, right + self.buffer)
            bottom = min(height, bottom + self.buffer)

            img = img.crop((left, top, right, bottom))
            img = tf.image.resize(img, [456, 456])
            crop_array = tf.keras.preprocessing.image.img_to_array(img)
            test_img = Image.fromarray((img.numpy()).astype(np.uint8)).convert('RGB')
            test_img.save("test.jpg")

            #crop_array = tf.expand_dims(crop_array, 0)
            if self.standardize:
                imgarray.append(np.array(test_img) / 255)
            else:
                imgarray.append(np.array(test_img))
            # img=np.array(img)
            # imgarray.append(np.expand_dims(img, axis=0)/255)

        return np.array(imgarray)
