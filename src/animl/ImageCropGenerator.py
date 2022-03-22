from tensorflow.keras.utils import Sequence
import numpy as np
from PIL import Image, ImageOps, ImageFile
ImageFile.LOAD_TRUNCATED_IMAGES = True



def crop_generator(images,boxes,resize=299,buffer=0,batch_size=32,standardize=True):
    """
    Yields the next training batch.
    Suppose `samples` is an array [[image1_filename,label1], [image2_filename,label2],...].
    """
    num_samples = len(images)
    batch_size=int(batch_size)
    resize=int(resize)
    #offset=0
    while True: # Loop forever so the generator never terminates
        #shuffle(samples)
        # Get index to start each batch: [0, batch_size, 2*batch_size, ..., max multiple of batch_size <= num_samples]
        for offset in range(0, num_samples, batch_size):
            # Initialise X_train and y_train arrays for this batch
            X_eval = []

            # For each image
            for i in range(offset,min(num_samples,offset+batch_size)):
                # Load image (X) and label (y)
                try: img=Image.open(images[i])
                except OSError: continue
                width, height = img.size

                left = width*boxes[i,0]
                top = height*boxes[i,1]
                right = width*(boxes[i,0]+boxes[i,2])
                bottom = height*(boxes[i,1]+boxes[i,3])

                buffer2=max(right-left,bottom-top)*buffer

                left = max(0,left-buffer2)
                top = max(0,top-buffer2)
                right = min(width,right+buffer2)
                bottom = min(height,bottom+buffer2)

                img=img.crop((left, top, right, bottom))
                img=resize_with_padding(img, (resize,resize))
                #img=img.resize((resize,resize))
                    
                # apply any kind of preprocessing                img = cv2.resize(img,(resize,resize))
                # Add example to arrays
                if standardize:
                    X_eval.append(np.array(img)/255)
                else:
                    X_eval.append(np.array(img))

            # Make sure they're numpy arrays (as opposed to lists)
            X_eval = np.array(X_eval)

            # The generator-y part: yield the next training batch            
            yield(X_eval)
            #offset+=batch_size



def resize_with_padding(img, expected_size):
    #img.thumbnail((expected_size[0], expected_size[1]))
    if img.size[0] == 0 or img.size[1] == 0: return img
    if img.size[0]>img.size[1]:
        new_size=(expected_size[0],int(expected_size[1]*img.size[1]/img.size[0]))
    else:
        new_size=(int(expected_size[0]*img.size[0]/img.size[1]),expected_size[1])
    img = img.resize(new_size, Image.BILINEAR) #NEAREST BILINEAR
    # print(img.size)
    delta_width = expected_size[0] - img.size[0]
    delta_height = expected_size[1] - img.size[1]
    pad_width = delta_width // 2
    pad_height = delta_height // 2
    padding = (pad_width, pad_height, delta_width - pad_width, delta_height - pad_height)
    return ImageOps.expand(img, padding)


class GenerateCropsFromFile(Sequence):
    def __init__(self, x, boxes, resize=299,buffer=2,batch_size=32,standardize=True):
        self.x = x
        self.boxes = boxes
        self.resize = int(resize)
        self.buffer=buffer
        self.batch_size = int(batch_size)
        self.standardize=standardize
        

    def __len__(self):
        return int(np.ceil(len(self.x) / float(self.batch_size)))

    def __getitem__(self, idx):
        imgarray = []
        for i in range(min(len(self.x),idx * self.batch_size),min(len(self.x),(idx + 1) * self.batch_size)):
        #for i in range(idx * self.batch_size,(idx + 1) *self.batch_size):
        #for i in range(1,2):
            try: img=Image.open(self.x[i])
            except OSError: continue
            width, height = img.size
            
            left = width*self.boxes[i,0]
            top = height*self.boxes[i,1]
            right = width*(self.boxes[i,0]+self.boxes[i,2])
            bottom = height*(self.boxes[i,1]+self.boxes[i,3])
        #    print(left,right,top,bottom)

            buffer2=max(right-left,bottom-top)*self.buffer

            left = max(0,left-buffer2)
            top = max(0,top-buffer2)
            right = min(width,right+buffer2)
            bottom = min(height,bottom+buffer2)
            
            img=img.crop((left, top, right, bottom))
            img=resize_with_padding(img, (self.resize,self.resize))
            if self.standardize:
                imgarray.append(np.array(img)/255)
            else:
                imgarray.append(np.array(img))
            #img=np.array(img)
            #imgarray.append(np.expand_dims(img, axis=0)/255)
        
        return np.array(imgarray)

