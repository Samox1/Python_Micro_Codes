# https://adventuresinmachinelearning.com/convolutional-neural-networks-tutorial-tensorflow/
import tensorflow as tf
import matplotlib.pyplot as plt
import numpy as np

tf.compat.v1.disable_eager_execution()


image_file = 'dog_z_majorki.jpg'
image = plt.imread(image_file)
image = np.array(image).astype(np.float32)
image = np.expand_dims(image,0)
print(image.shape)

# np.random.seed(0)
in_channels=3
out_channels = 3

filter_height = 3
filter_width = 4

# [batch_size, image_rows, image_cols, number_of_colors]
strides = [1,1,1,1]


przykladowy_filtr = np.random.randn(filter_height, filter_width, in_channels, out_channels).astype(np.float32)/10.0
# filter: [filter_height, filter_width, in_channels, out_channels]
obraz = tf.compat.v1.placeholder(dtype=tf.float32, shape=[1, None, None, in_channels])
filtr = tf.compat.v1.placeholder(dtype=tf.float32, shape=[filter_height, filter_width, in_channels, out_channels])
obraz_po_konwolucji = tf.compat.v1.nn.conv2d(input=obraz, filter = filtr,
                                   strides = strides, padding="VALID")

with tf.compat.v1.Session() as sess:
    image1, obraz_po_konwolucji1 = \
        sess.run([obraz,obraz_po_konwolucji], feed_dict={obraz: image, filtr : przykladowy_filtr})





# fragment_obrazu = image1[0,:filter_height, :filter_width, :]


n = 10

wsp_x = np.random.randint(0,obraz_po_konwolucji1.shape[1], size=n)
wsp_y = np.random.randint(0,obraz_po_konwolucji1.shape[2], size=n)
wsp_kanalu = np.random.randint(0,out_channels, size=n)


for i in range(n):
    piksel = obraz_po_konwolucji1[0, wsp_x[i], wsp_y[i], wsp_kanalu[i]]
    fragment_obrazu  = image1[0, strides[1]*wsp_x[i]:strides[1]*wsp_x[i]+filter_height, strides[2]*wsp_y[i]:strides[2]*wsp_y[i]+filter_width,:]
    wynik_konwolucji = np.sum(fragment_obrazu * przykladowy_filtr[:,:,:, wsp_kanalu[i]])
    print("wsp_x = {}, wsp_y = {}, wsp_kanal = {}, piksel_org = {}, piksel_est = {}".format(wsp_x[i], wsp_y[i], wsp_kanalu[i], piksel, wynik_konwolucji))


fig=plt.figure(figsize=(10, 5))
fig.add_subplot(1, 2, 1)
plt.imshow(image[0,:,:,:].astype(np.uint8))
fig.add_subplot(1, 2, 2)
plt.imshow(obraz_po_konwolucji1[0,:,:,:].astype(np.uint8))

plt.show()

