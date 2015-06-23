from SimpleCV import Image
import numpy

img = Image("us-scaled.jpg")
out = img.binarize(30)
out.save('out.png')

img = Image("out.png")
# ones = img.getNumpy()[:,:,:] == (0, 0, 0)
ones = img.getNumpy()[:,:,0] == 0
trues=numpy.where(ones)

points = []
for p in zip(trues[0], trues[1]):
    # points.append(p)
    print(str(p[0]) + " " + str(p[1]))

for one in ones:
    print(one)
