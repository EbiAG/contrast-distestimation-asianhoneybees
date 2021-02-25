# The analysis of the images from the Botanical Garden and OpenField will be done.
# The purpose is to obtain the contrast in these images.
# The method employed will be the same as in Tautz et al 2004,
# The contrast in any section is given by:
# 1. Obtan the average value of the R, G and B channel of each pixel.
# 2. Divide the standard deviation of these average values by the mean of these average values
# In our case, instead of looking at specific sections, we will do this for the whle image
# We will use two methods:
# 1. Obtain the contrast over the whole image
# 2. Obtain the contrast within three equivalent horizontal bands which cover the whole image
# Since the images were taken by the same person, the idea is that these bands represent
# 3 different visual fields for the bee; the dorsal, lateral and ventral

from PIL import Image
import numpy as np
import pandas as pd


# region Function to estimate Contrast
def contrast_estimation(img_file):
    dat = np.asarray(img_file)
    average_pixel = np.mean(dat, axis=2)
    cut1 = int(average_pixel.shape[0] / 3)
    cut2 = int(average_pixel.shape[0] * (2 / 3))
    average_pixel1 = average_pixel[0:cut1, :]
    average_pixel2 = average_pixel[cut1:cut2, :]
    average_pixel3 = average_pixel[cut2:, :]
    mean_contrast_overall = np.std(average_pixel) / np.mean(average_pixel)
    mean_contrast_band1 = np.std(average_pixel1) / np.mean(average_pixel1)
    mean_contrast_band2 = np.std(average_pixel2) / np.mean(average_pixel2)
    mean_contrast_band3 = np.std(average_pixel3) / np.mean(average_pixel3)
    return mean_contrast_overall, mean_contrast_band1, mean_contrast_band2, mean_contrast_band3
# endregion

# region Looping Over Images
contrast = pd.DataFrame(index=np.arange(10), columns=np.arange(6))
cond = ["BG", "OF"]
dist = ["100m", "200m", "300m", "400m", "500m"]
count = 0
for i in cond:
    for j in dist:
        fn = i + "_" + j + ".jpg"
        print(fn)
        contrast.iloc[count, 0] = i
        contrast.iloc[count, 1] = j
        img_file = Image.open(fn)
        contrast_estimates = contrast_estimation(img_file)
        contrast.iloc[count, 2] = contrast_estimates[0]
        contrast.iloc[count, 3] = contrast_estimates[1]
        contrast.iloc[count, 4] = contrast_estimates[2]
        contrast.iloc[count, 5] = contrast_estimates[3]
        count += 1
# endregion

# region Outputting CSV
contrast.rename(columns={0: 'Condition', 1: 'Distance', 2: 'Overall_Contrast',
                         3: 'Band1_Contrast', 4: 'Band2_Contrast', 5: 'Band3_Contrast'}, inplace=True)
contrast.to_csv("ContrastValues_BGOF.csv", index=False)
# endregion
