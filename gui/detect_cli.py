import os
import math
import time
import numpy as np
#if you utilize the following two lines you will be able to run 
#the figures in here. This requires changing the backend of the fig.show()
#for more backend choices please see https://matplotlib.org/tutorials/introductory/usage.html#what-is-a-backend
import matplotlib
#Qt5Agg is the backend
matplotlib.use('Qt5Agg')
import matplotlib.pyplot as plt
import safari_input
import subprocess
import xyz_postprocess as xyz_p
import detect_processor as detect

def frange(start, end, step):
    return np.arange(start, end, step)

def e_theta_loop(dir, theta1, theta2, theta_step):
    if dir != '.':
        dir = os.path.join('.',dir)

    for filename in os.listdir(dir):
        if filename.endswith('.data'):
            file = os.path.join(dir, filename)
            safio = safari_input.SafariInput(file.replace('.data', '.input'))
            print('loading data')
            data = detect.load(file)
            print('data loaded')
            fig, ax = plt.subplots()
            num = 0
            for theta in frange(theta1, theta2, theta_step):
                print('Theta: '+str(theta))
                spectrum = detect.Spectrum()
                spectrum.plots = False
                spectrum.name = filename.replace('.data','')
                spectrum.safio = safio
                spectrum.safio.DTECTPAR[0] = theta
                spectrum.detector = None
                spectrum.clean(data)
                energy, intensity = spectrum.detector.spectrum(res=spectrum.safio.ESIZE)
                intensity = intensity + num
                ax.plot(energy, intensity, label=str(theta))
                num = num + 1
                spectrum = None
            ax.legend()
            ax.set_title("Intensity vs Energy")
            ax.set_xlabel('Energy (eV)')
            ax.set_ylabel('Intensity')
            fig.show()
    input('Press Enter to exit')

dir = input('Input Directory: ')
theta1 = float(input('Initial Theta: '))
theta2 = float(input('Final Theta: '))
theta_step = float(input('Theta Step: '))

e_theta_loop(dir, theta1, theta2, theta_step)