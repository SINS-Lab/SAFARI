from PyQt5.QtWidgets import QWidget, QApplication
from PyQt5.QtWidgets import QGridLayout, QHBoxLayout, QVBoxLayout, QComboBox
from PyQt5.QtWidgets import QLineEdit, QLabel, QPushButton
from scipy.io import FortranFile
import os
import math
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Circle
from matplotlib.collections import PatchCollection
import safari_input
import subprocess
import xyz_postprocess as xyz_p

def read(f, first, data):
    if first:
        #Emin EMax ESize ASize
        data.append(f.read_reals(dtype=np.float))
        # NDtect
        data.append(f.read_ints(dtype=np.int32))
        # DParams 1-5
        data.append(f.read_reals(dtype=np.float))
        # DParams 6-10
        data.append(f.read_reals(dtype=np.float))
        return
    #NPTS
    npts = f.read_ints(dtype=np.int32)[0]
    for i in range (npts):
        # XTraj, yTraj, Level
        line = f.read_record('f4','f4','i4')
        var1 = [line[0][0], line[1][0], line[2][0]]
        # zTraj
        var2 = f.read_reals(dtype='f4')
        # Energy, Theta, Phi, Area
        var3 = f.read_reals(dtype='f4')
        line = [var1[0], var1[1], var2[0], var3[0], var3[1], var3[2], var1[2], var3[3]]
        data.append(line)

def loadFromText(file):
    f = open(file, 'r')
    n = 0
    data = []
    for line in f:
        arr = line.split()
        if n == 0:
            data.append([float(arr[0]), float(arr[1]),float(arr[2]),float(arr[3])])
        elif n == 1:
            data.append([float(arr[0])])
        elif n == 2:
            data.append([float(arr[0]), float(arr[1]),float(arr[2]),float(arr[3]),float(arr[4])])
        elif n == 3:
            data.append([float(arr[0]), float(arr[1]),float(arr[2]),float(arr[3]),float(arr[4])])
        else:
            data.append([float(arr[0]), float(arr[1]),float(arr[2]),\
                         float(arr[3]),float(arr[4]),float(arr[5]),\
                         float(arr[6]),float(arr[7])])
        n = n + 1
    return data
        

def loadFromCache(cache):
    return np.load(cache+'.npy')

def loadFromUndata(file, cache):
    data = []
    f = FortranFile(file, 'r')
    first = True
    read(f, first, data)
    first = False
    try:
        while True:
            read(f, first, data)
    except Exception as e:
        print(e)
        pass
    np.save(cache, data)
    cache = cache+'.txt'
    out = open(cache, 'w')
    for x in data:
        out.write(str(x)+'\n')
    out.close()
    f.close()
    return data

def load(file):
    
    if file.endswith('.txt') or file.endswith('.data'):
        return loadFromText(file)
        
    if not (file.endswith('.npy') or file.endswith('.undata')):
        return loadFromText(file+'.data')
    
    data = []
    cache = file.replace('.undata','')
    if os.path.isfile(cache+'.npy'):
        data = loadFromCache(cache)
    else:
        data = loadFromUndata(file, cache)
    return data

def unit(theta, phi):
    th = theta * math.pi / 180
    ph = phi * math.pi / 180
    sinth = math.sin(th)
    x = sinth * math.cos(ph)
    y = sinth * math.sin(ph)
    z = math.cos(th)
    s = math.sqrt(x*x + y*y + z*z)
    return np.array([x/s, y/s, z/s])

# x is an array containing the values to do the gaussian for.
def gauss(x, winv):
    return np.exp(-x*x*2.*winv*winv)*winv*0.7978845608

def integrate(numpoints, winv, points, areas, axis):
    # Initializing the array to 0 breaks for some reason.
    intensity = np.array([1e-60 for x in range(numpoints)])
    # We vectorize the maths here, so it only needs 1 loop.
    for i in range(numpoints):
        # eArr - energy[i] is the coordinate for the gaussian
        # Intensity of gaussian at this point
        intensity[i] = np.sum(gauss(points - axis[i], winv) * areas)
        # Cull out values that dont play nicely in excel
        if intensity[i] < 1e-60:
            intensity[i] = 0
            
    m = np.max(intensity)
    if m != 0:
        intensity /= m
    return intensity

class Detector:

    def __init__(self, *args, **kwargs):
        self.detections = np.zeros((0,8))
        self.outputprefix = 'spectrum'
        self.tmax = 180
        self.tmin = -180
        self.emin = 1e20
        self.emax = -1e20
        self.safio = None

    def clear(self):
        self.detections = np.zeros((0,8))
        
    def addDetection(self, line):
        self.detections = np.vstack((self.detections, line))
        e = line[3]
        if e < self.emin:
            self.emin  = e
        if e > self.emax:
            self.emax  = e
            
    def spectrumT(self, res, numpoints=1000):
        step = (self.tmax - self.tmin) / numpoints
        winv = 1/res
        angles = np.array([(self.tmin + x*step) for x in range(numpoints)])
        
        tArr = self.detections[...,4]
        aArr = self.detections[...,7]

        intensity = integrate(numpoints, winv, tArr, aArr, angles)

        out = open(self.outputprefix\
                  + 'Theta-'\
                  + str(self.tmin) + '-'\
                  + str(self.tmax)+'_'\
                  + str(res)+'.txt', 'w')
        out.write(str(len(aArr))+'\n')
        for i in range(numpoints):
            out.write(str(angles[i])+'\t'+str(intensity[i])+'\n')
        out.close()
        fig, ax = plt.subplots()
        ax.plot(angles, intensity)
        ax.set_title("Intensity vs Theta, Detections: "+str(len(aArr)))
        ax.set_xlabel('Angle (Degrees)')
        ax.set_ylabel('Intensity')
        fig.show()


    def spectrumE(self, res, numpoints=1000):
        step = (self.emax - self.emin) / numpoints
        winv = 1/res
        energy = np.array([(self.emin + x*step) for x in range(numpoints)])
        
        eArr = self.detections[...,3]
        aArr = self.detections[...,7]

        intensity = integrate(numpoints, winv, eArr, aArr, energy)

        out = open(self.outputprefix\
                  + 'Energy-'\
                  + str(self.emin) + '-'\
                  + str(self.emax)+'_'\
                  + str(res)+'.txt', 'w')
        out.write(str(len(aArr))+'\n')
        for i in range(numpoints):
            out.write(str(energy[i])+'\t'+str(intensity[i])+'\n')
        out.close()
        fig, ax = plt.subplots()
        ax.plot(energy, intensity)
        ax.set_title("I_E, Detections: "+str(len(aArr)))
        ax.set_xlabel('Energy (eV)')
        ax.set_ylabel('Intensity')
        fig.show()
        
    def impactParam(self, basis = None, dx=0, dy=0):
        x = self.detections[...,0]
        y = self.detections[...,1]
        c = self.detections[...,3]
        
        fig, ax = plt.subplots()
        patches = []
        colours = []
        
        maxX = dx
        maxY = dy
        
        if dx == 0 and dy == 2:
            maxX = np.max(x)
            maxY = np.max(y)
        
        ax.set_xlim(right=maxX)
        ax.set_ylim(top=maxY)
        
        if basis is not None:
            minz = 1e6
            for site in basis:
                if site[2] < minz:
                    minz = site[2]
                for i in range(2):
                    for j in range(2):
                        colours.append(site[2])
                        circle = Circle((site[0]+i*dx, site[1]+j*dy), 1)
                        patches.append(circle)

#        #TODO better colouring.
#        for i in range(len(site)):
#            site[i] = site[i] - minz

        p = PatchCollection(patches, alpha=0.4)
        p.set_array(np.array(colours))
        
        #Draw the basis
        ax.add_collection(p)
        
        #Draw the points
        scat = ax.scatter(x, y, c=c)
        fig.colorbar(scat, ax=ax)
        
        #Add a heightmap
        fig.colorbar(p, ax=ax)

        #Add selected point label
        text = ax.text(0.05, 0.95, 'None Selected',transform=ax.transAxes)
        
        ax.set_title("Detections: "+str(len(x)))
        ax.set_xlabel('X Target (Angstroms)')
        ax.set_ylabel('Y Target (Angstroms)')

        self.p, = ax.plot(0,0,'r+')

        def onclick(event):
            if event.xdata is None:
                return

            close = [1e20, 1e20]
            distsq = close[0]**2 + close[1]**2
            index = -1

            for i in range(len(x)):
                dxsq = (x[i]-event.xdata)**2
                dysq = (y[i]-event.ydata)**2
                if distsq > dxsq + dysq:
                    distsq = dxsq + dysq
                    close[0] = x[i]
                    close[1] = y[i]
                    index = i

            if event.dblclick:
                #Setup a single run safari for this.
                self.safio.fileIn = self.safio.fileIn.replace('_mod.input', '_ss.input')
                self.safio.setGridScat(True)
                self.safio.NUMCHA = 1
                self.safio.XSTART = close[0]
                self.safio.YSTART = close[1]
                self.safio.genInputFile(fileIn=self.safio.fileIn)
                sub = subprocess.run('Safari.exe', shell=True)
                close[0] = round(close[0],2)
                close[1] = round(close[1],2)
                name = self.safio.fileIn.replace('.input', '')
                xyz_p.process_file(name +'.xyz',\
                                   name+str(close[0])+','+str(close[1])+'.xyz')
                print(sub)

            close[0] = round(close[0],5)
            close[1] = round(close[1],5)
            energy = round(self.detections[index][3],2)
            text.set_text(str(close)+', '+str(energy)+'eV')
            self.p.set_xdata([close[0]])
            self.p.set_ydata([close[1]])
            fig.canvas.draw()

        fig.canvas.mpl_connect('button_press_event', onclick)
        
        fig.show()
        
class StripeDetector(Detector):
    
    def __init__(self, theta1, theta2, phi, width):
        super().__init__()
        width = abs(width)
        self.tmin = min(theta1, theta2)
        self.tmax = max(theta1, theta2)
        self.phiMax = phi + width/2
        self.phiMin = phi - width/2

    def isInDetector(self, theta, phi, e):
        return theta > self.tmin and theta < self.tmax\
             and phi > self.phiMin and phi < self.phiMax

class SpotDetector(Detector):

    def __init__(self, theta, phi, size):
        super().__init__()
        self.theta = theta
        self.phi = phi
        self.size = size
        self.dir = unit(theta, phi)
        self.quadDots = []
        self.quadDots.append(self.dir.dot(unit(theta - size/2, phi)))
        self.quadDots.append(self.dir.dot(unit(theta + size/2, phi)))
        self.quadDots.append(self.dir.dot(unit(theta, phi - size/2)))
        self.quadDots.append(self.dir.dot(unit(theta, phi + size/2)))

    def isInDetector(self, theta, phi, e):
        dir = unit(theta, phi)
        dotdir = dir.dot(self.dir)
        for dot in self.quadDots:
        # This would mean it is more aligned
        # to the centre than the corner is.
            if dotdir >= dot:
                return True
        return False

    def spectrum(self, res, numpoints=1000):
        return self.spectrumE(res=res, numpoints=numpoints)

class Spectrum:

    def __init__(self):
        self.detector = None
        self.box_emin = None
        self.safio = None
        self.name = None
        self.rawData = []
        self.stuck = []
        self.buried = []
        self.data = []

    def clear(self):
        self.detector = None
        self.box_emin = None
        self.safio = None
        self.rawData = []
        self.stuck = []
        self.buried = []
        self.data = []

    def clean(self, data, detectorType=-1, emin=-1e6, emax=1e6,\
                                           lmin=1, lmax=20, \
                                           phimin=-1e6, phimax=1e6, \
                                           thmin=-1e6, thmax=1e6):
        self.rawData = data
        self.data = []
        for i in range(0, 4):
            self.data.append(data[i])

        # If this is not the case, detector is defined elsewhere.
        if self.detector is None:
            self.detectorType = self.data[1][0]
            self.detectorParams = self.data[2]
            if self.detectorType == 1:
                self.detector = SpotDetector(self.detectorParams[0],\
                                             self.safio.PHI0,\
                                             self.detectorParams[2])
        self.detector.outputprefix = self.name+'_spectrum_'
        if emin!=-1e6:
            self.detector.emin = emin
        if emax!=-1e6:
            self.detector.emax = emax
        self.detector.clear()
        for i in range(4,len(data)):
            traj = data[i]
            e = traj[3]
            t = traj[4]
            p = traj[5]
            l = traj[6]
            # Stuck
            if e == -100:
                self.stuck.append(traj)
                continue
            if e == -200:
                self.buried.append(traj)
                continue
            if e < emin or e > emax or l > lmax or l < lmin\
            or t > thmax or t < thmin or p > phimax or p < phimin:
                continue
            if self.detector.isInDetector(t, p, e):
                self.detector.addDetection(traj)
            self.data.append(traj)

    def plotThetaE(self):
        
        # X Coord on graph
        x = []
        # Y Coord on graph
        y = []
        # Dot Colour, scaled by area.
        c = []
        
        for i in range(4,len(self.data)):
            line = self.data[i]
            x.append(line[4])
            y.append(line[3])
            c.append(line[6])
        
       # c = np.log(c)
        
        if np.min(c) != np.max(c):
            c = c - np.min(c)
        c = c / np.max(c)
        print(np.max(c))
        print(np.min(c))
        colour = [(var,0,0) for var in c]
        
        fig, ax = plt.subplots()
        ax.scatter(x, y, c=colour)
        ax.set_title("Energy vs Theta, Detections: "+str(len(x)))
        ax.set_xlabel('Angle (Degrees)')
        ax.set_ylabel('Energy (eV)')
        fig.show()

    def plotPhiTheta(self):
        
        # X Coord on graph
        x = []
        # Y Coord on graph
        y = []
        # Dot Colour, scaled by area.
        c = []
        
        for i in range(4,len(self.data)):
            line = self.data[i]
            x.append(line[5])
            y.append(line[4])
            c.append(line[6])
        
       # c = np.log(c)
        
        if np.min(c) != np.max(c):
            c = c - np.min(c)
        c = c / np.max(c)
        print(np.max(c))
        print(np.min(c))
        colour = [(var,0,0) for var in c]
        
        fig, ax = plt.subplots()
        ax.scatter(x, y, c=colour)
        ax.set_title("Theta vs Phi, Detections: "+str(len(x)))
        ax.set_xlabel('Phi Angle (Degrees)')
        ax.set_ylabel('Theta Angle (Degrees)')
        fig.show()
        
    def detectorSelection(self):
        dropdown = QComboBox()
        
        dropdown.addItem('Spot')
        dropdown.addItem('Stripe')
        
        self.detectorDropdown = dropdown
        
        return dropdown
    
    def getDetectorType(self):
        return self.detectorDropdown.currentText()
        
    def detectorSettings(self):
        '''When the button is pressed, this should
           Provide settings for the type of detector selected'''
        layout = QVBoxLayout()
        dtype = self.getDetectorType()
        
        self.popup2 = QWidget()
        window = self.popup2
        
        if self.box_emin is None:
            self.box_emin = QLineEdit(str(self.data[0][0]))
            self.box_emax = QLineEdit(str(self.data[0][1]))
            self.box_phimin = QLineEdit(str(self.safio.PHI0))
            self.box_phimax = QLineEdit(str(self.safio.PHI0))
            self.box_thetamin = QLineEdit(str(self.safio.DTECTPAR[0]))
            self.box_thetamax = QLineEdit(str(self.safio.DTECTPAR[0]))
            self.box_eres = QLineEdit(str(self.data[0][2]))
            self.box_ares = QLineEdit(str(self.safio.DTECTPAR[2]))
        
        if dtype == 'Spot':
            layout.addWidget(QLabel('phi'))
            layout.addWidget(self.box_phimin)
            layout.addWidget(QLabel('theta'))
            layout.addWidget(self.box_thetamin)
            layout.addWidget(QLabel('resolution'))
            layout.addWidget(self.box_ares)
        elif dtype == 'Stripe':
            print(dtype)
            
            
        # Button to close the window
        close = QPushButton('Done')
        def done():
            # Update the detector
            if dtype == 'Spot':
                phi = float(self.box_phimin.displayText())
                theta = float(self.box_thetamin.displayText())
                ares = float(self.box_ares.displayText())
                self.detector = SpotDetector(theta, phi, ares)
            elif dtype == 'Stripe':
                print(dtype)
            
            window.close()
        close.clicked.connect(done)
        layout.addWidget(close)
        window.setLayout(layout)
        window.show()
        return
    
    def setDetector(self, data):
        
        return
        

    def run(self, data):
        self.popup = QWidget()
        self.data = data
        window = self.popup
        layout = QVBoxLayout()
        sublayout = QHBoxLayout()
        dtectlayout = QHBoxLayout()
        ellayout = QHBoxLayout()
        anglelayout = QHBoxLayout()
        
        # Dropdown selector for detector types
        dtectlayout.addWidget(self.detectorSelection())
        
        dtectbutton = QPushButton('Detector Settings')
        def run():
            try:
                self.detectorSettings()
            except Exception as e:
                print(e)
                pass
        dtectbutton.clicked.connect(run)
        dtectlayout.addWidget(dtectbutton)
        
        
        layout.addLayout(dtectlayout)
        

        # Fields to enter values for stuff
        label = QLabel('emin')
        emin = QLineEdit(str(self.safio.EMIN))
        sublayout.addWidget(label)
        sublayout.addWidget(emin)
        ellayout.addLayout(sublayout)

        sublayout = QHBoxLayout()
        label = QLabel('emax')
        emax = QLineEdit(str(self.safio.EMAX))
        sublayout.addWidget(label)
        sublayout.addWidget(emax)
        ellayout.addLayout(sublayout)

        sublayout = QHBoxLayout()
        label = QLabel('eres')
        eres = QLineEdit(str(self.safio.ESIZE))
        sublayout.addWidget(label)
        sublayout.addWidget(eres)
        ellayout.addLayout(sublayout)

        sublayout = QHBoxLayout()
        label = QLabel('Lmin')
        lmin = QLineEdit('0')
        sublayout.addWidget(label)
        sublayout.addWidget(lmin)
        ellayout.addLayout(sublayout)

        sublayout = QHBoxLayout()
        label = QLabel('Lmax')
        lmax = QLineEdit('20')
        sublayout.addWidget(label)
        sublayout.addWidget(lmax)
        ellayout.addLayout(sublayout)

        layout.addLayout(ellayout)
        
        sublayout = QHBoxLayout()
        label = QLabel('thmin')
        thmin = QLineEdit('0')
        sublayout.addWidget(label)
        sublayout.addWidget(thmin)
        anglelayout.addLayout(sublayout)

        sublayout = QHBoxLayout()
        label = QLabel('thmax')
        thmax = QLineEdit('90')
        sublayout.addWidget(label)
        sublayout.addWidget(thmax)
        anglelayout.addLayout(sublayout)
        
        sublayout = QHBoxLayout()
        label = QLabel('phimin')
        phimin = QLineEdit(str(self.safio.PHI0 - 5))
        sublayout.addWidget(label)
        sublayout.addWidget(phimin)
        anglelayout.addLayout(sublayout)

        sublayout = QHBoxLayout()
        label = QLabel('phimax')
        phimax = QLineEdit(str(self.safio.PHI0 + 5))
        sublayout.addWidget(label)
        sublayout.addWidget(phimax)
        anglelayout.addLayout(sublayout)

        sublayout = QHBoxLayout()
        label = QLabel('ares')
        ares = QLineEdit(str(self.safio.ASIZE))
        sublayout.addWidget(label)
        sublayout.addWidget(ares)
        anglelayout.addLayout(sublayout)

        layout.addLayout(anglelayout)

        #Button to run the spectrum stuff.
        runbutton = QPushButton('I vs Energy')
        def run():
            try:
                self.setDetector(data)
                self.clean(data,emin=float(emin.displayText()),\
                                emax=float(emax.displayText()),\
                                phimin=float(phimin.displayText()),\
                                phimax=float(phimax.displayText()),\
                                thmin=float(thmin.displayText()),\
                                thmax=float(thmax.displayText()),\
                                lmin=float(lmin.displayText()),\
                                lmax=float(lmax.displayText()))
                self.detector.spectrum(res=float(eres.displayText()))
            except Exception as e:
                print(e)
                pass
        runbutton.clicked.connect(run)
        layout.addWidget(runbutton)
        
        #Button to run the spectrum stuff.
        tibutton = QPushButton('I vs Theta')
        def runIT():
            try:
                print('init detector')
                detector = self.detector
                dphi =  float(phimax.displayText()) - float(phimin.displayText())
                self.detector = StripeDetector(float(thmin.displayText()),\
                                               float(thmax.displayText()),\
                                               float(phimin.displayText()),\
                                               dphi)
                print('clean data')
                self.clean(data,emin=float(emin.displayText()),\
                                emax=float(emax.displayText()),\
                                phimin=float(phimin.displayText()),\
                                phimax=float(phimax.displayText()),\
                                thmin=float(thmin.displayText()),\
                                thmax=float(thmax.displayText()),\
                                lmin=float(lmin.displayText()),\
                                lmax=float(lmax.displayText()))
                print('make spectrum')
                self.detector.spectrumT(res=float(ares.displayText()))
                self.detector = detector
                print('done')
            except Exception as e:
                print(e)
                pass
        tibutton.clicked.connect(runIT)
        layout.addWidget(tibutton)
        
        #Button to run the spectrum stuff.
        etbutton = QPushButton('E vs Theta')
        def runET():
            try:
                self.setDetector(data)
                self.clean(data,emin=float(emin.displayText()),\
                                emax=float(emax.displayText()),\
                                phimin=float(phimin.displayText()),\
                                phimax=float(phimax.displayText()),\
                                thmin=float(thmin.displayText()),\
                                thmax=float(thmax.displayText()),\
                                lmin=float(lmin.displayText()),\
                                lmax=float(lmax.displayText()))
                self.plotThetaE()
            except Exception as e:
                print(e)
                pass
        etbutton.clicked.connect(runET)
        layout.addWidget(etbutton)
        
        #Button to run the spectrum stuff.
        tpbutton = QPushButton('Theta vs Phi')
        def runTP():
            try:
                print('init detector')
                detector = self.detector
                dphi =  float(phimax.displayText()) - float(phimin.displayText())
                self.detector = StripeDetector(float(thmin.displayText()),\
                                               float(thmax.displayText()),\
                                               float(phimin.displayText()),\
                                               dphi)
                self.clean(data,emin=float(emin.displayText()),\
                                emax=float(emax.displayText()),\
                                phimin=float(phimin.displayText()),\
                                phimax=float(phimax.displayText()),\
                                thmin=float(thmin.displayText()),\
                                thmax=float(thmax.displayText()),\
                                lmin=float(lmin.displayText()),\
                                lmax=float(lmax.displayText()))
                self.plotPhiTheta()
                self.detector = detector
            except Exception as e:
                print(e)
                pass
        tpbutton.clicked.connect(runTP)
        layout.addWidget(tpbutton)
        
        #Button to run the spectrum stuff.
        impbutton = QPushButton('Impact Plot')
        def runIMP():
            try:
                self.setDetector(data)
                self.clean(data,emin=float(emin.displayText()),\
                                emax=float(emax.displayText()),\
                                phimin=float(phimin.displayText()),\
                                phimax=float(phimax.displayText()),\
                                thmin=float(thmin.displayText()),\
                                thmax=float(thmax.displayText()),\
                                lmin=float(lmin.displayText()),\
                                lmax=float(lmax.displayText()))
                self.detector.safio = self.safio
                self.detector.impactParam(self.safio.BASIS,
                                         self.safio.AX, 
                                         self.safio.AY)
            except Exception as e:
                print(e)
                pass
        impbutton.clicked.connect(runIMP)
        layout.addWidget(impbutton)

        # Button to close the window
        close = QPushButton('Done')
        def done():
            window.close()
        close.clicked.connect(done)
        layout.addWidget(close)
        window.setLayout(layout)
        window.show()
        return

        
def fileSelection():
    dropdown = QComboBox()
    directory = '.'
    for filename in os.listdir(directory):
#  Load in the inputs, but not _mod or _ss as those are temporary ones.
        if filename.endswith('.input') and not filename==('safari.input')\
           and not (filename.endswith('_mod.input') or filename.endswith('_ss.input')):
            dropdown.addItem(filename.replace('.input', ''))
    return dropdown
        
def run(spectrum):
    app = QApplication([])
    window = QWidget()
    layout = QGridLayout()
    
    sublayout = QHBoxLayout()
    label = QLabel('input file name')
    filebox = fileSelection()
    
    sublayout.addWidget(label)
    sublayout.addWidget(filebox)
    
    x = 0
    y = 0

    layout.addLayout(sublayout, x, y)

    #Make a button for running 
    run = QPushButton('Spectrum')
    def push():
        data = load(filebox.currentText())
        try:
            spectrum = Spectrum()
            file = filebox.currentText()
            if file.endswith('.data'):
                file = file.replace('.data', '.input')
            elif file.endswith('.txt'):
                file = file.replace('.txt', '.input')
            elif file.endswith('.undata'):
                file = file.replace('.undata', '.input')
            elif file.endswith('.npy'):
                file = file.replace('.npy', '.input')
            else:
                file = file + '.input'
            spectrum.safio = safari_input.SafariInput(file)
            spectrum.name = filebox.currentText()
            spectrum.run(data)
            spectrum.popup.setWindowTitle(spectrum.name)
        except Exception as e:
            print(e)
            pass
    run.clicked.connect(push)
    
    box = QVBoxLayout()
    box.addWidget(run)

    layout.addLayout(box, x + 10, y)
    
    # Button to close the window
    close = QPushButton('Close')
    def done():
        window.close()
    close.clicked.connect(done)
    layout.addWidget(close)
    window.setLayout(layout)
    window.setWindowTitle('Detect')
    window.show()
    app.exec_()

if __name__ == '__main__':
    spectrum = Spectrum()
    run(spectrum)
