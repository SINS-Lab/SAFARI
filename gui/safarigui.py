# -*- coding: utf-8 -*-
"""
Created on Tue Jan 29 14:52:04 2019

This program loads in parameters from sample.input, and then
saves output parameters to modified.input. If sample.input is
not found, it will use some default values instead.

sample.input and modified.input are in the working directory.

When the input fields are de-focused, or enter is pressed, it
will update the modified.input file. When the run button is
pressed, it will execute the command in the box above it.

@author: Patrick
"""

from PyQt5.QtWidgets import *
import os
import subprocess

# A bunch of functions for fortran IO
def parseVar(input):
    string = str(input)
    if string.endswith('d0') or string.endswith('D0'):
        var = string.replace('D0','').replace('d0','')
        return float(var)
    if string == 't' or string == 'T':
        return True
    if string == 'f' or string == 'F':
        return False
    if isInt(string):
        return int(string)
    if isFloat(string):
        return float(string)
    return string

def parseLine(input):
    vars = input.split()
    ret = []
    for var in vars:
        ret.append(parseVar(var))
    return ret

def toStr(input):
    if isinstance(input, bool):
        return 't' if input else 'f'
    return str(input)

def serializeArr(input):
    output = ''
    n = 0
    for var in input:
        if n == 0:
            output = toStr(var)
        else:
            output = output + ' ' + toStr(var)
        n = n + 1
    return output

def serialize(*input):
    return serializeArr(input)

def isInt(s):
    try: 
        int(s)
        return True
    except ValueError:
        return False

def isFloat(s):
    try: 
        float(s)
        return True
    except ValueError:
        return False

class SafariInput:

    def __init__(self, fileIn):
        self.fileIn = fileIn
        
        # Inialize All variables
        self.E0 = 1625.0
        self.THETA0 = 55.0
        self.PHI0 = 45.0
        self.MASS = 22.989
        self.EMIN = 0.5
        self.EMAX = 1625.0
        self.ESIZE = 16.0
        self.ASIZE = 5.0

        self.NDTECT = 1
        self.DTECTPAR = [55.0, 45.0, 3.0, 0.0]
        
        self.DELLOW = 1e-8
        self.DELT0 = 10.0
        self.DEMAX = 0.3
        self.DEMIN = 0.0
        self.ABSERR = 5e-6
        self.NPART = 10
        self.RECOIL = True
        self.Z1 = 5.0
        self.NTAB = 30000
        self.RRMIN = 0.0
        self.RRSTEP = 2.5e-3
        self.ZMIN = 0.0
        self.ZSTEP = 3e-4
        self.MAXDIV = 10
        self.MINDIV = 10

        self.NWRITX = 0.1
        self.NWRITY = 0.1
        self.FAX = 4
        self.FAY = 1
        self.NPAR = 4
        self.IPOT = 1
        self.POTPAR = [9125.84, 4.44, 66703.59, 11.24]
        self.NIMPAR = 2
        self.IIMPOT = 1
        self.PIMPAR = [1.26, 2.0]
        self.TEMP = 0.0
        self.SEED = 0.9436337324
        self.NITER = 1
        self.IMAGE = True
        self.SENRGY = -1.5
        self.BDIST = 8.18
        self.AX = 4.09
        self.AY = 4.09
        self.NBASIS = 1
        self.BASIS = [[0.0, 0.0, 0.0, 1]]
        self.NTYPES = 1
        self.ATOMS = [[107.87, 47]]
        self.SPRINGS = [[5.0, 5.0, 5.0]]
        self.CORR = True
        self.ATOMK = 0
        self.RNEIGH = 8.36405
        
        #No idea on good defaults for these
        self.NUMCHA = 0
        self.XSTART = 0
        self.XSTEP = 0
        self.YSTART = 0
        self.YSTEP = 0
        self.NBZ = 1
        self.TOL = 0
        self.NBG = 1
        self.GTOL = 0
        self.GMAX = [0]
        self.NG = [0]
        self.NZ = [0]
        self.ZMAX = [0]


        self.load()
        # Instead here we should check for a default file, and use that
        # if we do not have the requested input file.
        self.fileIn = 'modified.input'
        # This copies stuff from the original input to the modified one.
        self.save()
        return

    def load(self):
        if not os.path.exists(self.fileIn):
            return

        file = open(self.fileIn, 'r')
        # This starts at 1, so it matches the file lines.
        n = 1
        # This is used for offsetting the lines depending
        # on whether anything depends on entries before.
        o = 0

        for line in file:
            # The variable names here are identical to 
            # the ones used in the fortran source.
            
            args = parseLine(line)
            # Number of arguments, used for padding arrays with 0
            num = len(args)
            # E0 THETA0 PHI0 MASS
            if n == 1:
                self.E0 = args[0]
                self.THETA0 = args[1]
                self.PHI0 = args[2]
                self.MASS = args[3]
            #EMIN,EMAX,ESIZE,ASIZE
            if n == 2:
                self.EMIN = args[0]
                self.EMAX = args[1]
                self.ESIZE = args[2]
                self.ASIZE = args[3]
            # Detector type
            if n == 3:
                self.NDTECT = args[0]
            # Detector Params, 4 of them
            if n == 4:
                self.DTECTPAR = [args[0], args[1], args[2], args[3]]
            # min and max time steps
            if n == 5:
                self.DELLOW = args[0]
                self.DELT0 = args[1]
            # DEMAX,DEMIN,ABSERR
            if n == 6:
                self.DEMAX = args[0]
                self.DEMIN = args[1]
                self.ABSERR = args[2]
            # NPART - ATOMS ON SURFACE
            if n == 7:
                self.NPART = args[0]
            # Recoil
            if n == 8:
                self.RECOIL = args[0]
            # Interaction Height
            if n == 9:
                self.Z1 = args[0]
            # Table Entries
            if n == 10:
                self.NTAB = args[0]
            # R min and R step
            if n == 11:
                self.RRMIN = args[0]
                self.RRSTEP = args[1]
            # Z min and Z step
            if n == 12:
                self.ZMIN = args[0]
                self.ZSTEP = args[1]
            # Min and Max div, Here we check if o needs incrementing.
            if n == 13:
                if o <= 0:
                    self.MAXDIV = args[0]
                    self.MINDIV = args[1]
                    if self.MINDIV == self.MAXDIV and self.MAXDIV == 1:
                        # This will be decremented after leaving this if
                        o = 4
                else:
                    if o == 3:
                        self.NUMCHA = args[0]
                    if o == 2:
                        self.XSTART = args[0]
                        self.XSTEP = args[1]
                    if o == 1:
                        self.YSTART = args[0]
                        self.YSTEP = args[1]
            # NWRITX, NWRITY
            if n == 14:
                self.NWRITX = args[0]
                self.NWRITY = args[1]
            # FAX and FAY
            if n == 15:
                self.FAX = args[0]
                self.FAY = args[1]
            # NPAR and IPOT
            if n == 16:
                self.NPAR = args[0]
                self.IPOT = args[1]
            # Parameters for IPOT
            if n == 17:
                self.POTPAR = []
                for i in range(self.NPAR):
                    if i<num:
                        self.POTPAR.append(args[i])
                    else:
                        self.POTPAR.append(0)
            # NIMPAR and IIMPOT
            if n == 18:
                self.NIMPAR = args[0]
                self.IIMPOT = args[1]
            # Parameters for IIMPOT, as well as other stuff?
            if n == 19:
                if o <= 0:
                    self.PIMPAR = []
                    for i in range(self.NIMPAR):
                        if i<num:
                            self.PIMPAR.append(args[i])
                        else:
                            self.PIMPAR.append(0)
                    if self.IIMPOT == 2:
                        # This will be decremented after leaving this if
                        o = 7
                elif o == 6:
                    self.NBZ = args[0]
                    self.TOL = args[1]
                elif o == 5:
                    self.ZMAX = []
                    for i in range(self.NBZ):
                        if i<num:
                            self.ZMAX.append(args[i])
                        else:
                            self.ZMAX.append(0)
                elif o == 4:
                    self.NZ = []
                    for i in range(self.NBZ):
                        if i<num:
                            self.NZ.append(args[i])
                        else:
                            self.NZ.append(0)
                elif o == 3:
                    self.NBG = args[0]
                    self.GTOL = args[1]
                elif o == 2:
                    self.GMAX = []
                    for i in range(self.NBG):
                        if i<num:
                            self.GMAX.append(args[i])
                        else:
                            self.GMAX.append(0)
                elif o == 1:
                    self.NG = []
                    for i in range(self.NBG):
                        if i<num:
                            self.NG.append(args[i])
                        else:
                            self.NG.append(0)
            # Temp, Seed, NITER
            if n == 20:
                self.TEMP = args[0]
                self.SEED = args[1]
                self.NITER = args[2]
            # Use Image Charge
            if n == 21:
                self.IMAGE = args[0]
            # SENRGY and BDIST
            if n == 22:
                self.SENRGY = args[0]
                self.BDIST = args[1]
            # AX and AY
            if n == 23:
                self.AX = args[0]
                self.AY = args[1]
            # Load the basis cell
            if n == 24:
                # How many atoms in the basis
                if o <= 0:
                    self.NBASIS = args[0]
                    o = self.NBASIS + 1
                    self.BASIS = []
                # Load each atom
                else:
                    # X, Y, Z, Atom Type
                    site = [args[0], args[1], args[2], args[3]]
                    self.BASIS.append(site)
            # Number of atom types
            if n == 25:
                # Load number of atoms
                if o <= 0:
                    self.NTYPES = args[0]
                    self.ATOMS = []
                    self.SPRINGS = []
                    # 2 lines per atom.
                    o = self.NTYPES * 2 + 1
                # Load atom parameters
                else:
                    # Even entries are the atom
                    if o % 2 == 0:
                        # Mass and Charge
                        atom = [args[0], args[1]]
                        self.ATOMS.append(atom)
                    #Odd entries are the spring
                    else:
                        # Spring parameters, X, Y and Z
                        spring = [args[0], args[1], args[2]]
                        self.SPRINGS.append(spring)
            # CORR,ATOMK,RNEIGH
            if n == 26:
                self.CORR = args[0]
                self.ATOMK = args[1]
                self.RNEIGH = args[2]
                
            # Decrement our sub-line first.
            o = o - 1
            # Only increment this if not in a sub-line section.
            if o <= 0:
                n = n+1
        return

    def save(self):
        output = open(self.fileIn, 'w')

        file = serialize(self.E0, self.THETA0, self.PHI0, self.MASS) + '\n' \
            +  serialize(self.EMIN, self.EMAX, self.ESIZE, self.ASIZE)  + '\n' \
            +  serialize(self.NDTECT)  + '\n' \
            +  serializeArr(self.DTECTPAR)  + '\n' \
            +  serialize(self.DELLOW, self.DELT0)  + '\n' \
            +  serialize(self.DEMAX, self.DEMIN, self.ABSERR)  + '\n' \
            +  serialize(self.NPART)  + '\n' \
            +  serialize(self.RECOIL)  + '\n' \
            +  serialize(self.Z1)  + '\n' \
            +  serialize(self.NTAB)  + '\n' \
            +  serialize(self.RRMIN, self.RRSTEP)  + '\n' \
            +  serialize(self.ZMIN, self.ZSTEP)  + '\n' \
            +  serialize(self.MAXDIV, self.MINDIV)  + '\n'
        
        if self.MINDIV == self.MAXDIV and self.MAXDIV == 1:
            file = file \
            +  serialize(self.NUMCHA)  + '\n' \
            +  serialize(self.XSTART, self.XSTEP)  + '\n' \
            +  serialize(self.YSTART, self.YSTEP)  + '\n'
            
        file = file \
            +  serialize(self.NWRITX, self.NWRITY)  + '\n' \
            +  serialize(self.FAX, self.FAY)  + '\n' \
            +  serialize(self.NPAR, self.IPOT)  + '\n' \
            +  serializeArr(self.POTPAR)  + '\n' \
            +  serialize(self.NIMPAR, self.IIMPOT)  + '\n' \
            +  serializeArr(self.PIMPAR)  + '\n'
        
        if self.IIMPOT == 2:
            file = file \
            +  serialize(self.NBZ, self.TOL)  + '\n' \
            +  serializeArr(self.ZMAX)  + '\n' \
            +  serializeArr(self.NZ)  + '\n' \
            +  serialize(self.NBG, self.GTOL)  + '\n' \
            +  serializeArr(self.GMAX)  + '\n' \
            +  serializeArr(self.NG)  + '\n'
            
        file = file \
            +  serialize(self.TEMP, self.SEED, self.NITER)  + '\n' \
            +  serialize(self.IMAGE)  + '\n' \
            +  serialize(self.SENRGY, self.BDIST)  + '\n' \
            +  serialize(self.AX, self.AY)  + '\n' \
            +  serialize(self.NBASIS)  + '\n'
        
        for i in range(self.NBASIS):
            file = file + serializeArr(self.BASIS[i])  + '\n'
        
        file = file + serialize(self.NTYPES) + '\n'
        
        for i in range(self.NTYPES):
            atom = self.ATOMS[i]
            spring = self.SPRINGS[i]
            file = file \
            +  serializeArr(atom)  + '\n' \
            +  serializeArr(spring)  + '\n' \
        
        file = file + serialize(self.CORR, self.ATOMK, self.RNEIGH) + '\n'
            
        output.write(file)
        return
    
def makeDetectorParamsPopup(safari, layout, label, text, name, subindex):
    button = QPushButton(name)
    def edit():
        #Set it here so it doesn't get GC'd
        safari.popup = QWidget()
        window = safari.popup
        layout = QVBoxLayout()
        table = QTableWidget(1,subindex)
        # Populate the table
        for i in range(subindex):
            table.setItem(0,i,QTableWidgetItem(str(safari.DTECTPAR[i])))
        # Button to close the window
        close = QPushButton('Done')
        def done():
            # Update values from table.
            for i in range(subindex):
                safari.DTECTPAR[i]=parseVar(table.item(0,i).text())
            safari.save()
            window.close()
        close.clicked.connect(done)
        layout.addWidget(table)
        layout.addWidget(close)
        window.setLayout(layout)
        window.show()
    button.clicked.connect(edit)
    layout.addWidget(button)

def makeDIVSPopup(safari, layout, label, text, name, subindex):
    button = QPushButton(name)
    def edit():
        #Set it here so it doesn't get GC'd
        safari.popup = QWidget()
        window = safari.popup
        layout = QGridLayout()
        
        temp = QHBoxLayout()
        label = QLabel('NUMCHA')
        textn = QLineEdit(str(safari.NUMCHA))
        def edit():
            safari.NUMCHA = parseVar(textn.displayText())
            safari.save()
        textn.editingFinished.connect(edit)
        temp.addWidget(label)
        temp.addWidget(textn)
        layout.addLayout(temp, 0, 0)
        
        temp = QHBoxLayout()
        label = QLabel('XSTART')
        textxa = QLineEdit(str(safari.XSTART))
        def edit():
            safari.XSTART = parseVar(textxa.displayText())
            safari.save()
        textxa.editingFinished.connect(edit)
        temp.addWidget(label)
        temp.addWidget(textxa)
        layout.addLayout(temp, 10, 0)
        
        temp = QHBoxLayout()
        label = QLabel('XSTEP')
        textxf = QLineEdit(str(safari.XSTEP))
        def edit():
            safari.XSTEP = parseVar(textxf.displayText())
            safari.save()
        textxf.editingFinished.connect(edit)
        temp.addWidget(label)
        temp.addWidget(textxf)
        layout.addLayout(temp, 20, 0)
        
        temp = QHBoxLayout()
        label = QLabel('YSTART')
        textya = QLineEdit(str(safari.YSTART))
        def edit():
            safari.YSTART = parseVar(textya.displayText())
            safari.save()
        textya.editingFinished.connect(edit)
        temp.addWidget(label)
        temp.addWidget(textya)
        layout.addLayout(temp, 30, 0)
        
        temp = QHBoxLayout()
        label = QLabel('YSTEP')
        textyf = QLineEdit(str(safari.YSTEP))
        def edit():
            safari.YSTEP = parseVar(textyf.displayText())
            safari.save()
        textyf.editingFinished.connect(edit)
        temp.addWidget(label)
        temp.addWidget(textyf)
        layout.addLayout(temp, 40, 0)
        
        # Button to close the window
        close = QPushButton('Done')
        def done():
            window.close()
        close.clicked.connect(done)
        
        layout.addWidget(close)
        window.setLayout(layout)
        window.show()
    button.clicked.connect(edit)
    layout.addWidget(button)

def makePOTPARPopup(safari, layout, label, text, name, subindex):
    button = QPushButton(name)
    def edit():
        subindex = safari.NPAR
        #Set it here so it doesn't get GC'd
        safari.popup = QWidget()
        window = safari.popup
        layout = QVBoxLayout()
        table = QTableWidget(1,subindex)
        # Populate the table
        for i in range(subindex):
            table.setItem(0,i,QTableWidgetItem(str(safari.POTPAR[i])))
        # Button to close the window
        close = QPushButton('Done')
        def done():
            # Update values from table.
            for i in range(subindex):
                safari.POTPAR[i]=parseVar(table.item(0,i).text())
            safari.save()
            window.close()
        close.clicked.connect(done)
        layout.addWidget(table)
        layout.addWidget(close)
        window.setLayout(layout)
        window.show()
    button.clicked.connect(edit)
    layout.addWidget(button)

def makePIMPARPopup(safari, layout, label, text, name, subindex):
    button = QPushButton(name)
    def edit():
        subindex = safari.NIMPAR
        #Set it here so it doesn't get GC'd
        safari.popup = QWidget()
        window = safari.popup
        layout = QVBoxLayout()
        table = QTableWidget(1,subindex)
        # Populate the table
        for i in range(subindex):
            table.setItem(0,i,QTableWidgetItem(str(safari.PIMPAR[i])))
        # Button to close the window
        close = QPushButton('Done')
        def done():
            # Update values from table.
            for i in range(subindex):
                safari.PIMPAR[i]=parseVar(table.item(0,i).text())
            safari.save()
            window.close()
        close.clicked.connect(done)
        layout.addWidget(table)
        layout.addWidget(close)
        window.setLayout(layout)
        window.show()
    button.clicked.connect(edit)
    layout.addWidget(button)

def makeNBZPopup(safari, layout, label, text, name, subindex):
    button = QPushButton(name)
    def edit():
        subindex = safari.NBZ
        #Set it here so it doesn't get GC'd
        safari.popup4 = QWidget()
        window = safari.popup4
        layout = QVBoxLayout()
        table = QTableWidget(2,subindex)
        table.setVerticalHeaderLabels(['NZ:', 'ZMAX'])
        # Populate the table
        for i in range(subindex):
            table.setItem(0,i,QTableWidgetItem(str(safari.NZ[i])))
            table.setItem(1,i,QTableWidgetItem(str(safari.ZMAX[i])))
        # Button to close the window
        close = QPushButton('Done')
        def done():
            # Update values from table.
            for i in range(subindex):
                safari.NZ[i]=parseVar(table.item(0,i).text())
                safari.ZMAX[i]=parseVar(table.item(1,i).text())
            safari.save()
            window.close()
        close.clicked.connect(done)
        layout.addWidget(table)
        layout.addWidget(close)
        window.setLayout(layout)
        window.show()
    button.clicked.connect(edit)
    layout.addWidget(button)
    return

def makeNBGPopup(safari, layout, label, text, name, subindex):
    button = QPushButton(name)
    def edit():
        subindex = safari.NBG
        #Set it here so it doesn't get GC'd
        safari.popup3 = QWidget()
        window = safari.popup3
        layout = QVBoxLayout()
        print(subindex)
        table = QTableWidget(2,subindex)
        table.setVerticalHeaderLabels(['NG:', 'GMAX'])
        # Populate the table
        for i in range(subindex):
            table.setItem(0,i,QTableWidgetItem(str(safari.NG[i])))
            table.setItem(1,i,QTableWidgetItem(str(safari.GMAX[i])))
        # Button to close the window
        close = QPushButton('Done')
        def done():
            # Update values from table.
            for i in range(subindex):
                safari.NG[i]=parseVar(table.item(0,i).text())
                safari.GMAX[i]=parseVar(table.item(1,i).text())
            safari.save()
            window.close()
        close.clicked.connect(done)
        layout.addWidget(table)
        layout.addWidget(close)
        window.setLayout(layout)
        window.show()
    button.clicked.connect(edit)
    layout.addWidget(button)
    return

def makeIIMPOTIs2Popup(safari, layout, label, text, name, subindex):
    button = QPushButton('Used if IIMPOT is 2')
    def edit():
        #Set it here so it doesn't get GC'd
        safari.popup = QWidget()
        window = safari.popup
        layout = QGridLayout()
        print(safari.NBZ)
        temp = QHBoxLayout()
        label = QLabel('NBZ')
        textz = QLineEdit(str(safari.NBZ))
        def edit():
            safari.NBZ = parseVar(textz.displayText())
            # Pad the array.
            i = len(safari.NZ)
            while i < safari.NBZ:
                safari.NZ.append(0)
                safari.ZMAX.append(0)
                i = len(safari.NG)
            safari.save()
        textz.editingFinished.connect(edit)
        temp.addWidget(label)
        temp.addWidget(textz)
        layout.addLayout(temp, 0, 0)
        
        temp = QHBoxLayout()
        label = QLabel('TOL')
        textt = QLineEdit(str(safari.TOL))
        def edit():
            safari.TOL = parseVar(textt.displayText())
            safari.save()
        textt.editingFinished.connect(edit)
        temp.addWidget(label)
        temp.addWidget(textt)
        layout.addLayout(temp, 10, 0)

        # Popup table
        makeNBZPopup(safari, layout, label, text, 'Z values', subindex)
        
        temp = QHBoxLayout()
        label = QLabel('NBG')
        textg = QLineEdit(str(safari.NBG))
        def edit():
            safari.NBG = parseVar(textg.displayText())
            # Pad the array.
            i = len(safari.NG)
            while i < safari.NBG:
                safari.NG.append(0)
                safari.GMAX.append(0)
                i = len(safari.NG)
            safari.save()
        textg.editingFinished.connect(edit)
        temp.addWidget(label)
        temp.addWidget(textg)
        layout.addLayout(temp, 30, 0)
        
        temp = QHBoxLayout()
        label = QLabel('GTOL')
        textgt = QLineEdit(str(safari.GTOL))
        def edit():
            safari.GTOL = parseVar(textgt.displayText())
            safari.save()
        textgt.editingFinished.connect(edit)
        temp.addWidget(label)
        temp.addWidget(textgt)
        layout.addLayout(temp, 40, 0)

        # Popup table
        makeNBGPopup(safari, layout, label, text, 'G values', subindex)

        # Button to close the window
        close = QPushButton('Done')
        def done():
            window.close()
        close.clicked.connect(done)
        
        layout.addWidget(close)
        window.setLayout(layout)
        window.show()
    button.clicked.connect(edit)
    layout.addWidget(button)

def makeBasisPopup(safari, layout, label, text, name, subindex):
    button = QPushButton('Edit Basis')
    def edit():
        subindex = safari.NBASIS
        #Set it here so it doesn't get GC'd
        safari.popup = QWidget()
        window = safari.popup
        layout = QVBoxLayout()
        table = QTableWidget(subindex,4)
        table.setHorizontalHeaderLabels(['X', 'Y', 'Z', 'Type'])
        # Populate the table
        for i in range(subindex):
            site = safari.BASIS[i]
            for j in range(4):
                table.setItem(i,j,QTableWidgetItem(str(site[j])))
        # Button to close the window
        close = QPushButton('Done')
        def done():
            # Update values from table.
            for i in range(subindex):
                site = safari.BASIS[i]
                for j in range(4):
                    site[j] = parseVar(table.item(i,j).text())
            safari.save()
            window.close()
        close.clicked.connect(done)
        layout.addWidget(table)
        layout.addWidget(close)
        window.setLayout(layout)
        window.show()
    button.clicked.connect(edit)
    layout.addWidget(button)

def makeAtomsPopup(safari, layout, label, text, name, subindex):
    button = QPushButton('Edit Atoms')
    def edit():
        subindex = safari.NTYPES
        #Set it here so it doesn't get GC'd
        safari.popup = QWidget()
        window = safari.popup
        layout = QVBoxLayout()
        table = QTableWidget(subindex,5)
        table.setHorizontalHeaderLabels(['Mass', 'Charge', 'SX', 'SY', 'SZ'])
        # Populate the table
        for i in range(subindex):
            atom = safari.ATOMS[i]
            spring = safari.SPRINGS[i]
            table.setItem(i,0,QTableWidgetItem(str(atom[0])))
            table.setItem(i,1,QTableWidgetItem(str(atom[1])))
            table.setItem(i,2,QTableWidgetItem(str(spring[0])))
            table.setItem(i,3,QTableWidgetItem(str(spring[1])))
            table.setItem(i,4,QTableWidgetItem(str(spring[2])))
            
        # Button to close the window
        close = QPushButton('Done')
        def done():
            # Update values from table.
            for i in range(subindex):
                atom = safari.ATOMS[i]
                spring = safari.SPRINGS[i]
                atom[0] = parseVar(table.item(i,0).text())
                atom[1] = parseVar(table.item(i,1).text())
                spring[0] = parseVar(table.item(i,2).text())
                spring[1] = parseVar(table.item(i,3).text())
                spring[2] = parseVar(table.item(i,4).text())
            safari.save()
            window.close()
        close.clicked.connect(done)
        layout.addWidget(table)
        layout.addWidget(close)
        window.setLayout(layout)
        window.show()
    button.clicked.connect(edit)
    layout.addWidget(button)

def makeInputBox(safari, name, index, subindex):
    layout = QHBoxLayout()
    label = QLabel(name)
    text = QLineEdit()
    #Some values do their own custom stuff, in that case, this is set False
    hastext = True
    #Add the label first
    layout.addWidget(label)
    
    # First line of the file.
    if index==1:
        if subindex==0:
            text = QLineEdit(str(safari.E0))
            def edit():
                safari.E0 = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
        if subindex==1:
            text = QLineEdit(str(safari.THETA0))
            def edit():
                safari.THETA0 = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
        if subindex==2:
            text = QLineEdit(str(safari.PHI0))
            def edit():
                safari.PHI0 = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
        if subindex==3:
            text = QLineEdit(str(safari.MASS))
            def edit():
                safari.MASS = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
    # Second line of the file
    if index==2:
        if subindex==0:
            text = QLineEdit(str(safari.EMIN))
            def edit():
                safari.EMIN = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
        if subindex==1:
            text = QLineEdit(str(safari.EMAX))
            def edit():
                safari.EMAX = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
        if subindex==2:
            text = QLineEdit(str(safari.ESIZE))
            def edit():
                safari.ESIZE = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
        if subindex==3:
            text = QLineEdit(str(safari.ASIZE))
            def edit():
                safari.ASIZE = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
    # Detector type, should be replaced with dropdown list later.
    if index==3:
        text = QLineEdit(str(safari.NDTECT))
        def edit():
            safari.NDTECT = parseVar(text.displayText())
            safari.save()
        text.editingFinished.connect(edit)
    # Parameters for the detector
    if index==4:
        layout.removeWidget(label)
        makeDetectorParamsPopup(safari, layout, label, text, name, subindex)
        hastext = False
    # Time Steps
    if index==5:
        if subindex==0:
            text = QLineEdit(str(safari.DELLOW))
            def edit():
                safari.DELLOW = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
        if subindex==1:
            text = QLineEdit(str(safari.DELT0))
            def edit():
                safari.DELT0 = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
    if index==6:
        if subindex==0:
            text = QLineEdit(str(safari.DEMAX))
            def edit():
                safari.DEMAX = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
        if subindex==1:
            text = QLineEdit(str(safari.DEMIN))
            def edit():
                safari.DEMIN = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
        if subindex==2:
            text = QLineEdit(str(safari.ABSERR))
            def edit():
                safari.ABSERR = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
    if index==7:
        text = QLineEdit(str(safari.NPART))
        def edit():
            safari.NPART = parseVar(text.displayText())
            safari.save()
        text.editingFinished.connect(edit)
    if index==8:
        button = QRadioButton()
        button.setChecked(safari.RECOIL)
        def push():
            safari.RECOIL = button.isChecked()
            safari.save()
        button.toggled.connect(push)
        hastext = False
        layout.addWidget(button)
    if index==9:
        text = QLineEdit(str(safari.Z1))
        def edit():
            safari.Z1 = parseVar(text.displayText())
            safari.save()
        text.editingFinished.connect(edit)
    if index==10:
        text = QLineEdit(str(safari.NTAB))
        def edit():
            safari.NTAB = parseVar(text.displayText())
            safari.save()
        text.editingFinished.connect(edit)
    if index==11:
        if subindex==0:
            text = QLineEdit(str(safari.RRMIN))
            def edit():
                safari.RRMIN = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
        if subindex==1:
            text = QLineEdit(str(safari.RRSTEP))
            def edit():
                safari.RRSTEP = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
    if index==12:
        if subindex==0:
            text = QLineEdit(str(safari.ZMIN))
            def edit():
                safari.ZMIN = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
        if subindex==1:
            text = QLineEdit(str(safari.ZSTEP))
            def edit():
                safari.ZSTEP = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
    if index==13:
        if subindex==0:
            text = QLineEdit(str(safari.MAXDIV))
            def edit():
                safari.MAXDIV = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
        if subindex==1:
            text = QLineEdit(str(safari.MINDIV))
            def edit():
                safari.MINDIV = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
        if subindex==2:
            layout.removeWidget(label)
            makeDIVSPopup(safari, layout, label, text, name, subindex)
            hastext = False
    if index==14:
        if subindex==0:
            text = QLineEdit(str(safari.NWRITX))
            def edit():
                safari.NWRITX = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
        if subindex==1:
            text = QLineEdit(str(safari.NWRITY))
            def edit():
                safari.NWRITY = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
    if index==15:
        if subindex==0:
            text = QLineEdit(str(safari.FAX))
            def edit():
                safari.FAX = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
        if subindex==1:
            text = QLineEdit(str(safari.FAY))
            def edit():
                safari.FAY = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
    if index==16:
        if subindex==0:
            text = QLineEdit(str(safari.NPAR))
            def edit():
                safari.NPAR = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
        if subindex==1:
            # This should be replaced with a dropdown list later.
            text = QLineEdit(str(safari.IPOT))
            def edit():
                safari.IPOT = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
    #Parameters for image potential
    if index==17:
        layout.removeWidget(label)
        makePOTPARPopup(safari, layout, label, text, name, subindex)
        hastext = False
    if index==18:
        if subindex==0:
            text = QLineEdit(str(safari.NIMPAR))
            def edit():
                safari.NIMPAR = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
        if subindex==1:
            # This should be replaced with a dropdown list later.
            text = QLineEdit(str(safari.IIMPOT))
            def edit():
                safari.IIMPOT = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
    #Parameters for image potential
    if index==19:
        layout.removeWidget(label)
        makePIMPARPopup(safari, layout, label, text, name, subindex)
        makeIIMPOTIs2Popup(safari, layout, label, text, name, subindex)
        hastext = False
    if index==20:
        if subindex==0:
            text = QLineEdit(str(safari.TEMP))
            def edit():
                safari.TEMP = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
        if subindex==1:
            text = QLineEdit(str(safari.SEED))
            def edit():
                safari.SEED = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
        if subindex==2:
            text = QLineEdit(str(safari.NITER))
            def edit():
                safari.NITER = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
    if index==21:
        button = QRadioButton()
        def push():
            safari.IMAGE = button.isChecked()
            safari.save()
        button.setChecked(safari.IMAGE)
        button.toggled.connect(push)
        hastext = False
        layout.addWidget(button)
    if index==22:
        if subindex==0:
            text = QLineEdit(str(safari.SENRGY))
            def edit():
                safari.SENRGY = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
        if subindex==1:
            text = QLineEdit(str(safari.BDIST))
            def edit():
                safari.BDIST = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
    if index==23:
        if subindex==0:
            text = QLineEdit(str(safari.AX))
            def edit():
                safari.AX = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
        if subindex==1:
            text = QLineEdit(str(safari.AY))
            def edit():
                safari.AY = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
    if index==24:
        text = QLineEdit(str(safari.NBASIS))
        def edit():
            safari.NBASIS = parseVar(text.displayText())
            # Pad the array.
            i = len(safari.BASIS)
            while i < safari.NBASIS:
                safari.BASIS.append([0.0,0.0,0.0,0])
                i = len(safari.BASIS)
            safari.save()
        text.editingFinished.connect(edit)
        #We add extra stuff, so manually add text here.
        hastext=False
        layout.addWidget(text)
        makeBasisPopup(safari, layout, label, text, name, subindex)
    if index==25:
        text = QLineEdit(str(safari.NTYPES))
        def edit():
            safari.NTYPES = parseVar(text.displayText())
            # Pad the arrays.
            i = len(safari.ATOMS)
            while i < safari.NTYPES:
                safari.ATOMS.append([0.0,0])
                safari.SPRINGS.append([0.0,0.0,0.0])
                i = len(safari.ATOMS)
            safari.save()
        text.editingFinished.connect(edit)
        #We add extra stuff, so manually add text here.
        hastext=False
        layout.addWidget(text)
        makeAtomsPopup(safari, layout, label, text, name, subindex)
    if index==26:
        if subindex==0:
            button = QRadioButton()
            def push():
                safari.CORR = button.isChecked()
                safari.save()
            button.setChecked(safari.CORR)
            button.toggled.connect(push)
            hastext = False
            layout.addWidget(button)
        if subindex==1:
            text = QLineEdit(str(safari.ATOMK))
            def edit():
                safari.ATOMK = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
        if subindex==2:
            text = QLineEdit(str(safari.RNEIGH))
            def edit():
                safari.RNEIGH = parseVar(text.displayText())
                safari.save()
            text.editingFinished.connect(edit)
        
    # Most things use text box
    if hastext:
        layout.addWidget(text)
        
    return layout

def make_Button(name, click):
    button = QPushButton(name);
    button.clicked.connect(click)
    return button
    
def makeApplication(safari):
    app = QApplication([])
    window = QWidget()
    layout = QGridLayout()
    
    x = 0
    y = 0
    
    box = makeInputBox(safari, 'Initial Energy', 1, 0)
    layout.addLayout(box,x,y + 00)
    box = makeInputBox(safari, 'Initial Theta', 1, 1)
    layout.addLayout(box,x,y + 10)
    box = makeInputBox(safari, 'Initial Phi', 1, 2)
    layout.addLayout(box,x,y + 20)
    box = makeInputBox(safari, 'Projectile Mass', 1, 3)
    layout.addLayout(box,x,y + 30)
    
    x += 10
    box = makeInputBox(safari, 'Minimum Energy', 2, 0)
    layout.addLayout(box,x,y + 00)
    box = makeInputBox(safari, 'Maximum Energy', 2, 1)
    layout.addLayout(box,x,y + 10)
    box = makeInputBox(safari, 'ESize?', 2, 2)
    layout.addLayout(box,x,y + 20)
    box = makeInputBox(safari, 'ASize?', 2, 3)
    layout.addLayout(box,x,y + 30)
    
    x += 10
    # This should be replaced with a dropdown selector.
    box = makeInputBox(safari, 'Detector Type', 3, 0)
    layout.addLayout(box,x,y + 00)
    # Detector parameters on same row.
    box = makeInputBox(safari, 'Detector Parameters', 4, 4)
    layout.addLayout(box,x,y + 10)
    
    x+=10
    box = makeInputBox(safari, 'Min Time Step', 5, 0)
    layout.addLayout(box,x,y + 00)
    box = makeInputBox(safari, 'Max Time Step', 5, 1)
    layout.addLayout(box,x,y + 10)
    
    x+=10
    box = makeInputBox(safari, 'DEMAX - Max Energy Diff?', 6, 0)
    layout.addLayout(box,x,y + 00)
    box = makeInputBox(safari, 'DEMIN - Min Energy Diff?', 6, 1)
    layout.addLayout(box,x,y + 10)
    box = makeInputBox(safari, 'ABSERR?', 6, 2)
    layout.addLayout(box,x,y + 20)
    
    x+=10
    #Next 4 values on the same row.
    box = makeInputBox(safari, 'Atoms On Surface', 7, 0)
    layout.addLayout(box,x,y + 00)
    box = makeInputBox(safari, 'Recoil', 8, 0)
    layout.addLayout(box,x,y + 30)
    box = makeInputBox(safari, 'Interaction Height', 9, 0)
    layout.addLayout(box,x,y + 10)
    box = makeInputBox(safari, 'Table Entries', 10, 0)
    layout.addLayout(box,x,y + 20)
    
    x+=10
    box = makeInputBox(safari, 'R Min', 11, 0)
    layout.addLayout(box,x,y + 00)
    box = makeInputBox(safari, 'R Step', 11, 1)
    layout.addLayout(box,x,y + 10)
    box = makeInputBox(safari, 'Z Min', 12, 0)
    layout.addLayout(box,x,y + 20)
    box = makeInputBox(safari, 'Z Step', 12, 1)
    layout.addLayout(box,x,y + 30)
    
    x+=10
    box = makeInputBox(safari, 'MAXDIV', 13, 0)
    layout.addLayout(box,x,y + 00)
    box = makeInputBox(safari, 'MINDIV', 13, 1)
    layout.addLayout(box,x,y + 10)
    box = makeInputBox(safari, 'Extra, Used if MAXDIV==MINDIV==1', 13, 2)
    layout.addLayout(box,x,y + 20)
    
    x+=10
    box = makeInputBox(safari, 'NWRITX', 14, 0)
    layout.addLayout(box,x,y + 00)
    box = makeInputBox(safari, 'NWRITY', 14, 1)
    layout.addLayout(box,x,y + 10)
    
    x+=10
    box = makeInputBox(safari, 'FAX', 15, 0)
    layout.addLayout(box,x,y + 00)
    box = makeInputBox(safari, 'FAY', 15, 1)
    layout.addLayout(box,x,y + 10)
    
    x+=10
    box = makeInputBox(safari, 'Image Potential Number', 16, 0)
    layout.addLayout(box,x,y + 00)
    box = makeInputBox(safari, 'Image Potential', 16, 1)
    layout.addLayout(box,x,y + 10)
    box = makeInputBox(safari, 'Image Potential Parameters', 17, 0)
    layout.addLayout(box,x,y + 20)
    box = makeInputBox(safari, 'Use Image Charge', 21, 0)
    layout.addLayout(box,x,y + 30)
    
    x+=10
    box = makeInputBox(safari, 'NImage Potential Number', 18, 0)
    layout.addLayout(box,x,y + 00)
    box = makeInputBox(safari, 'NImage Potential', 18, 1)
    layout.addLayout(box,x,y + 10)
    box = makeInputBox(safari, 'NImage Potential Parameters', 19, 0)
    layout.addLayout(box,x,y + 20)
    # I need to also include a button for making a popup for the sub-values.
    
    x+=10
    box = makeInputBox(safari, 'Temp', 20, 0)
    layout.addLayout(box,x,y + 00)
    box = makeInputBox(safari, 'Seed', 20, 1)
    layout.addLayout(box,x,y + 10)
    box = makeInputBox(safari, 'NITER', 20, 2)
    layout.addLayout(box,x,y + 20)
    
    x+=10
    box = makeInputBox(safari, 'Stuck Energy', 22, 0)
    layout.addLayout(box,x,y + 00)
    box = makeInputBox(safari, 'Buried Distance', 22, 1)
    layout.addLayout(box,x,y + 10)
    
    x+=10
    box = makeInputBox(safari, 'x Lattice Constant', 23, 0)
    layout.addLayout(box,x,y + 00)
    box = makeInputBox(safari, 'y Lattice Constant', 23, 1)
    layout.addLayout(box,x,y + 10)
    
    x+=10
    box = makeInputBox(safari, 'Basis Size', 24, 0)
    layout.addLayout(box,x,y + 00)
    
    x+=10
    box = makeInputBox(safari, 'Basis Types', 25, 0)
    layout.addLayout(box,x,y + 00)
    
    x+=10
    box = makeInputBox(safari, 'CORR', 26, 0)
    layout.addLayout(box,x,y + 00)
    box = makeInputBox(safari, 'ATOMK', 26, 1)
    layout.addLayout(box,x,y + 10)
    box = makeInputBox(safari, 'RNEIGH', 26, 2)
    layout.addLayout(box,x,y + 20)
    
    #Make a button for running safari
    run = QPushButton('Run')
    text = QLineEdit('notepad.exe')
    def push():
        #Replace with whatever for running safari later.
        subprocess.call(text.displayText().split())
        print(text.displayText())
    run.clicked.connect(push)
    
    box = QVBoxLayout()
    box.addWidget(text)
    box.addWidget(run)
    x+=10
    layout.addLayout(box, x, y + 00)
    
    window.setLayout(layout)
    window.show()
    app.exec_()

if __name__ == '__main__':
    safari = SafariInput('./sample.input')
    makeApplication(safari)
