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

from PyQt5.QtWidgets import QWidget, QApplication
from PyQt5.QtWidgets import QGridLayout, QHBoxLayout, QVBoxLayout
from PyQt5.QtWidgets import QLineEdit, QLabel, QPushButton, QRadioButton
from PyQt5.QtWidgets import QTableWidget, QTableWidgetItem
from safari_input import SafariInput
from safari_input import parseVar
import subprocess
import sys

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
        label = QLabel('Shot Count')
        textn = QLineEdit(str(safari.NUMCHA))
        def edit():
            safari.NUMCHA = parseVar(textn.displayText())
            safari.save()
        textn.editingFinished.connect(edit)
        temp.addWidget(label)
        temp.addWidget(textn)
        layout.addLayout(temp, 0, 0)
        
        temp = QHBoxLayout()
        label = QLabel('Minimum X')
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
        label = QLabel('Minimum Y')
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
        table = QTableWidget(subindex,6)
        table.setHorizontalHeaderLabels(['Mass', 'Charge', 'Symbol',\
                                         'SX', 'SY', 'SZ'])
        # Populate the table
        for i in range(subindex):
            atom = safari.ATOMS[i]
            spring = safari.SPRINGS[i]
            table.setItem(i,0,QTableWidgetItem(str(atom[0])))
            table.setItem(i,1,QTableWidgetItem(str(atom[1])))
            table.setItem(i,2,QTableWidgetItem(str(atom[2])))
            table.setItem(i,3,QTableWidgetItem(str(spring[0])))
            table.setItem(i,4,QTableWidgetItem(str(spring[1])))
            table.setItem(i,5,QTableWidgetItem(str(spring[2])))
            
        # Button to close the window
        close = QPushButton('Done')
        def done():
            # Update values from table.
            for i in range(subindex):
                atom = safari.ATOMS[i]
                spring = safari.SPRINGS[i]
                atom[0] = parseVar(table.item(i,0).text())
                atom[1] = parseVar(table.item(i,1).text())
                new = parseVar(table.item(i,2).text())
                old = atom[2]
                if new != old:
                    print('Should update mass an charge from table')
                atom[2] =  new
                spring[0] = parseVar(table.item(i,3).text())
                spring[1] = parseVar(table.item(i,4).text())
                spring[2] = parseVar(table.item(i,5).text())
            safari.save()
            window.close()
        close.clicked.connect(done)
        layout.addWidget(table)
        layout.addWidget(close)
        window.setLayout(layout)
        window.show()
    button.clicked.connect(edit)
    layout.addWidget(button)

def makeInputBox(safari, name, index, subindex, _layout=None):
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
        if subindex==4:
            text = QLineEdit(str(safari.SYMION))
            def edit():
                safari.SYMION = parseVar(text.displayText())
                # TODO if this is set, it should then update
                # Mass from a lookup table.
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
        button.clicked.connect(push)
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
        button.setChecked(safari.IMAGE)
        def push():
            safari.IMAGE = button.isChecked()
            safari.save()
        button.clicked.connect(push)
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
            button.setChecked(safari.CORR)
            def push():
                safari.CORR = button.isChecked()
                safari.save()
            button.clicked.connect(push)
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
            
    if index==27:
        layout.removeWidget(label)
        buttonMC = QRadioButton('Montecarlo')
        buttonMC.setChecked(safari.isMonteCarlo())
        buttonGC = QRadioButton('GridScat')
        buttonGC.setChecked(safari.isGridScat())
        def pushMC():
            if safari.isGridScat():
                safari.setGridScat(False)
                buttonGC.setChecked(False)
            safari.setMonteCarlo(buttonMC.isChecked())
            safari.save()
            safari.load()
            initBoxes(safari, _layout)
        def pushGC():
            if safari.isMonteCarlo():
                safari.setMonteCarlo(False)
                buttonMC.setChecked(False)
            safari.setGridScat(buttonGC.isChecked())
            safari.save()
            safari.load()
            initBoxes(safari, _layout)
        buttonMC.clicked.connect(pushMC)
        layout.addWidget(buttonMC)
        buttonGC.clicked.connect(pushGC)
        layout.addWidget(buttonGC)
        hastext = False
        
        
    # Most things use text box
    if hastext:
        layout.addWidget(text)
        
    return layout

def make_Button(name, click):
    button = QPushButton(name);
    button.clicked.connect(click)
    return button

def clearLayout(layout):
    if layout != None:
        while layout.count():
            child = layout.takeAt(0)
            if child.widget() is not None:
                child.widget().deleteLater()
            elif child.layout() is not None:
                clearLayout(child.layout())

def initBoxes(safari, layout):
    clearLayout(layout)
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
    box = makeInputBox(safari, 'Projectile Symbol', 1, 4)
    layout.addLayout(box,x,y + 40)
    
    x += 10
    box = makeInputBox(safari, 'Minimum Energy', 2, 0)
    layout.addLayout(box,x,y + 00)
    box = makeInputBox(safari, 'Maximum Energy', 2, 1)
    layout.addLayout(box,x,y + 10)
    box = makeInputBox(safari, 'Energy Resolution', 2, 2)
    layout.addLayout(box,x,y + 20)
    box = makeInputBox(safari, 'Angular Resolution', 2, 3)
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
    box = makeInputBox(safari, 'Euler Error Max', 6, 0)
    layout.addLayout(box,x,y + 00)
    box = makeInputBox(safari, 'Euler Error Min', 6, 1)
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
    box = makeInputBox(safari, 'Impact Parameters', 13, 2)
    layout.addLayout(box,x,y + 20)
    
    x+=10
    box = makeInputBox(safari, 'NWRITX', 14, 0)
    layout.addLayout(box,x,y + 00)
    box = makeInputBox(safari, 'NWRITY', 14, 1)
    layout.addLayout(box,x,y + 10)
    
    x+=10
    box = makeInputBox(safari, 'Maximum X (Fraction of Lattice)', 15, 0)
    layout.addLayout(box,x,y + 00)
    box = makeInputBox(safari, 'Maximum Y (Fraction of Lattice)', 15, 1)
    layout.addLayout(box,x,y + 10)
    
    x+=10
    box = makeInputBox(safari, 'Lattice Potential Number', 16, 0)
    layout.addLayout(box,x,y + 00)
    box = makeInputBox(safari, 'Lattice Potential', 16, 1)
    layout.addLayout(box,x,y + 10)
    box = makeInputBox(safari, 'Lattice Potential Parameters', 17, 0)
    layout.addLayout(box,x,y + 20)
    box = makeInputBox(safari, 'Use Image Charge', 21, 0)
    layout.addLayout(box,x,y + 30)
    
    x+=10
    box = makeInputBox(safari, 'Image Potential Number', 18, 0)
    layout.addLayout(box,x,y + 00)
    box = makeInputBox(safari, 'Image Potential', 18, 1)
    layout.addLayout(box,x,y + 10)
    box = makeInputBox(safari, 'Image Potential Parameters', 19, 0)
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
    
    x+=10
    box = makeInputBox(safari, 'Monte Carlo', 27, 0, _layout=layout)
    layout.addLayout(box,x,y + 00)
    
    #Make a button for running safari
    run = QPushButton('Run')
    text = QLineEdit('Safari.exe')
    def push():
        print('Generating Safari Input')
        safari.genInputFile()
        print('Running Safari')
        subprocess.Popen(text.displayText().split(), shell=True)
        
    run.clicked.connect(push)
    
    box = QVBoxLayout()
    box.addWidget(text)
    box.addWidget(run)
    x+=10
    layout.addLayout(box, x, y + 00)
    
def makeApplication(safari):
    app = QApplication([])
    window = QWidget()
    layout = QGridLayout()
    
    initBoxes(safari, layout)
    
    window.setLayout(layout)
    window.show()
    sys.exit(app.exec_())

if __name__ == '__main__':
    safari = SafariInput('sample.input')
    makeApplication(safari)
