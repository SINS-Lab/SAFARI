import os
import subprocess
import time

#from PyQt5.QtWidgets import QWidget, QApplication
#from PyQt5.QtWidgets import QGridLayout, QHBoxLayout, QVBoxLayout, QComboBox
#from PyQt5.QtWidgets import QLineEdit, QLabel, QPushButton

def run(threads, directory='.'):
    processes = []
    number = 1
    for filename in os.listdir(directory):
        if filename.endswith(".input") and not filename==('safari.input'):
            saf_file = open('safari.input', 'w')
            saf_file.write(filename.replace('.input', ''))
            saf_file.close()
            process = subprocess.Popen('Safari.exe', shell=True)
            processes.append(process)
            #Wait a second for it to start running
            time.sleep(1)
            number = number + 1
            # If hit number to run, wait for all runs to finish, before continuing.
            if number > threads:
                for p in processes:
                    p.join()
                processes = []
                number = 1
    


#def run():
#    app = QApplication([])
#    window = QWidget()
#    layout = QGridLayout()
#    
#    # Button to close the window
#    close = QPushButton('Close')
#    def done():
#        window.close()
#    close.clicked.connect(done)
#    layout.addWidget(close)
#    window.setLayout(layout)
#    window.setWindowTitle('Detect')
#    window.show()
#    app.exec_()
    

if __name__ == '__main__':
#    run()
    txtnum = input("Number of Threads? ")
