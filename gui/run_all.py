import os
import subprocess
import time

def run(threads, directory='.'):
    processes = []
    number = 1
    
    if directory!='.':
        directory = os.path.join('.',directory)
    
    for filename in os.listdir(directory):
        if filename.endswith(".input") and not filename==('safari.input'):
            file = os.path.join(directory, filename).replace('.input','')
            saf_file = open('safari.input', 'w')
            saf_file.write(file)
            saf_file.close()
            print('Running: '+filename)
            process = subprocess.Popen('Safari.exe', shell=True)
            processes.append(process)
            #Wait a second for it to start running
            time.sleep(1)
            number = number + 1
            # If hit number to run, wait for all runs to finish, before continuing.
            if number > threads:
                for p in processes:
                    p.wait()
                processes = []
                number = 1
if __name__ == '__main__':
#    run()
    txtnum = input("Number of Threads? ")
    rundir = input("Run Directory? ")
    threads = int(txtnum)
    run(threads, directory=rundir)