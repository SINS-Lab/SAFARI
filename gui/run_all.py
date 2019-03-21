import os
import subprocess
import time

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
if __name__ == '__main__':
#    run()
    txtnum = input("Number of Threads? ")
    rundir = input("Run Directory? ")
    threads = int(txtnum)
    run(threads, directory=rundir)