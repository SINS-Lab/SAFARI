import os
import subprocess
import time
import platform

def run(threads, directory='.', recursive=True):
    processes = []
    number = 1
    
    command = 'Safari.exe'
    if platform.system() == 'Linux':
        command = './Safari'
    
    for filename in os.listdir(directory):
        truefile = os.path.join(directory, filename)
        if recursive and os.path.isdir(truefile):
            run(threads, truefile)
            continue
        if filename.endswith(".input") and not filename==('safari.input'):
            file = os.path.join(directory, filename).replace('.input','')
            saf_file = open('safari.input', 'w')
            saf_file.write(file)
            saf_file.close()
            print('Running: '+filename)
            process = subprocess.Popen(command, shell=True)
            processes.append(process)
            #Wait a second for it to start running
            time.sleep(1)
            number = number + 1
            # If hit number to run, wait for a run to finish, before continuing.
            while number > threads:
                for p in processes:
                    p.poll()
                    if p.returncode != None:
                        number = number - 1
                        processes.remove(p)
                        break
                time.sleep(1)
                    
                    
if __name__ == '__main__':
#    run()
    txtnum = input("Number of Threads? ")
    rundir = input("Run Directory? ")
    threads = int(txtnum)
    if rundir!='.':
        rundir = os.path.join('.',rundir)
    run(threads, directory=rundir)