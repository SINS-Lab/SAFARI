
# Safari Installation

-    Install mingw-get
-    use mingw-get to install gnu fortran compiler: mingw32-gcc-fortran-bin, mingw23-base-bin
-        Install to <*>\MinGW folder
-    Add <*>\MinGW\bin to the path
-    Install Codeblocks - unselect MinGW compiler suite
-    Run Codeblocks, should find the fortran compiler from mingw

The above works fine on Windows 10. IF you have windows 7 you will have some problems. 
You will want to install MinGW on the desktop temporarily. After Codeblocks is installed
then put the MinGW folder intot he Codeblocks directory. If you have 32 bit its just
in program files. if you ahve 64 bit you will want to install codeblocks in the (x86)
directory area. 



# To Run detect_gui and safari_gui, if these modules are not installed.

-    run `pip install pyqt5`
-    run `pip install scipy`
-    run `pip install matplotlib`