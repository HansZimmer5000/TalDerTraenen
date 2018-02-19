
import os
import sys
import glob
import time

# Compiles all .erl files in the current directory that have changed since last compiling.
def __make_all_modules():
    COMMAND = "erl -make"
    os.system(COMMAND)


# Execute the tests of the given module.
def __test_module(modulename):
    print(modulename)
    os.system("erl -noshell -s " + modulename + " test -run init stop")


# Opens a new Commandline window (cmd.exe) and starts a new erlang node with the given name and modulecode. 
# The module must contain a "start" function without arguments.
def __start_node(nodename, modulename):
    os.system("start erl -noshell -sname " + nodename + " -s " + modulename + " start")

# Clears a certain file (filename) of any content.
def __clear_file(filename):
    open(filename, "w").close()

def __clear_all_log_files_in_current_dir():
    log_filenames = glob.glob("*.log")
    for log_filename in log_filenames:
        __clear_file(log_filename)

# According to given Input, either all Testfiles gonna be executed or 
# the client, server and hbq erlang node + server will be started.
if __name__ == "__main__":
    try: 
        user_input = sys.argv[1]
    except IndexError:
        user_input = "0"
    
    if user_input == "0":
        __make_all_modules()
    elif user_input == "1":
        __make_all_modules()
        __test_module("testnameservice")
        __test_module("testggtprozess")
    elif user_input == "2":
        __make_all_modules()
        __clear_all_log_files_in_current_dir()
    else:
        print("Argument: '" + user_input + "' unkonwn.")