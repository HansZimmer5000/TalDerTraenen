
import os
import sys
import glob
import time

#TODO: Test file muss dieses include_lib enthalten und kann nach compilen mit "MODULENAME:test()." getestet werden, geht auch in bash mit "erl -noshell -s MODULENAME test"

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

# Deletes all files of the defined types in the current folder
def __remove_all_unecessary_files(unecessary_file_types):
    unecessary_files = []
    for unecessary_file_type in unecessary_file_types:
        unecessary_files = unecessary_files + glob.glob("*" + unecessary_file_type)
    for unecessary_file in unecessary_files:
        os.remove(unecessary_file)

# According to given Input, either all Testfiles gonna be executed or 
# the client, server and hbq erlang node + server will be started.
if __name__ == "__main__":
    try: 
        user_input = sys.argv[1]
    except IndexError:
        user_input = "0"
    
    if user_input == "0":
        __make_all_modules()
        __remove_all_unecessary_files([".beam"])
    elif user_input == "1":
        __make_all_modules()
        __test_module("testallgemein")
        __test_module("testclient")
        __test_module("testserver")
        __test_module("testcmem")
        __test_module("testhbq")
        __test_module("testdlq")
        __remove_all_unecessary_files([".beam"])
    elif user_input == "2":
        __make_all_modules()
        __clear_all_log_files_in_current_dir()
        __start_node("hbq", "hbq")
        time.sleep(1)
        __start_node("server", "server")
        time.sleep(1)
        __start_node("client", "client")
    elif user_input == "3":
        __remove_all_unecessary_files([".beam", ".dump", ".log"])
    else:
        print("Argument: '" + user_input + "' unkonwn.")