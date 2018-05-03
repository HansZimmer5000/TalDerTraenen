
import os
import sys
import glob
import time

# Compiles all .erl files in the current directory that have changed since last compiling.
def __make_all_modules():
    COMMAND = "erl -make"
    os.system(COMMAND)

# Looks in current folder for files macthing "test*.erl"
def __get_all_test_modulenames():
    return glob.glob("test*.erl")

# Execute the tests of the given module.
def __test_module(modulename):
    print(modulename)
    os.system("erl -noshell -s " + modulename + " test -run init stop")


# Opens a new Commandline window (cmd.exe) and starts a new erlang node with the given name and modulecode. 
# The module must contain a "start" function without arguments.
def __start_node(nodename, modulename, parameter):
    os.system("start erl -noshell -sname " + nodename + " -s " + modulename + " start" + " " + parameter)

def __start_node_mac(nodename, modulename, parameter):
    erlcommand = "erl -noshell -sname " + nodename + " -s " + modulename + " start" + " " + parameter
    erlcommand = "cd /Users/hapemac/Repo/TalDerTraenen/a3 && " + erlcommand
    command = "osascript -e " + "'" + "tell application " + '"' + "Terminal" + '"'+ " to do script " + '"' + erlcommand + '"' + "'"
    os.system(command)

def __start_normal_shell(nodename):
    os.system("start erl -sname " + nodename)

# Deletes all files of the defined types in the current folder
def __remove_all_unecessary_files(unecessary_file_types):
    unecessary_files = []
    for unecessary_file_type in unecessary_file_types:
        unecessary_files = unecessary_files + glob.glob("*" + unecessary_file_type)
    for unecessary_file in unecessary_files:
        os.remove(unecessary_file)


# According to given Input, either all Testfiles gonna be executed or 
# the distributed system is going to startup
if __name__ == "__main__":
    try: 
        user_input = sys.argv[1]
    except IndexError:
        user_input = "0"
    
    if user_input == "0":
        __make_all_modules()
    elif user_input == "1":
        __make_all_modules()
        __remove_all_unecessary_files([".log"])
        test_modulenames = __get_all_test_modulenames()
        for modulename in test_modulenames:
            pointIndex = modulename.find(".")
            print("_________________________________________")
            __test_module(modulename[:pointIndex])
        __remove_all_unecessary_files([".beam", ".dump"])
    elif user_input == "2":
        __make_all_modules()
        __remove_all_unecessary_files([".log"])
        __start_node_mac("ns", "nameservice", "")
        __start_node_mac("core1", "core", "")
        __start_node_mac("core2", "core", "")
    elif user_input == "3":
        __remove_all_unecessary_files([".log", ".beam", ".dump"])
    else:
        print("Argument: '" + user_input + "' unkonwn.")