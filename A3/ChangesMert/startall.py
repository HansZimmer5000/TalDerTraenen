
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
    erlcommand = "cd /Users/hapemac/Repo/TalDerTraenen/a3/changesmert && " + erlcommand
    command = "osascript -e " + "'" + "tell application " + '"' + "Terminal" + '"'+ " to do script " + '"' + erlcommand + '"' + "'"
    os.system(command)

def __start_stations(count):
    while(count > 0):
        count_str = str(count)
        stationname = "team-06-"
        if(count < 10):
            stationname = stationname + "0"
        stationname = stationname + count_str
        __start_node_mac("station" + count_str, "station", "A " + stationname)
        count = count - 1

def __start_normal_shell(nodename):
    os.system("start erl -sname " + nodename)

# Deletes all files of the defined types in the current folder
def __remove_all_unecessary_files(unecessary_file_types):
    unecessary_files = []
    for unecessary_file_type in unecessary_file_types:
        unecessary_files = unecessary_files + glob.glob("*" + unecessary_file_type)
    for unecessary_file in unecessary_files:
        os.remove(unecessary_file)

# Clears a certain file (filename) of any content.
def __clear_file(filename):
    open(filename, "w").close()

def __clear_all_log_files_in_current_dir():
    log_filenames = glob.glob("*.log")
    for log_filename in log_filenames:
        __clear_file(log_filename)

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
        __clear_all_log_files_in_current_dir()
        __start_node_mac("ns", "nameservice", "")
        #__start_node("bench", "benchmark", "")
        __start_stations(5)
    elif user_input == "3":
        __remove_all_unecessary_files([".log", ".beam", ".dump"])
    else:
        print("Argument: '" + user_input + "' unkonwn.")