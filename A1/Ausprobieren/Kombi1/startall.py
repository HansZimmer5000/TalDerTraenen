
import os
import sys
import glob
import time

# Opens a new Commandline window (cmd.exe) and starts a new erlang node with the given name and modulecode. 
# The module must contain a "start" function without arguments.
def __start_node(nodename, modulename):
    os.system("start erl -noshell -sname " + nodename + " -s " + modulename + " start")

def __start_empty_shell_node(nodename):
    os.system("start erl -sname " + nodename)

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
        user_input = "2"
    
    if user_input == "2":
        __remove_all_unecessary_files([".dump", ".log"])
        __start_empty_shell_node("hbq")
        time.sleep(1)
        __start_node("server", "server_starter")
        time.sleep(1)
        __start_node("client", "client")
    elif user_input == "3":
        __remove_all_unecessary_files([".beam", ".dump", ".log"])
    else:
        print("Argument: '" + user_input + "' unkonwn.")