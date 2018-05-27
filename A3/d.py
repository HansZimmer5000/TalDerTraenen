
import glob

def __get_all_log_files():
    return glob.glob("team*.log")

def __handle_log_file(files):
    for currentfile in files:
        print("\n" + currentfile, end=" ")
        currentfile = open(currentfile, "r")
        file_dict = {name: currentfile}
        for currentline in currentfile.readlines():
            output = __handle_line(currentline)
            {name: currentfile, ou}
        

def __handle_line(currentline):
    output = ""
    if "with Slot" in currentline:
        output = currentline[72:74]
        #print("Current Slot: " + currentline[72:74], end=" ")
    if "Got NextSlotnumber" in currentline:
        output = currentline[48:]
        #print("Next Slot: " + currentline[48:])
    return output

if __name__ == "__main__":
    files = __get_all_log_files()
    __handle_log_file(files)