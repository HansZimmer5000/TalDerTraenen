from __future__ import print_function
import glob


def __get_all_log_files():
    return glob.glob("team*.log")

def __handle_log_file(files):
    file_dict = {}
    for currentfile in files:
        #print("\n" + currentfile)
        currentfile = open(currentfile, "r")
	frame_pairs = []
        for currentline in currentfile.readlines():
            startSlot = get_with_slot(currentline)
	    sendSlot = get_send_slot(currentline)
	    frame_pair = startSlot + sendSlot
	    if(frame_pair != []):
	    	frame_pairs = frame_pairs + frame_pair
	if(frame_pairs != []):
		file_dict.update({currentfile.name:frame_pairs})
    print_dict(file_dict)
        

def get_with_slot(currentline):
    output = []
    if "with Slot" in currentline:
        output = [currentline[72:74]]
    return output
	
def get_send_slot(currentline):
    output = []
    if "Got NextSlotnumber" in currentline:
        output = currentline[48:50]
	output = [output.replace(" ", "")]
        #print("Next Slot: " + currentline[48:])
    return output
    
def print_dict(file_dict):
    for key in file_dict.keys():
	print(key, end=":")
	currentslotlist = file_dict[key]
	print(currentslotlist)

if __name__ == "__main__":
    files = __get_all_log_files()
    __handle_log_file(files)