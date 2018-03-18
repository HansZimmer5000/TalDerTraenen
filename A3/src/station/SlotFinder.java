package station;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;

public class SlotFinder {
	
	public static int findSlotInNextFrame(ArrayList<String> messages){
		
		for(String currentString: messages) {
			
		}
		
		
	}
	
	private static int getSlotNumberFromMessage(String message) {
		//Beispiel (getrennt mit ,): A,-team-4711-,4,77394825
		
		//return Integer.valueOf(new String(message.getBytes()[25], StandardCharsets.UTF_8));
		return 0;
	}
}
