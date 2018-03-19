package station;

import java.util.ArrayList;

public class UTCClock {

	private long offset;
	
	public UTCClock(long newOffset){
		this.offset = newOffset;
	}
	
	public void adjust(ArrayList<Message> messages){
		String currentStationType;
		long currentAdjustment;
		
		for (Message currentMessage : messages){
			currentStationType = currentMessage.getStationType();
			if(currentStationType == "A"){
				//currentAdjustment = getAdjustment()
				//TODO: Würden hier Zeit des Empfangs brauchen, dann wäre klar wie hoch der unterschied ist.
				//Vom Empfang bis hier kann nämlich eine gewisse Zeit vergehen die das Ergebnsi verfälscht.
			}
		}
	}
}
