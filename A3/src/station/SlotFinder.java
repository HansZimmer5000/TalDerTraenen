package station;

import java.util.ArrayList;
import java.util.Random;

public class SlotFinder {
	
	public static int findSlotInNextFrame(ArrayList<String> messages, String stationName){
		int currentSlotNumber;
		String currentMessage;
		ArrayList<Integer> usedSlotNumbers;
		ArrayList<Integer> freeSlotNumbers;
		
		usedSlotNumbers = new ArrayList<Integer>();
		freeSlotNumbers = new ArrayList<Integer>();
		
		for(int index = 0; index < messages.size(); index++) {
			currentMessage = messages.get(index);
			if(messageIsFromStation(currentMessage, stationName)){
				currentSlotNumber = getSlotNumberFromMessage(currentMessage);
				return currentSlotNumber;
			} else {
				currentSlotNumber = getSlotNumberFromMessage(currentMessage);
				usedSlotNumbers.add(currentSlotNumber);
			}
		}
	
		freeSlotNumbers = calculateFreeSlotNumbers(usedSlotNumbers);
		return selectRandomFreeSlotNumber(freeSlotNumbers);		
	}
	
	private static boolean messageIsFromStation(String message, String stationName){
		return message.contains(stationName);
	}
	
	private static int getSlotNumberFromMessage(String message) {
		//Beispiel: "A-team-4711-477394825"
		final int sendTimeLength = 8;
		final int maxSlotNumberLength = 2;
		int totalLength;
		String slotNumberAsString;
		
		totalLength = message.length();
		slotNumberAsString = message.substring(totalLength - (sendTimeLength + maxSlotNumberLength));
		
		if(slotNumberAsString.indexOf("-") >= 0){
			slotNumberAsString = slotNumberAsString.substring(1, 2);
		} else {
			slotNumberAsString = slotNumberAsString.substring(0, 2);
		}
		
		return Integer.valueOf(slotNumberAsString);
	}
	
	private static ArrayList<Integer> calculateFreeSlotNumbers(ArrayList<Integer> usedSlotNumbers){
		ArrayList<Integer> freeSlotNumbers;
		
		freeSlotNumbers = new ArrayList<Integer>();
		
		for(Integer slotNumber = 1; slotNumber <= 25; slotNumber++){
			if(!(usedSlotNumbers.contains(slotNumber))){
				freeSlotNumbers.add(slotNumber);
			}
		}
		
		return freeSlotNumbers;
	}
	
	private static int selectRandomFreeSlotNumber(ArrayList<Integer> freeSlotNumbers){
		int randomIndex;
		Random rand;
		
		rand = new Random();
		randomIndex= rand.nextInt(freeSlotNumbers.size());
		
		return freeSlotNumbers.get(randomIndex);
	}
}



