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
				currentSlotNumber = MessageHelper.getSlotNumber(currentMessage);
				return currentSlotNumber;
			} else {
				currentSlotNumber = MessageHelper.getSlotNumber(currentMessage);
				usedSlotNumbers.add(currentSlotNumber);
			}
		}
	
		freeSlotNumbers = calculateFreeSlotNumbers(usedSlotNumbers);
		return selectRandomFreeSlotNumber(freeSlotNumbers);		
	}
	
	private static boolean messageIsFromStation(String message, String stationName){
		return message.contains(stationName);
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



