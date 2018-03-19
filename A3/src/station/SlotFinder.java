package station;

import java.util.ArrayList;
import java.util.Random;

public class SlotFinder {
	
	public static int findSlotInNextFrame(ArrayList<Message> messages, String stationName){
		int currentSlotNumber;
		Message currentMessage;
		ArrayList<Integer> usedSlotNumbers;
		ArrayList<Integer> freeSlotNumbers;
		
		usedSlotNumbers = new ArrayList<Integer>();
		freeSlotNumbers = new ArrayList<Integer>();
		
		for(int index = 0; index < messages.size(); index++) {
			currentMessage = messages.get(index);
			if(messageIsFromStation(currentMessage, stationName)){
				currentSlotNumber = currentMessage.getSlotNumber();
				return currentSlotNumber;
			} else {
				currentSlotNumber = currentMessage.getSlotNumber();
				usedSlotNumbers.add(currentSlotNumber);
			}
		}
	
		freeSlotNumbers = calculateFreeSlotNumbers(usedSlotNumbers);
		return selectRandomFreeSlotNumber(freeSlotNumbers);		
	}
	
	private static boolean messageIsFromStation(Message message, String stationName){
		return message.getStationName().contains(stationName);
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



