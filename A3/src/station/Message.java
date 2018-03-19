package station;

import java.nio.charset.StandardCharsets;

public class Message {
	
	/*
	Nachrichtenaufbau (String message):
    Gesamt 34 Byte
    - Byte 0        A oder B    Stationsklasse
    - Byte 1-24     -team-4711- Nutzdaten
    - Byte 25       4           reservierte Slotnummer für den nächsten Frame!
    - Byte 26-33    77394825    Zeit (gesendet) in ms seit 01.01.1970, 8-Byte Integer, Big Endian
	 */
	
	private final String stationType;
	private final String payload;
	private final int slotNumber;
	private long sendTime;
	private long receivedTime;
	
	//\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	// CONSTRUCTOR
	////////////////////////////////////////////
	public static Message createIncompleteMessage(String stationType, String payload, int slotNumber){
		return new Message(stationType, payload, slotNumber, 0, 0);
	}
	
	public static Message createMessageFromByte(byte[] messageInByte, long newReceivedTime){
		String receivedMessageAsString = new String(messageInByte, StandardCharsets.UTF_8);
		String newStationType;
		String newPayLoad;
		int newSlotNumber;
		long newSendTime;
		
		newStationType = receivedMessageAsString.substring(0, 1);
		newPayLoad = receivedMessageAsString.substring(1, 25);
		newSlotNumber = (int) messageInByte[25];
		newSendTime = Long.valueOf(receivedMessageAsString.substring(receivedMessageAsString.length() - 8));
		
		return new Message(newStationType, newPayLoad, newSlotNumber, newSendTime, newReceivedTime);
	}
	
	private Message(String newStationType, String newPayLoad, int newSlotNumber, long newSendTime, long newReceivedTime){
		this.stationType = newStationType;
		this.payload = newPayLoad;
		this.slotNumber = newSlotNumber;
		this.sendTime = newSendTime;
		this.receivedTime = newReceivedTime;
	}
	
	//\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	// GETTER SETTER
	////////////////////////////////////////////
	public String getStationType(){
		return this.stationType;
	}
	
	public String getStationName(){
		return this.payload.substring(0, 11);
	}
	
	public int getSlotNumber() {
		return this.slotNumber;
	}
	
	private void setSendtime(long sendTime){
		this.sendTime = sendTime;
	}
	
	//\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	// WORK
	////////////////////////////////////////////

	public byte[] prepareForSending(long sendTime){
		this.setSendtime(sendTime);
		
		return this.toByteArray();
	}
	
	//\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	// CONVERSIONS
	////////////////////////////////////////////
	public String toString(){
		String result = this.stationType + this.payload + String.valueOf(this.slotNumber);
		
		if(sendTime > 0){result += String.valueOf(sendTime);}
		
		return result;
	}
	
	private byte[] toByteArray(){
		byte[] stationTypeAndPayLoadAsByteArray, slotNumberAsByteArray, sendTimeAsByteArray;
		
		stationTypeAndPayLoadAsByteArray = (this.stationType + this.payload).getBytes();
		slotNumberAsByteArray = new byte[]{(byte) this.slotNumber};
		sendTimeAsByteArray = String.valueOf(this.sendTime).getBytes();
		
		return unifyArrays(new byte[][] {stationTypeAndPayLoadAsByteArray, slotNumberAsByteArray, sendTimeAsByteArray});
	}
	
	private byte[] unifyArrays(byte[][] byteArrays){
		int totalLength, currentFilledCount;
		byte[] result;
		
		totalLength = 0;
		for(byte[] tmpArray : byteArrays){
			totalLength += tmpArray.length;
		}
		
		result = new byte[totalLength];
		currentFilledCount = 0;
		for(byte[] tmpArray : byteArrays){
			for(int innerIndex = 0; innerIndex < tmpArray.length; innerIndex++){
				result[currentFilledCount] = tmpArray[innerIndex];
				currentFilledCount++;
			}
		}
		
		return result;
	}

}
