package station;

import java.nio.charset.StandardCharsets;

public class MessageGenerator {
	
	/*Nachrichtenaufbau:
	    Gesamt 34 Byte
	    - Byte 0        A oder B    Stationsklasse
	    - Byte 1-24     -team-4711- Nutzdaten
	    - Byte 25       4           reservierte Slotnummer f�r den n�chsten Frame!
	    - Byte 26-33    77394825    Zeit (gesendet) in ms seit 01.01.1970, 8-Byte Integer, Big Endian
	*/
	public static String convertMessageFromByte(byte[] messageInByte){
		return new String(messageInByte, StandardCharsets.UTF_8);
	}
	
	public static byte[] convertMessageToByte(String message){
		//TODO Nachricht hat 21-22 Letters, das sollen dann wie 34 Byte werden?
		return message.getBytes();
		/*
		byte[] stationTypeAndTeamName, slotNumber, sendTime;
		int messageLength;
		
		messageLength = message.length();		
		stationTypeAndTeamName = message.substring(0, 24).getBytes();
		if(messageLength == 34){
			slotNumber = message.substring(25).getBytes();
		} else {
			slotNumber = message.substring(25,26).getBytes();
		}
		sendTime = message.substring(messageLength - 8).getBytes();
		
		return unifyArrays(new byte[][] {stationTypeAndTeamName, slotNumber, sendTime});
		*/
	}
	
	private static byte[] unifyArrays(byte[][] byteArrays){
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
	
	public static String createMessage(String stationType, String teamname, int slotNumber){
		return stationType + teamname + String.valueOf(slotNumber);
	}
}
