package station;

import java.nio.charset.StandardCharsets;

public class MessageGenerator {
	
	/*Nachrichtenaufbau:
	    Gesamt 34 Byte
	    - Byte 0        A oder B    Stationsklasse
	    - Byte 1-24     -team-4711- Nutzdaten
	    - Byte 25       4           reservierte Slotnummer für den nächsten Frame!
	    - Byte 26-33    77394825    Zeit (gesendet) in ms seit 01.01.1970, 8-Byte Integer, Big Endian
	*/
	public static String convertMessageFromByte(byte[] messageInByte){
		return new String(messageInByte, StandardCharsets.UTF_8);
	}
	
	public static byte[] convertMessageToByte(String message){
		return message.getBytes();
	}
	
	public static String createMessage(String stationType, String teamname, int slotNumber){
		return stationType + teamname + String.valueOf(slotNumber);
	}
}
