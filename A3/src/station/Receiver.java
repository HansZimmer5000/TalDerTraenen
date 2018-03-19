package station;

import java.net.DatagramPacket;
import java.net.InetAddress;
import java.net.MulticastSocket;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;

public class Receiver {

	public static ArrayList<byte[]> listenToFrame(){
		ArrayList<byte[]> messages;
		String ip;
		int port; 
		InetAddress group;
		MulticastSocket socket;
		
		messages = new ArrayList<byte[]>();
		ip = "225.10.1.2";
		port = 6789; //Should be 15001 but is not working.
		
		try {
			group = InetAddress.getByName(ip);
			socket = new MulticastSocket(port);
			socket.joinGroup(group);
			
			// 1 Sekunde (Slot) zu hören & Nachrichten abfangen. Und wie zurückgeben?
		} catch(Exception e){
			e.printStackTrace();
		}
		
		return messages;
	}
	
	public static byte[] receiveMessage(MulticastSocket socket){
		byte[] message;
		
		try {			
			byte[] buf = new byte[1000];
			DatagramPacket recv = new DatagramPacket(buf, buf.length);
			socket.receive(recv);
			
			message = recv.getData();
			System.out.println("Received: " + new String(message, StandardCharsets.UTF_8));
		} catch(Exception e){
			message = new byte[0];
			e.printStackTrace();
		}
		return message;
	}
}
