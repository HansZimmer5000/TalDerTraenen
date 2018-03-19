package station;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.InetAddress;
import java.net.MulticastSocket;
import java.net.UnknownHostException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;

public class Receiver {

	private static final String IP = "225.10.1.2";
	private static final int PORT = 6789;//Should be but is not working: 15001
	private static InetAddress group = null;
	private static MulticastSocket socket = null;
	
	public static ArrayList<byte[]> listenToFrame(){
		ArrayList<byte[]> messages;
		byte[] currentMessage;
		
		messages = new ArrayList<byte[]>();
		
		connect();
		
		currentMessage = receiveMessage(socket);
		messages.add(currentMessage);
		// TODO 1 Sekunde (Slot) zu hören & Nachrichten abfangen. Und wie zurückgeben?
		// TODO: irgendwie mit aktueller Zeit und dann while(AktuelleZeit <= StartZeit+1000ms)?
		
		return messages;
	}
	
	private static void connect(){
		if(socket == null || group == null){					
			try {
				group = InetAddress.getByName(IP);
			} catch (UnknownHostException e) {System.out.println("Couldn't get group from IP! (Sender)");}
			try {
				socket = new MulticastSocket(PORT);
			} catch (IOException e) {System.out.println("Couldn't get Socket from PORT! (Sender)");}
			try {
				socket.joinGroup(group);
			} catch (IOException e) {System.out.println("Couldn't join group! (Sender)");}
		}
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
