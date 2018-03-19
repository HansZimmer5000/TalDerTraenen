package station;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.InetAddress;
import java.net.MulticastSocket;
import java.net.UnknownHostException;

public class Sender {

	private static final String IP = "225.10.1.2";
	private static final int PORT = 6789;//Should be but is not working: 15001
	private static InetAddress group = null;
	private static MulticastSocket socket = null;
	
	public static void send(Message message, long sendTime){
		byte[] messageAsByte;
		
		messageAsByte = message.prepareForSending(sendTime);
		connect();
		send(messageAsByte);
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
	
	private static void send(byte[] messageAsByte){	
		DatagramPacket packet;
		
		packet = new DatagramPacket(messageAsByte, messageAsByte.length, group, 6789);
		try {
			socket.send(packet);
		} catch (IOException e) {System.out.println("Couldn't send! (Sender)");}
    }
}
