package station;

import java.net.DatagramPacket;
import java.net.InetAddress;
import java.net.MulticastSocket;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;

public class Receiver {

	static ArrayList<String> listenToFrame(){
		ArrayList<String> messages;
		String ip;
		int port; 
		InetAddress group;
		MulticastSocket socket;
		
		messages = new ArrayList<String>();
		ip = "225.10.1.2";
		port = 6789; //Should be 15001 but is not working.
	
		try {
			group = InetAddress.getByName(ip);
			socket = new MulticastSocket(port);
			socket.joinGroup(group);
			
			byte[] buf = new byte[1000];
			DatagramPacket recv = new DatagramPacket(buf, buf.length);
			socket.receive(recv);
			System.out.println("Received: " + new String(recv.getData(), StandardCharsets.UTF_8));
		} catch(Exception e){
			e.printStackTrace();
		}
		return messages;
	}
}
