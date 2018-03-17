package station;

import java.net.DatagramPacket;
import java.net.InetAddress;
import java.net.MulticastSocket;
import java.nio.charset.StandardCharsets;

public class Ausprobieren {

	public static void main(String[] args) {
		// join a Multicast group and send the group salutations
		try {
			String msg = "Hello";
			String ip = "225.10.1.2";
			int port = 6789; //Should be but is not working: 15001
			
			
			
			InetAddress group = InetAddress.getByName(ip);
			MulticastSocket s = new MulticastSocket(port);
			s.joinGroup(group);
			
			
			
			
			
			DatagramPacket hi = new DatagramPacket(msg.getBytes(), msg.length(), group, 6789);
			s.send(hi);
			
			
			
			// get their responses!
			byte[] buf = new byte[1000];
			DatagramPacket recv = new DatagramPacket(buf, buf.length);
			s.receive(recv);
			System.out.println(new String(recv.getData(), StandardCharsets.UTF_8));
			
			
			
			
			
			// OK, I'm done talking - leave the group...
			s.leaveGroup(group);
			s.close();
			}
		
		
		catch(Exception e){
			e.printStackTrace();
		}
			
	}

}