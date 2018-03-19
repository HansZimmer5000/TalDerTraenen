

import java.net.DatagramPacket;
import java.net.InetAddress;
import java.net.MulticastSocket;
import java.nio.charset.StandardCharsets;


public class Ausprobieren {

	public static void main(String[] args){
		String testString1 = "A-team-4711-477394825";
		String testString2 = "A-team-4711-1277394825";
		
		HelloRunnable1.main();
		HelloRunnable2.main();
	}
	
	public static void oldmain(String[] args) {
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
	
	public static class HelloRunnable1 implements Runnable {

	    public void run() {
	    	InetAddress group = null;
	    	MulticastSocket socket = null;
			String msg = "Hello";
			String ip = "225.10.1.2";
			int port = 6789; //Should be but is not working: 15001
	    	try {
	    		if(socket == null){					
					group = InetAddress.getByName(ip);
					socket = new MulticastSocket(port);
					socket.joinGroup(group);
	    		}
				
				DatagramPacket hi = new DatagramPacket(msg.getBytes(), msg.length(), group, 6789);
				socket.send(hi);
	    	} catch(Exception e){
	    		e.printStackTrace();
	    	}
	    	try {
				Thread.sleep(20);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
	    	run();
	    }

	    public static void main() {
	        (new Thread(new HelloRunnable1())).start();
	    }

	}
	
	public static class HelloRunnable2 implements Runnable {

	    public void run() {
	    	InetAddress group;
	    	MulticastSocket socket = null;
			String msg = "Hello";
			String ip = "225.10.1.2";
			int port = 6789; //Should be but is not working: 15001
	    	try {
	    		if(socket == null){					
					group = InetAddress.getByName(ip);
					socket = new MulticastSocket(port);
					socket.joinGroup(group);
	    		}
				
				// get their responses!
				byte[] buf = new byte[1000];
				DatagramPacket recv = new DatagramPacket(buf, buf.length);
				socket.receive(recv);
				System.out.println(new String(recv.getData(), StandardCharsets.UTF_8));
	    	} catch(Exception e){
	    		e.printStackTrace();
	    	}
	    	run();
	    }

	    public static void main() {
	        (new Thread(new HelloRunnable2())).start();
	    }

	}

}