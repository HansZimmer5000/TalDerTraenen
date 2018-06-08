package prototypes;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.Inet4Address;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.UnknownHostException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

public class Test {

	public static void main(String[] args) throws Exception {
		String test = "Abc";
		System.out.println(test.substring(0,1));
		System.out.println(test.substring(1, test.length()));
	}
	
	public static void writeFiletest() throws IOException {
		Path currentFolder = Paths.get("");
		
		List<String> lines = Arrays.asList("The first line", "The second line");
		Path file = Paths.get("testfile.txt");
		Files.write(file, lines, Charset.forName("UTF-8"));
	}
	
	public static void stringsplittest(){
		String testString = "rebind(String a, int b)";
		System.out.println(testString.split("\\(")[1]);
	}
	
	public static void intIntegerTest(Integer value1, int value2){
		System.out.println(value1 + value2);
	}
	
	public static void testHowToGetTCPConnection() throws UnknownHostException, IOException{
		GenSocketClass server = new GenSocketClass(null, 15000, true);
		GenSocketClass client = new GenSocketClass(Inet4Address.getByName("127.0.0.1"), 15000, false);
		new Thread(server).start();
		client.connectTo();
	}
	
	public static class GenSocketClass implements Runnable{
		
		ServerSocket serverSocket;
		Socket socket;
				
		public GenSocketClass(InetAddress inetAddress, int port, boolean isServer) throws IOException {
			if(isServer){
				this.serverSocket = new ServerSocket(port);
			} else {
				this.socket = new Socket(inetAddress, port);
			}
		}
		
		public void connectTo() throws IOException{
			BufferedWriter os = new BufferedWriter(new OutputStreamWriter(this.socket.getOutputStream()));
			os.write("hihi");
			System.out.println("send hihi");
			os.flush();
			os.close();
			socket.close();
		}

		@Override
		public void run() {
			try {
				Socket connection = this.serverSocket.accept();
				BufferedReader is = new BufferedReader(new InputStreamReader(connection.getInputStream()));
				System.out.println("Got Connection from: " + connection.getPort() + " at: " + connection.getLocalPort());
				System.out.println("Got " + is.readLine());
				is.close();
				serverSocket.close();
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}
	
	
	public static void testIfObjectIsCallableAfterStartingItInAThread() throws InterruptedException{
		TestClass testinstance = new TestClass("aaaaaa");
		
		testinstance.start();
		Thread TestClassThread = testinstance.currentThread();
		
		Thread.sleep(1000);
		testinstance.setMyStr("b");
		
	}
	
	public static class TestClass extends Thread {
		String myStr;
		
		public TestClass(String str) {
			this.myStr = str;
		}
		
		public void setMyStr(String newStr) {
			this.myStr = newStr;
		}
		
		@Override
		public void run() {
			System.out.println(this.myStr);
			try {
				sleep(10);
				run();
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
	}

}
