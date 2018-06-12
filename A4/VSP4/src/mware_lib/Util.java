package mware_lib;

/**
 * Util class.
 * 
 * @author Mert Siginc
 * In dieser Klasse befinden sich Hilfsmethoden
 *
 */

public class Util {
	private static final int DEFAULT_PORT = 55555;
	
	public static void println(String string, boolean debug){
		 if (debug){
			System.out.println(string); 
		 }
	 }
	 
	 public static int checkPort(String[] args){
		 if (args.length == 3) {
				return Integer.parseInt(args[1]);
			} else {
				return DEFAULT_PORT;
			}
	 }
}
