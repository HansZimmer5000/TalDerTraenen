package idl_compiler;

public class StartMain {
	public static void main(String[] args) {
		if(args.length < 1) {
		String[] args2 = new String[1];
		args2[0] = "../class.idl";
		}
		Parser.main(args);
	}
}
