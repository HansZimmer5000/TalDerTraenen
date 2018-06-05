package prototypes;

public class Test {

	public static void main(String[] args) throws InterruptedException {
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
