package starter;

public class StarterStarter {

	public static void main(String[] args) {
		try {
			new Thread(new NameserviceServerMainThread()).start();
			Thread.sleep(1000);
			new Thread(new CalculatorServerMainThread()).start();
			Thread.sleep(1000);
			new Thread(new CalculatorClientMainThread()).start();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private static class CalculatorServerMainThread implements Runnable {
		@Override
		public void run() {
			try {
				CalculatorServer.main(null);
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}
	
	private static class NameserviceServerMainThread implements Runnable {
		@Override
		public void run() {
			try {
				NameserviceServer.main(null);
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}
	
	private static class CalculatorClientMainThread implements Runnable {
		@Override
		public void run() {
			try {
				CalculatorClient.main(null);
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}

}
