package station;


public class UTCClock {

	private long offset;
	
	public UTCClock(long newOffset){
		this.offset = newOffset;
	}
	
	public void adjust(Message message){	
		String stationType;
		long adjustment;
		
		stationType = message.getStationType();
		if(stationType == "A"){
			adjustment = getAdjustment(message);
			calculateNewOffset(adjustment);
		}
	}
	
	private long getAdjustment(Message message){
		long receivedTime;
		long sendTime;
		
		try {
			receivedTime = message.getReceivedTime();
		} catch (Exception e) {
			System.err.println("No ReceivedTime! (UTCClock)");
			receivedTime = 0;
		}
		
		try {
			sendTime = message.getSendTime();
		} catch (Exception e) {
			System.err.println("No SendTime! (UTCClock)");
			sendTime = 0;
		}
		
		return receivedTime - sendTime;
	}
	
	private void calculateNewOffset(long adjustment){
		this.offset = this.offset + adjustment;
	}
}
