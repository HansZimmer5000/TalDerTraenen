package station;

import static org.junit.Assert.*;

import java.util.Arrays;

import org.junit.Test;

import station.MessageHelper;

public class MessageGeneratorTests {

	@Test
	public void test_convertMessageFromByte_1() {
		String isResult;
		String shouldResult;
		byte[] messageInByte;
		
		shouldResult = "A-team-4711-477394825";
		messageInByte = new byte[] {65, 
									45, 116, 101, 97, 109, 45, 52, 55, 49, 49, 45, 
									4,
									55, 55, 51, 57, 52, 56, 50, 53};
		isResult = MessageHelper.convertMessageFromByte(messageInByte);
		assertEquals(shouldResult, isResult);
	}
	
	@Test
	public void test_convertMessageFromByte_2() {
		String isResult;
		String shouldResult;
		byte[] messageInByte;
		
		shouldResult = "A-team-4711-2577394825";
		messageInByte = new byte[] {65, 
									45, 116, 101, 97, 109, 45, 52, 55, 49, 49, 45, 
									25,
									55, 55, 51, 57, 52, 56, 50, 53};
		isResult = MessageHelper.convertMessageFromByte(messageInByte);
		assertEquals(shouldResult, isResult);
	}
	
	@Test
	public void test_convertMessageToByte_1() {
		byte[] isResult;
		byte[] shouldResult;
		String message;
		
		message = "A-team-4711-477394825";
		shouldResult = new byte[] {65, 
									45, 116, 101, 97, 109, 45, 52, 55, 49, 49, 45, 
									52,
									55, 55, 51, 57, 52, 56, 50, 53};
		isResult = MessageHelper.convertMessageToByte(message);
		assertArrayEquals(shouldResult, isResult);
	}
	
	@Test
	public void test_createMessage_1() {
		String isResult;
		String shouldResult;
		
		shouldResult = "A-team-4711-4";
		isResult = MessageHelper.createUncompleteMessage("A", "-team-4711-", 4);
		assertEquals(shouldResult, isResult);
	}
}
