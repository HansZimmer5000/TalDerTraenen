package test;

import static org.junit.Assert.*;

import java.util.Arrays;

import org.junit.Test;

import station.MessageGenerator;

public class MessageGeneratorTests {

	@Test
	public void test_convertMessageFromByte() {
		String isResult;
		String shouldResult;
		byte[] messageInByte;
		
		shouldResult = "A-team-4711-4";
		messageInByte = new byte[] {65, 45, 116, 101, 97, 109, 45, 52, 55, 49, 49, 45, 52};
		isResult = MessageGenerator.convertMessageFromByte(messageInByte);
		assertEquals(shouldResult, isResult);
	}
	
	@Test
	public void test_convertMessageToByte() {
		byte[] isResult;
		byte[] shouldResult;
		String message;
		
		message = "A-team-4711-4";
		shouldResult = new byte[] {65, 45, 116, 101, 97, 109, 45, 52, 55, 49, 49, 45, 52};
		isResult = MessageGenerator.convertMessageToByte(message);
		assertArrayEquals(shouldResult, isResult);
	}
	
	@Test
	public void test_createMessage() {
		String isResult;
		String shouldResult;
		
		shouldResult = "A-team-4711-4";
		isResult = MessageGenerator.createMessage("A", "-team-4711-", 4);
		assertEquals(shouldResult, isResult);
	}
}
