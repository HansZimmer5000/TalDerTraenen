package station;

import static org.junit.Assert.*;

import java.util.ArrayList;

import org.junit.Test;

public class SlotFinderTest {

	@Test
	public void test_findSlotInNextFrame_1() {
		ArrayList<Message> messages;
		String teamName;
		int isResult;
		int shouldResult;
		
		teamName = "team-6000";
		messages = new ArrayList<Message>();
		try{
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 1));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 2));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 3));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 4));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 5));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 6));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 7));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 8));
			
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 10));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 11));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 12));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 13));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 14));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 15));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 16));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 17));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 18));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 19));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 20));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 21));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 22));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 23));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 24));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 25));
		} catch(Exception e){
			fail("couldn't create all messages");
		}
		shouldResult = 9;
		isResult = SlotFinder.findSlotInNextFrame(messages, teamName);
		
		assertEquals(shouldResult, isResult);
	}

	@Test
	public void test_findSlotInNextFrame_2() {
		ArrayList<Message> messages;
		String teamName;
		int isResult;
		int shouldResult;
		
		teamName = "team-6000";
		messages = new ArrayList<Message>();
		try{
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 1));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 2));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 3));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 4));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 5));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 6));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 7));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 8));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 9));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 10));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 11));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 12));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 13));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 14));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 15));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 16));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 17));
			messages.add(Message.createIncompleteMessage("A", "-team-6000123456789012-", 18));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 19));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 20));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 21));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 22));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 23));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 24));
			messages.add(Message.createIncompleteMessage("A", "-team-4711123456789012-", 25));
		} catch(Exception e){
			fail("couldn't create all messages");
		}
		shouldResult = 18;
		isResult = SlotFinder.findSlotInNextFrame(messages, teamName);
		
		assertEquals(shouldResult, isResult);
	}
	
}
