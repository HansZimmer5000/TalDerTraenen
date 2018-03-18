package station;

import static org.junit.Assert.*;

import java.util.ArrayList;

import org.junit.Test;

public class SlotFinderTest {

	@Test
	public void test_findSlotInNextFrame_1() {
		ArrayList<String> messages;
		int isResult;
		int shouldResult;
		System.out.println("messages sind noch ohne TS!");
		assert(false);
		
		messages = new ArrayList<String>();
		messages.add("A-team-4711-1");
		messages.add("B-team-4711-2");
		messages.add("A-team-4711-3");
		messages.add("B-team-4711-4");
		messages.add("A-team-4711-5");
		messages.add("A-team-4711-6");
		messages.add("A-team-4711-7");
		messages.add("A-team-4711-8");
		
		messages.add("A-team-4711-10");
		messages.add("A-team-4711-11");
		messages.add("A-team-4711-12");
		messages.add("A-team-4711-13");
		messages.add("A-team-4711-14");
		messages.add("A-team-4711-15");
		messages.add("A-team-4711-16");
		messages.add("A-team-4711-17");
		messages.add("A-team-4711-18");
		messages.add("A-team-4711-19");
		messages.add("A-team-4711-20");
		messages.add("A-team-4711-21");
		messages.add("A-team-4711-22");
		messages.add("A-team-4711-23");
		messages.add("A-team-4711-24");
		messages.add("A-team-4711-25");
		shouldResult = 9;
		isResult = SlotFinder.findSlotInNextFrame(messages);
		
		assertEquals(shouldResult, isResult);
	}

}
