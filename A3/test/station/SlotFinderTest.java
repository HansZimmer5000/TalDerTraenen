package station;

import static org.junit.Assert.*;

import java.util.ArrayList;

import org.junit.Test;

public class SlotFinderTest {

	@Test
	public void test_findSlotInNextFrame_1() {
		ArrayList<String> messages;
		String teamName;
		int isResult;
		int shouldResult;
		
		teamName = "team-6000";
		messages = new ArrayList<String>();
		messages.add("A-team-4711-177394825");
		messages.add("B-team-4711-277394825");
		messages.add("A-team-4711-377394825");
		messages.add("B-team-4711-477394825");
		messages.add("A-team-4711-577394825");
		messages.add("A-team-4711-677394825");
		messages.add("A-team-4711-777394825");
		messages.add("A-team-4711-877394825");
		
		messages.add("A-team-4711-1077394825");
		messages.add("A-team-4711-1177394825");
		messages.add("A-team-4711-1277394825");
		messages.add("A-team-4711-1377394825");
		messages.add("A-team-4711-1477394825");
		messages.add("A-team-4711-1577394825");
		messages.add("A-team-4711-1677394825");
		messages.add("A-team-4711-1777394825");
		messages.add("A-team-4711-1877394825");
		messages.add("A-team-4711-1977394825");
		messages.add("A-team-4711-2077394825");
		messages.add("A-team-4711-2177394825");
		messages.add("A-team-4711-2277394825");
		messages.add("A-team-4711-2377394825");
		messages.add("A-team-4711-2477394825");
		messages.add("A-team-4711-2577394825");
		shouldResult = 9;
		isResult = SlotFinder.findSlotInNextFrame(messages, teamName);
		
		assertEquals(shouldResult, isResult);
	}

	@Test
	public void test_findSlotInNextFrame_2() {
		ArrayList<String> messages;
		String teamName;
		int isResult;
		int shouldResult;
		
		teamName = "team-6000";
		messages = new ArrayList<String>();
		messages.add("A-team-4711-177394825");
		messages.add("B-team-4711-277394825");
		messages.add("A-team-4711-377394825");
		messages.add("B-team-4711-477394825");
		messages.add("A-team-4711-777394825");
		messages.add("A-team-4711-877394825");
		messages.add("A-team-4711-977394825");
		messages.add("A-team-4711-1077394825");
		messages.add("A-team-4711-1177394825");
		messages.add("A-team-4711-1277394825");
		messages.add("A-team-4711-1377394825");
		messages.add("A-team-4711-1477394825");
		messages.add("A-team-4711-1577394825");
		messages.add("A-team-4711-1677394825");
		messages.add("A-team-4711-1777394825");
		messages.add("A-team-6000-1877394825");
		messages.add("A-team-4711-1977394825");
		messages.add("A-team-4711-2077394825");
		messages.add("A-team-4711-2177394825");
		messages.add("A-team-4711-2277394825");
		messages.add("A-team-4711-2377394825");
		messages.add("A-team-4711-2477394825");
		messages.add("A-team-4711-2577394825");
		shouldResult = 18;
		isResult = SlotFinder.findSlotInNextFrame(messages, teamName);
		
		assertEquals(shouldResult, isResult);
	}
	
}
