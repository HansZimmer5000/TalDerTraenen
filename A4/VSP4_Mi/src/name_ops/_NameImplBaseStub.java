package name_ops;

import mware_lib.ObjectReference;

public class _NameImplBaseStub extends _NameImplBase {

	private ObjectReference objectReference;

	public _NameImplBaseStub(Object con) {
		this.objectReference = (ObjectReference) con;
	}

	public void rebind(String servantSocket, String name) {
		objectReference.sendMethodAndParametersToServiceAndWaitForAnswer("rebind", "String " + servantSocket + ",String " + name);
	}

	public Object resolve(String name) {
		String socketString = objectReference.sendMethodAndParametersToServiceAndWaitForAnswer("resolve", "String " + name);
		return socketString;
	}

}
