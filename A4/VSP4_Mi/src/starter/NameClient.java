package starter;

import mware_lib.ObjectBroker;
import mware_lib.ObjectReference;
import name_ops._NameImplBase;

public class NameClient {

	public static _NameImplBase createNameClient(ObjectBroker objectBroker) {
		ObjectReference nameServiceObjectReference = ObjectReference.init(objectBroker, "", _NameImplBase.NAMESERVICEPORT);
		_NameImplBase nameClient = _NameImplBase.narrowCast(nameServiceObjectReference);

		return nameClient;
	}
}
