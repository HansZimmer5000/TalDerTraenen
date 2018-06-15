package math_opsCompiler;

import mware_lib.ComHandler;

import java.util.ArrayList;
import java.util.Arrays;

public class _CalculatorImplBaseStub implements _CalculatorImplBase{

private ComHandler rawObject;
public _CalculatorImplBaseStub(Object rawObjectRef) {
String nsAnswerSplited[] = ((String) rawObjectRef).split("\\|");
rawObject = new ComHandler(nsAnswerSplited[0], new Integer(nsAnswerSplited[1]), false);
}
public double add(double a, double b) {
ArrayList<Object> params = new ArrayList<Object>(Arrays.asList(a,b));
String answerString = params.toString();
answerString = answerString.substring(1, answerString.length()-1);
return new Double((String) rawObject.sendToService("add", answerString));
}
}
