package ist.meic.pa.command;

import ist.meic.pa.Debugger;
import ist.meic.pa.MethodCallEntry;

import java.util.Stack;

public class ReturnCommand implements Command {
    @Override
    public Object execute(Stack<MethodCallEntry> stack, Object calledObject, String[] args) throws Throwable {
        MethodCallEntry calledMethod = stack.pop();
        Class returnType = calledMethod.getResultSig();
        //TODO: parse input, if its is possible to args contain more then one value
        return Debugger.getParameterParser(returnType.getSimpleName()).parse(args[0]);
    }
}

