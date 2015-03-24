package ist.meic.pa.command;

import ist.meic.pa.MethodCallEntry;

import java.util.Stack;

public class ReturnCommand extends Command {
    @Override
    public Object execute(Stack<MethodCallEntry> stack, Object calledObject, String[] args, Throwable t) throws Throwable {
        MethodCallEntry calledMethod = stack.pop();
        Class returnType = calledMethod.getResultSig();
        //TODO: parse input, if its is possible to args contain more then one value
        return getParameterParser().get(returnType.getSimpleName()).parse(args[0]);
    }
}

