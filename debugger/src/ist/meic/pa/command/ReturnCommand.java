package ist.meic.pa.command;

import ist.meic.pa.MethodCallEntry;

import java.util.Stack;

public class ReturnCommand extends Command {
    @Override
    public Object execute(Stack<MethodCallEntry> stack, String[] args, Throwable t) {
        MethodCallEntry calledMethod = stack.peek();
        Class returnTypeClass = calledMethod.getResultSig();

        return getParameterParser(returnTypeClass.getSimpleName()).parse(args[0]);
    }
}

