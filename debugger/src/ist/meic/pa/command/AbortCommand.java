package ist.meic.pa.command;

import ist.meic.pa.MethodCallEntry;

import java.util.Stack;

public class AbortCommand implements Command {
    @Override
    public Object execute(Stack<MethodCallEntry> stack, Object calledObject, String[] args) throws Throwable {
        System.exit(0);
        return calledObject;
    }
}
