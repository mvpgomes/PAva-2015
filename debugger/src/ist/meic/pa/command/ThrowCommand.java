package ist.meic.pa.command;

import ist.meic.pa.MethodCallEntry;

import java.util.Stack;

public class ThrowCommand extends Command {
    @Override
    public Object execute(Stack<MethodCallEntry> stack, String[] args, Throwable t) throws Throwable {
        throw t.getCause();
    }
}
