package ist.meic.pa.command;

import ist.meic.pa.MethodCallEntry;
import ist.meic.pa.Tuple;

import java.util.Stack;

/**
 * This command rethrows the exception that generated the repl.
 */
public class ThrowCommand extends Command {
    /**
     * Note that this implementation removes the last element from the stack.
     */
    @Override
    public Tuple<Boolean, Object> execute(Stack<MethodCallEntry> stack, String[] args, Throwable t) throws Throwable {
        stack.pop();
        throw t.getCause();
    }
}
