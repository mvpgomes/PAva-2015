package ist.meic.pa.command;

import ist.meic.pa.MethodCallEntry;
import ist.meic.pa.Tuple;

import java.util.Stack;

/**
 * The abort command aborts the execution of the program that is being executed.
 * The command does not expect any arguments.
 */
public class AbortCommand implements Command {
    @Override
    public Tuple<Boolean, Object> execute(Stack<MethodCallEntry> stack, String[] args, Throwable t) {
        System.exit(0);
        return new Tuple<>(Boolean.TRUE, null);
    }
}
