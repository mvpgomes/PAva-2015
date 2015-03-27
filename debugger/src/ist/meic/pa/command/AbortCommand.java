package ist.meic.pa.command;

import ist.meic.pa.MethodCallEntry;
import ist.meic.pa.Tuple;

import java.util.Stack;

public class AbortCommand implements Command {
    @Override
    public Tuple<Boolean, Object> execute(Stack<MethodCallEntry> stack, String[] args, Throwable t) throws Throwable {
        System.exit(0);
        return new Tuple<>(Boolean.TRUE, null);
    }
}
